{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module MemoryDatabase where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Maybe (fromJust, isJust)
import Control.Concurrent (MVar, newMVar, takeMVar, putMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Hash.MD5 (Str(Str), md5s)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, secondsToNominalDiffTime)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Tuple.Extra (fst3, snd3, thd3, second3)
import Data.Functor ((<&>))
import System.Random (randomIO)
import Data.Fixed (Fixed, E12)
import Control.Monad (forM)
import Data.Foldable (foldl', find)
import Control.Monad.Trans.Except (ExceptT, throwE, catchE)
import System.Random.Shuffle (shuffleM)
import Data.Aeson (decodeFileStrict)

import Domain

data MemoryDatabase = MemoryDatabase {
  mutex :: MVar (),
  admins :: S.Set AccessToken,
  elections :: IORef (M.Map ElectionID (String, Bool, Election)),
  tokens :: IORef (M.Map (String, ClientID) (RefreshToken, AccessToken, UTCTime)),
  passwords :: IORef (M.Map String String),
  votes :: IORef (M.Map ElectionID (M.Map CandidateID (M.Map Rating Int))),
  userVotes :: IORef (M.Map ElectionID (M.Map String Ballot)),
  results :: IORef (M.Map ElectionID ElectionResults)
}

memoryDatabaseFromFile :: FilePath -> AccessToken -> IO MemoryDatabase
memoryDatabaseFromFile file adminToken = do
  mutex <- newMVar ()
  let admins = S.singleton adminToken
  ElectionConfig name candidates <- fromJust <$> decodeFileStrict file
  elections <- newIORef $ M.singleton 0 (name, True, Election candidates)
  tokens <- newIORef M.empty
  passwords <- newIORef M.empty
  let emptyVoteMap = M.fromList $ zip [Terrible .. Excellent] (repeat 0)
  votes <- newIORef . M.singleton 0 . M.fromList $
    map (\(Candidate cID _ _) -> (cID, emptyVoteMap)) candidates
  userVotes <- newIORef $ M.singleton 0 M.empty
  results <- newIORef M.empty
  return $ MemoryDatabase mutex admins elections tokens passwords votes userVotes results

instance Database MemoryDatabase where
  atomicallyWith db k = do
    liftIO $ takeMVar (mutex db)
    k db `finallyE` liftIO (putMVar (mutex db) ())

  isValidSession db login clientID = liftIO $
    isJust . M.lookup (login, clientID) <$> readIORef (tokens db)

  isValidCredentials db login password = liftIO $
    readIORef (passwords db) <&> M.lookup login <&> \case
      Nothing -> False
      Just h -> h == md5s (Str password)

  getTokens db login clientID = do
    refreshToken <- liftIO $ fst3 . fromJust . M.lookup (login, clientID) <$> readIORef (tokens db)
    accessToken <- fromJust <$> getNewAccessToken db refreshToken
    return $ ConnectionInfo refreshToken accessToken

  existsUser db login = liftIO $ isJust . M.lookup login <$> readIORef (passwords db)

  existsClient db clientID = liftIO $ elem clientID . map snd . M.keys <$> readIORef (tokens db)

  addUser db login password clientID = liftIO $ do
    (rtok, _) <- createToken $ login ++ password ++ clientID
    (atok, time) <- createToken rtok
    let expirationTime = addUTCTime (secondsToNominalDiffTime $ tokenValidityTime * 60) time
    modifyIORef' (tokens db) $
      M.insert (login, clientID) (RefreshToken rtok, AccessToken atok, expirationTime)
    modifyIORef' (passwords db) $
      M.insert login $ md5s (Str password)

  getNewAccessToken db refreshToken@(RefreshToken rtok) = liftIO $ do
    tokenMap <- readIORef (tokens db)
    case filter ((refreshToken ==) . fst3 . snd) $ M.assocs tokenMap of
      [(k, _)] -> do
        (atok, time) <- createToken rtok
        let expirationTime = addUTCTime (secondsToNominalDiffTime $ tokenValidityTime * 60) time
        writeIORef (tokens db) $
          M.update (\(r, _, _) -> Just (r, AccessToken atok, expirationTime)) k tokenMap
        return . Just $ AccessToken atok
      _ -> return Nothing

  isValidAccessToken db accessToken = liftIO $ do
    tokenMap <- readIORef (tokens db)
    time <- getCurrentTime
    return $ case filter ((accessToken ==) . snd3 . snd) $ M.assocs tokenMap of
      [(_, (_, _, expirationTime))] -> time < expirationTime
      _ -> False
  
  isAdminToken db accessToken = pure $ S.member accessToken (admins db)

  existsElection db electionID = liftIO $ isJust . M.lookup electionID <$> readIORef (elections db)

  findAllElections db = liftIO $
    map (\(id, (name, isRunning, _)) -> ElectionInfo id name isRunning) . M.assocs <$>
      readIORef (elections db)

  findElectionDataByID db electionID = liftIO $ do
    Election candidates <- thd3 . fromJust . M.lookup electionID <$> readIORef (elections db)
    Election <$> shuffleM candidates

  isFinishedElection db electionID = liftIO $
    snd3 . fromJust . M.lookup electionID <$> readIORef (elections db)

  setFinishedElection db electionID = liftIO $
    modifyIORef' (elections db) $ M.update (Just . second3 (const True)) electionID

  findElectionStatsByID db electionID = do
    votes <- liftIO . readIORef $ votes db
    let electionVotes = M.assocs . fromJust . M.lookup electionID $ votes
    forM electionVotes $ \(cID, ratings) ->
      findCandidateByID db electionID cID <&> (, M.assocs ratings)
    
  findCandidateByID db electionID candidateID = liftIO $ do
    Election candidates <- thd3 . fromJust . M.lookup electionID <$> readIORef (elections db)
    return . fromJust $ find hasRightID candidates
      where hasRightID (Candidate cID _ _) = cID == candidateID

  addElectionResults db electionID electionResults = liftIO $
    modifyIORef' (results db) $ M.insert electionID electionResults

  findElectionResultsByID db electionID = liftIO $ M.lookup electionID <$> readIORef (results db)
  
  findUserByToken db accessToken = liftIO $
    fst . fst . head . filter ((== accessToken) . snd3 . snd) . M.assocs <$> readIORef (tokens db)
  
  findVoteByUser db electionID login = liftIO $
    M.lookup login . fromJust . M.lookup electionID <$> readIORef (userVotes db)

  addVote db electionID login ballot@(Ballot ratings) = liftIO $ do
    modifyIORef' (userVotes db) $ M.update (Just . M.insert login ballot) electionID
    modifyIORef' (votes db) $ M.update (Just . flip (foldl' $ flip addRating) ratings) electionID
      where
        addRating (candidateID, rating) =
          M.update (Just . M.update (Just . (+1)) rating) candidateID

createToken :: String -> IO (String, UTCTime)
createToken baseStr = do
  time <- getCurrentTime
  n :: Int <- randomIO
  return (md5s . Str $ baseStr ++ show time ++ show n, time)

-- minutes
tokenValidityTime :: Fixed E12
tokenValidityTime = 10

finallyE :: Monad m => ExceptT e m a -> ExceptT e m () -> ExceptT e m a
finallyE m closer = do
  res <- catchE (fmap Right m) (return . Left)
  closer
  either throwE return res
