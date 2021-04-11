{-# LANGUAGE OverloadedStrings, TupleSections, NumericUnderscores #-}

module Main where

import API
import Major
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import qualified Data.Map.Strict as M
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text.Lazy (Text)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Control.Monad (when, unless, void, forM_)
import Matrix
import Network.HTTP.Types.Status
import qualified Data.Set as S
import qualified Data.Aeson as J
import Data.Maybe
import System.Random
import Control.Concurrent
import Data.Hashable

type Login = String
type Password = String

hashPassword :: String -> Int
hashPassword = hash

isValidConfig :: [CandidateInfo] -> Bool
isValidConfig _ = True -- todo

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 3) $ fail "Wrong number of arguments"
  let [portStr, inputFile, adminPassword] = args
  let adminPwHash = hash adminPassword
  let port = fromMaybe (-1) $ readMaybe portStr
  when (port < 1024 || port > 65535) $ fail "Invalid port"
  maybeVoteConfig <- J.decodeFileStrict' inputFile
  when (isNothing maybeVoteConfig) $ fail "Config not found"

  let candidatesInfo = fromJust maybeVoteConfig :: [CandidateInfo]
  unless (isValidConfig candidatesInfo) $ fail "Invalid config"

  userAuthRef <- newIORef M.empty :: IO (IORef (M.Map Login Int))
  userVotesRef <- newIORef S.empty :: IO (IORef (S.Set Login))

  electionPhaseRef <- newIORef Register
  numberVotesCastRef <- newIORef 0
  -- cell (i, k) is the number of ratings k for candidate i
  voteData <- newMatrix (length candidatesInfo) 7 0 :: IO (Matrix Int)

  scotty port $ do

    get "/state" $ do
      (json =<<) $ liftIO $ do
        phase <- readIORef electionPhaseRef
        if phase /= Results then
          return $ ServerState phase Nothing
        else do
          numberVotesCast <- fromIntegral <$> readIORef numberVotesCastRef
          numberUsersRegistered <- fromIntegral . M.size <$> readIORef userAuthRef
          let participation = numberVotesCast / numberUsersRegistered
          ServerState phase . Just . (participation,) <$> computeResults voteData

    post "/register/:login/:password" $ do
      phase <- liftIO $ readIORef electionPhaseRef
      if phase /= Register then
        status $ mkStatus 400 "Server not in Register state"
      else do
        login <- param "login"
        userAuth <- liftIO $ readIORef userAuthRef
        case M.lookup login userAuth of
          Just _ -> status $ mkStatus 409 "User already registered"
          Nothing -> do
            pwHash <- hashPassword <$> param "password"
            liftIO $ modifyIORef' userAuthRef (M.insert login pwHash)
    
    post "/vote/:login/:password" $ do
      phase <- liftIO $ readIORef electionPhaseRef
      if phase /= Voting then
        status $ mkStatus 400 "Server not in Voting state"
      else do
        login <- param "login"
        alreadyVoted <- liftIO $ S.member login <$> readIORef userVotesRef
        if alreadyVoted then
          status $ mkStatus 409 "User already voted"
        else do
          pwHash <- hashPassword <$> param "password"
          userAuth <- liftIO $ readIORef userAuthRef
          case M.lookup login userAuth of
            Just pwHashSaved | pwHash == pwHashSaved -> (json =<<) $ do
              voteShards <- shards <$> jsonData
              liftIO $ do
                forM_ voteShards $ \(VoteShard candidateId grade) -> do
                  updateMatrix voteData candidateId (fromEnum grade + 1) (+1)
                modifyIORef' numberVotesCastRef (+1)
            _ -> status $ mkStatus 409 "Wrong credentials"
    
    put "/admin/forward/:password" $ do
      pwHash <- hashPassword <$> param "password"
      if pwHash == adminPwHash then liftIO $ do
        phase <- readIORef electionPhaseRef
        case phase of
          Register -> writeIORef electionPhaseRef Voting
          Voting -> writeIORef electionPhaseRef Results
          Results -> return ()
      else
        status $ mkStatus 409 "Wrong admin password"