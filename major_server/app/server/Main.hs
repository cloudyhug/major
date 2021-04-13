{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import API
import Major (computeResults)
import Matrix (newMatrix, updateMatrix, Matrix)
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.HTTP.Types.Status (status400, status409)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, unless, void, forM_, (<=<))
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef, IORef)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Aeson as J
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Hashable (Hashable(hash))
import Data.Functor((<&>))

type Login = String
type Password = String

hashPassword :: String -> Int
hashPassword = hash

isValidConfig :: [CandidateInfo] -> Bool
isValidConfig _ = True -- todo

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 4) $ fail "Wrong number of arguments"
  let [portStr, inputFile, adminLogin, adminPassword] = args
      adminPwHash = hash adminPassword
      port = fromMaybe (-1) $ readMaybe portStr
  when (port < 1024 || port > 65535) $ fail "Invalid port"
  maybeVoteConfig <- J.decodeFileStrict' inputFile
  when (isNothing maybeVoteConfig) $ fail "Config not found"

  -- app state

  let candidatesInfo = fromJust maybeVoteConfig :: [CandidateInfo]
  unless (isValidConfig candidatesInfo) $ fail "Invalid config"

  userAuthRef <- newIORef M.empty :: IO (IORef (M.Map Login Int))
  userVotesRef <- newIORef S.empty :: IO (IORef (S.Set Login))

  electionPhaseRef <- newIORef Register
  numberVotesCastRef <- newIORef 0
  -- cell (i, k) is the number of ratings k for candidate i
  voteData <- newMatrix (length candidatesInfo) 7 0 :: IO (Matrix Int)

  -- REST API
  scotty port $ do
    middleware simpleCors

    -- input body: empty
    -- output body: ServerState (JSON)
    get "/state" $ do
      json <=< liftIO $ do
        readIORef electionPhaseRef >>= \case
          Register -> return $ ServerState Register Nothing
          Voting -> return . ServerState Voting . Just $ VoteInfo candidatesInfo
          Results -> do
            numberVotesCast <- readIORef numberVotesCastRef
            numberUsersRegisteredFlt <- fromIntegral . M.size <$> readIORef userAuthRef
            if numberVotesCast == 0 then
              return $ ServerState Results Nothing
            else do
              let numberVotesCastFlt = fromIntegral numberVotesCast
                  participationRate = numberVotesCastFlt / numberUsersRegisteredFlt
              computeResults voteData numberVotesCast
                <&> ServerState Results . Just . ElectionResults (participationRate * 100.0)

    -- input body: Authentication (JSON)
    -- output body: empty
    post "/register" $ do
      phase <- liftIO $ readIORef electionPhaseRef
      if phase /= Register then
        status status400 >> addHeader "message" "Election is not in Register phase"
      else do
        Authentication login password <- jsonData
        userAuth <- liftIO $ readIORef userAuthRef
        case M.lookup login userAuth of
          Just _ -> status status409 >> addHeader "message" "User already registered"
          Nothing -> do
            liftIO $ modifyIORef' userAuthRef (M.insert login $ hashPassword password)
            addHeader "message" "Registered successfully"
    
    -- input body: Ballot (JSON)
    -- output body: empty
    post "/vote" $ do
      phase <- liftIO $ readIORef electionPhaseRef
      if phase /= Voting then do
        status status400 >> addHeader "message" "Election is not in Voting phase"
      else do
        ballot <- jsonData
        let (Authentication login password) = authentication ballot
        alreadyVoted <- liftIO $ S.member login <$> readIORef userVotesRef
        if alreadyVoted then
          status status409 >> addHeader "message" "User already voted"
        else do
          userAuth <- liftIO $ readIORef userAuthRef
          case M.lookup login userAuth of
            Just pwHashSaved | hashPassword password == pwHashSaved -> do
              liftIO $ do
                forM_ (shards ballot) $ \(VoteShard candidateId grade) ->
                  updateMatrix voteData candidateId (fromEnum grade + 1) (+1)
                modifyIORef' numberVotesCastRef (+1)
                modifyIORef' userVotesRef (S.insert login)
              addHeader "message" "Voted successfully"
            _ -> status status409 >> addHeader "message" "Wrong credentials"
    
    -- input body: Authentication (JSON)
    -- output body: empty
    put "/forward" $ do
      Authentication login password <- jsonData
      let pwHash = hashPassword password
      if login == adminLogin && pwHash == adminPwHash then liftIO $ do
        readIORef electionPhaseRef >>= \case
          Register -> writeIORef electionPhaseRef Voting
          Voting -> writeIORef electionPhaseRef Results
          Results -> return ()
      else status status409 >> addHeader "message" "Wrong admin credentials"
    
    -- input body: Authentication (JSON)
    -- output body: raw text
    post "/users" $ do
      Authentication login password <- jsonData
      let pwHash = hashPassword password
      if login == adminLogin && pwHash == adminPwHash then text <=< liftIO $ do
        registeredUsers <- M.keysSet <$> readIORef userAuthRef
        usersWhoVoted <- readIORef userVotesRef
        return $ T.concat
          [ "registered: ", T.pack $ show (S.difference registeredUsers usersWhoVoted),
            "\nvoted: ", T.pack $ show usersWhoVoted ]
      else status status409 >> addHeader "message" "Wrong admin credentials"