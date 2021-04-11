{-# LANGUAGE OverloadedStrings #-}

module Main where

import API (Ballot(shards), CandidateInfo, ElectionPhase(Results, Register, Voting),
            ServerState(ServerState), VoteShard(VoteShard), ElectionResults(ElectionResults))
import Major (computeResults)
import Matrix (newMatrix, updateMatrix, Matrix)
import Web.Scotty (get, json, jsonData, text, param, post, put, scotty, status)
import Web.Scotty.Internal.Types (ActionT)
import Network.HTTP.Types.Status (mkStatus)
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
      json <=< liftIO $ do
        phase <- readIORef electionPhaseRef
        if phase /= Results then
          return $ ServerState phase Nothing
        else do
          numberVotesCast <- readIORef numberVotesCastRef
          numberUsersRegistered <- M.size <$> readIORef userAuthRef
          let participationRate = fromIntegral numberVotesCast / fromIntegral numberUsersRegistered
          computeResults voteData numberVotesCast
            <&> ServerState phase . Just . ElectionResults (participationRate * 100.0)

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
            status $ mkStatus 200 "Registered successfully"
    
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
                modifyIORef' userVotesRef (S.insert login)
              status $ mkStatus 200 "Voted successfully"
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
    
    get "/admin/users/:password" $ do
      pwHash <- hashPassword <$> param "password"
      if pwHash == adminPwHash then text <=< liftIO $ do
        registeredUsers <- M.keysSet <$> readIORef userAuthRef
        usersWhoVoted <- readIORef userVotesRef
        return $ T.concat
          [ "registered: ", T.pack $ show (S.difference registeredUsers usersWhoVoted),
            "\nvoted: ", T.pack $ show usersWhoVoted ]
      else
        status $ mkStatus 409 "Wrong admin password"