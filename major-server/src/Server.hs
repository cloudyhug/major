{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Server where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Lazy as T
import qualified Data.Aeson as J

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.HTTP.Types.Status (status400, status401, status403, status404)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)

import Domain
import API

launch :: Database db => db -> Int -> IO ()
launch db port = scotty port $ do
  middleware logStdoutDev

  post "/connect" $ buildResponse json =<< do
    credentials <- fromJust . J.decode <$> body
    liftIO . runExceptT $ connect db credentials
  
  post "/register" $ buildResponse (const $ return ()) =<< do
    credentials <- fromJust . J.decode <$> body
    liftIO . runExceptT $ register db credentials
  
  get "/refreshToken" $ buildResponse text =<< do
    token <- RefreshToken . BS.unpack <$> body
    liftIO . runExceptT $ T.pack . rawAccessToken <$> renewToken db token
  
  get "/elections" $ buildResponse json =<< do
    token <- AccessToken . T.unpack . fromJust <$> header "accessToken"
    liftIO . runExceptT $ getElections db token
  
  get "/electionInfo/:eID" $ buildResponse json =<< do
    token <- AccessToken . T.unpack . fromJust <$> header "accessToken"
    eID <- param "eID"
    liftIO . runExceptT $ getElectionInfo db token eID
  
  get "/electionResults/:eID" $ buildResponse json =<< do
    token <- AccessToken . T.unpack . fromJust <$> header "accessToken"
    eID <- param "eID"
    liftIO . runExceptT $ getElectionResults db token eID
  
  get "/myVote/:eID" $ buildResponse json =<< do
    token <- AccessToken . T.unpack . fromJust <$> header "accessToken"
    eID <- param "eID"
    liftIO . runExceptT $ getVote db token eID
  
  post "/vote/:eID" $ buildResponse (const $ return ()) =<< do
    token <- AccessToken . T.unpack . fromJust <$> header "accessToken"
    eID <- param "eID"
    ballot <- fromJust . J.decode <$> body
    liftIO . runExceptT $ vote db token eID ballot
  
  post "/admin/end/:eID" $ buildResponse (const $ return ()) =<< do
    token <- AccessToken . T.unpack . fromJust <$> header "accessToken"
    eID <- param "eID"
    liftIO . runExceptT $ endElection db token eID
    
buildResponse :: (r -> ActionM ()) -> Either APIError r -> ActionM ()
buildResponse f = \case
  Right result -> f result
  Left err -> do
    status $ errorToStatus err
    addHeader "message" . T.pack $ show err
      where
        errorToStatus = \case
          UnacceptableCredentials -> status401
          InvalidSession          -> status401
          InvalidCredentials      -> status401
          UserAlreadyRegistered   -> status403
          ClientAlreadyRegistered -> status403
          InvalidRefreshToken     -> status401
          InvalidAccessToken      -> status401
          ElectionNotFound        -> status404
          ElectionStillRunning    -> status400
          ElectionNotRunning      -> status400
          NoVoteCast              -> status400
          UserDidNotVote          -> status404
          UserAlreadyVoted        -> status403
          UserIsNotAdmin          -> status403
          InvalidRequestInput     -> status400
