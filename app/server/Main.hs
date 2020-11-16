{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import Major (computeWinner, computeStatistics, exportStatistics)
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import qualified Data.Map.Strict as M
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text.Lazy (Text)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Control.Monad (when)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) $ fail "missing arguments"
  let (portStr, candidates) = (head args, tail args)
  let port = fromMaybe (-1) $ readMaybe portStr
  when (port < 1 || port > 65535) $ fail "invalid port"
  votesRef <- newIORef $ M.fromList $ map (\c -> (c, [])) candidates
  scotty port $ do
    -- gives to the client the candidates list
    get "/candidates" $ do
      candidates <- liftIO $ M.keys <$> readIORef votesRef
      raw . BS.intercalate ";" $ map BS.pack candidates
    -- records a new vote, with a rating for each candidate
    post "/vote" $ do
      newVotes <- read . BS.unpack <$> body :: ActionT Text IO [(Name, Rating)]
      liftIO . modifyIORef votesRef $ flip (foldl (\v (n, r) -> M.adjust (r :) n v)) newVotes
    -- computes the results if it has not been done and gives the winner's name to the client
    get "/result" $ do
      votes <- liftIO $ readIORef votesRef
      if (sum . map length $ M.elems votes) == 0 then raw BS.empty
      else
        let winner = computeWinner votes in
        raw $ BS.pack winner
    post "/generate" . liftIO $ do
      votes <- readIORef votesRef
      exportStatistics (computeStatistics <$> votes) "results.png"

