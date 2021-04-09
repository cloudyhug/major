{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import Common
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
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Matrix

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) $ fail "missing arguments"
  let (portStr, candidateNames) = (head args, tail args)
  let port = fromMaybe (-1) $ readMaybe portStr
  when (port < 1 || port > 65535) $ fail "invalid port"

  electionPhase <- newIORef Register
  numberVotesCast <- newIORef 0
  -- cell (i, k) is the number of ratings k for candidate i
  voteData <- newMatrix (length candidateNames) 7 0 :: IO (Matrix Int)

  scotty port $ do
    get "/state" $ do
      (json =<<) $ liftIO $ do
        phase <- readIORef electionPhase
        if phase /= Results then
          return $ ServerState phase Nothing
        else
          ServerState phase . Just <$> computeResults candidateNames voteData


  -- votesRef <- newIORef $ M.fromList $ map (, []) candidates
  -- scotty port $ do
  --   -- gives to the client the candidates list
  --   get "/candidates" $ do
  --     candidates <- liftIO $ M.keys <$> readIORef votesRef
  --     raw . BS.intercalate ";" $ map BS.pack candidates
  --   -- records a new vote, with a rating for each candidate
  --   post "/vote" $ do
  --     newVotes <- read . BS.unpack <$> body :: ActionT Text IO [(Name, Rating)]
  --     liftIO . modifyIORef votesRef $ flip (foldl (\v (n, r) -> M.adjust (r :) n v)) newVotes
  --   -- computes the results if it has not been done and gives the winner's name to the client
  --   get "/result" $ do
  --     votes <- liftIO $ readIORef votesRef
  --     if (sum . map length $ M.elems votes) == 0 then raw BS.empty
  --     else
  --       let winner = computeWinner votes in
  --       raw $ BS.pack winner
  --   post "/generate" . liftIO $ do
  --     votes <- readIORef votesRef
  --     exportStatistics (computeStatistics <$> votes) "results.png"
