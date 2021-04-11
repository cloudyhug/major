{-# LANGUAGE DeriveGeneric #-}

module API where

import GHC.Generics
import Data.Aeson.Types

data Rating = Excellent | Good | Satisfactory | Sufficient | Disappointing | Bad | Terrible
  deriving (Eq, Enum, Generic)

instance ToJSON Rating
instance FromJSON Rating

-- /state response body

data ElectionPhase =
    Register
  | Voting
  | Results
  deriving (Eq, Generic)

instance ToJSON ElectionPhase
instance FromJSON ElectionPhase

data CandidateScore = CandidateScore {
  scoreId :: Int,
  medianGrade :: Rating,
  percentScore :: Double
} deriving Generic

instance ToJSON CandidateScore
instance FromJSON CandidateScore

data ServerState = ServerState {
  phase :: ElectionPhase,
  results :: Maybe (Double, [CandidateScore])
} deriving Generic

instance ToJSON ServerState
instance FromJSON ServerState

-- /votelogin response body

data CandidateInfo = CandidateInfo {
  id :: Int,
  name :: String,
  party :: String,
  colour :: String
} deriving Generic

instance ToJSON CandidateInfo
instance FromJSON CandidateInfo

data VoteInfo = VoteInfo {
  token :: Int,
  candidatesInfo :: [CandidateInfo]
} deriving Generic

instance ToJSON VoteInfo
instance FromJSON VoteInfo

-- /vote request body

data VoteShard = VoteShard {
  candidateId :: Int,
  grade :: Rating
} deriving Generic

instance ToJSON VoteShard
instance FromJSON VoteShard

data Ballot = Ballot {
  shards :: [VoteShard]
} deriving Generic

instance ToJSON Ballot
instance FromJSON Ballot