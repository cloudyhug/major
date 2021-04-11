{-# LANGUAGE DeriveGeneric #-}

module API where

import GHC.Generics
import Data.Aeson.Types

data Rating = Excellent | Good | Satisfactory | Sufficient | Disappointing | Bad | Terrible
  deriving (Eq, Enum, Generic)

instance ToJSON Rating
instance FromJSON Rating

instance Ord Rating where
  compare grade1 grade2 =
    -- higher enum rank => lower grade
    case (fromEnum grade1 - fromEnum grade2) of
      0             -> EQ
      n | n > 0     -> LT
        | otherwise -> GT

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
} deriving (Eq, Generic)

instance ToJSON CandidateScore
instance FromJSON CandidateScore

instance Ord CandidateScore where
  compare (CandidateScore _ median1 score1) (CandidateScore _ median2 score2) =
    case compare median1 median2 of
      EQ -> compare score1 score2
      cmp -> cmp

data ServerState = ServerState {
  phase :: ElectionPhase,
  info :: Maybe ElectionInfo
} deriving Generic

instance ToJSON ServerState
instance FromJSON ServerState

data ElectionInfo =
    ElectionResults {
      participation :: Double,
      scores :: [CandidateScore]
    }
  | VoteInfo {
      candidatesInfo :: [CandidateInfo]
    } deriving Generic

instance ToJSON ElectionInfo
instance FromJSON ElectionInfo

data CandidateInfo = CandidateInfo {
  id :: Int,
  name :: String,
  party :: String,
  colour :: String
} deriving Generic

instance ToJSON CandidateInfo
instance FromJSON CandidateInfo

-- /vote request body

data VoteShard = VoteShard {
  candidateId :: Int,
  grade :: Rating
} deriving Generic

instance ToJSON VoteShard
instance FromJSON VoteShard

data Ballot = Ballot {
  authentication :: Authentication,
  shards :: [VoteShard]
} deriving Generic

instance ToJSON Ballot
instance FromJSON Ballot

data Authentication = Authentication {
  login :: String,
  password :: String
} deriving Generic

instance ToJSON Authentication
instance FromJSON Authentication