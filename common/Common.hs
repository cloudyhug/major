{-# LANGUAGE DeriveGeneric #-}

module Common where

import GHC.Generics
import Data.Aeson.Types

type Name = String

data Rating = Excellent | VeryGood | Good | Ok | Meh | Poor | Reject
  deriving (Eq, Ord, Show, Read, Enum, Generic)

instance ToJSON Rating
instance FromJSON Rating

incrRating :: Rating -> Rating
incrRating Excellent = Excellent
incrRating r = pred r

decrRating :: Rating -> Rating
decrRating Reject = Reject
decrRating r = succ r

class PrettyPrint a where
  pp :: a -> String

instance PrettyPrint Rating where
  pp Excellent = "Excellent *__*"
  pp VeryGood = "Very Good :D"
  pp Good = "Good :)"
  pp Ok = "Ok :|"
  pp Meh = "Meh :/"
  pp Poor = "Poor :("
  pp Reject = "Reject >__<"

data ElectionPhase =
    Register
  | Voting
  | Results
  deriving (Eq, Generic)

data CandidateScore = CandidateScore {
  candidateName :: String,
  medianGrade :: Rating,
  percentScore :: Double
} deriving Generic

data ServerState = ServerState {
  phase :: ElectionPhase,
  results :: Maybe [CandidateScore]
} deriving Generic

instance ToJSON ElectionPhase
instance FromJSON ElectionPhase
instance ToJSON CandidateScore
instance FromJSON CandidateScore
instance ToJSON ServerState
instance FromJSON ServerState