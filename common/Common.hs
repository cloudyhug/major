module Common where

type Name = String

data Rating = Excellent | VeryGood | Good | Ok | Meh | Poor | Reject
  deriving (Eq, Ord, Show, Read, Enum)

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

