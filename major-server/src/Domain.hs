{-# LANGUAGE OverloadedStrings #-}
module Domain where

import qualified Data.Vector as V

import Data.Aeson
import Control.Applicative (liftA2)
import Data.Either (fromRight)
import Data.Scientific (floatingOrInteger)
import Control.Monad.Trans.Except (ExceptT)

newtype RefreshToken = RefreshToken String
  deriving (Eq, Show)

newtype AccessToken = AccessToken String
  deriving (Eq, Ord, Show)

instance ToJSON AccessToken where
  toJSON (AccessToken atok) = object ["accessToken" .= atok]

rawAccessToken :: AccessToken -> String
rawAccessToken (AccessToken atok) = atok

type ClientID = String
type ElectionID = Int
type CandidateID = Int

data Credentials = Credentials String String ClientID
  deriving Show

instance FromJSON Credentials where
  parseJSON = withObject "Credentials" $ \v ->
    Credentials <$> v .: "login" <*> v .: "password" <*> v .: "clientID"

data ConnectionInfo = ConnectionInfo RefreshToken AccessToken
  deriving Show

instance ToJSON ConnectionInfo where
  toJSON (ConnectionInfo (RefreshToken rtok) (AccessToken atok)) =
    object ["refreshToken" .= rtok, "accessToken" .= atok]

data ElectionInfo = ElectionInfo ElectionID String Bool
  deriving Show

instance ToJSON ElectionInfo where
  toJSON (ElectionInfo eID name isRunning) =
    object ["id" .= eID, "name" .= name, "isRunning" .= isRunning]

data Party = Party String String
  deriving Show

instance ToJSON Party where
  toJSON (Party name colour) = object ["name" .= name, "colour" .= colour]

instance FromJSON Party where
  parseJSON = withObject "Party" $ \v -> Party <$> v .: "name" <*> v .: "colour"

data Candidate = Candidate CandidateID String Party
  deriving Show

instance ToJSON Candidate where
  toJSON (Candidate cID name party) = object ["id" .= cID, "name" .= name, "party" .= party]

instance FromJSON Candidate where
  parseJSON = withObject "Candidate" $ \v ->
    Candidate <$> v .: "id" <*> v .: "name" <*> v .: "party"

newtype Election = Election [Candidate]
  deriving Show

instance ToJSON Election where
  toJSON (Election candidates) = Array . V.fromList $ map toJSON candidates

data ElectionConfig = ElectionConfig String [Candidate]
  deriving Show

instance FromJSON ElectionConfig where
  parseJSON = withObject "ElectionConfig" $ \v ->
    ElectionConfig <$> v .: "name" <*> v .: "candidates"

data Rating = Terrible | Bad | Inadequate | Passable | Sufficient | Good | Excellent
  deriving (Read, Show, Eq, Ord, Enum)

instance ToJSON Rating where
  toJSON = toJSON . fromEnum

instance FromJSON Rating where
  parseJSON = withScientific "Rating" $
    pure . toEnum . fromRight (error "a rating cannot be a float") . floatingOrInteger

newtype ElectionResults = ElectionResults [(String, Rating, Double)]
  deriving Show

instance ToJSON ElectionResults where
  toJSON (ElectionResults results) = Array . V.fromList $ map f results
    where f (name, rating, score) = object ["name" .= name, "rating" .= rating, "score" .= score]

newtype Ballot = Ballot [(CandidateID, Rating)]
  deriving Show

instance FromJSON Ballot where
  parseJSON = withArray "Ballot" $ fmap (Ballot . V.toList) . mapM parseRating
    where parseRating = withObject "Ballot rating" $ \v -> liftA2 (,) (v .: "id") (v .: "rating")

instance ToJSON Ballot where
  toJSON (Ballot ratings) = Array . V.fromList $ map f ratings
    where f (cID, rating) = object ["id" .= cID, "rating" .= rating]

data APIError =
    UnacceptableCredentials
  | InvalidSession
  | InvalidCredentials
  | UserAlreadyRegistered
  | ClientAlreadyRegistered
  | InvalidRefreshToken
  | InvalidAccessToken
  | ElectionNotFound
  | ElectionStillRunning
  | ElectionNotRunning
  | NoVoteCast
  | UserDidNotVote
  | UserAlreadyVoted
  | UserIsNotAdmin
  | InvalidRequestInput
  deriving Show

type AppM a = ExceptT APIError IO a

class Database db where
  atomicallyWith :: db -> (db -> AppM a) -> AppM a
  isValidSession :: db -> String -> ClientID -> AppM Bool
  isValidCredentials :: db -> String -> String -> AppM Bool
  getTokens :: db -> String -> ClientID -> AppM ConnectionInfo
  existsUser :: db -> String -> AppM Bool
  existsClient :: db -> ClientID -> AppM Bool
  addUser :: db -> String -> String -> ClientID -> AppM ()
  getNewAccessToken :: db -> RefreshToken -> AppM (Maybe AccessToken)
  isValidAccessToken :: db -> AccessToken -> AppM Bool
  isAdminToken :: db -> AccessToken -> AppM Bool
  existsElection :: db -> ElectionID -> AppM Bool
  findAllElections :: db -> AppM [ElectionInfo]
  findElectionDataByID :: db -> ElectionID -> AppM Election
  isFinishedElection :: db -> ElectionID -> AppM Bool
  setFinishedElection :: db -> ElectionID -> AppM ()
  findElectionStatsByID :: db -> ElectionID -> AppM [(Candidate, [(Rating, Int)])]
  findCandidateByID :: db -> ElectionID -> CandidateID -> AppM Candidate
  addElectionResults :: db -> ElectionID -> ElectionResults -> AppM ()
  findElectionResultsByID :: db -> ElectionID -> AppM (Maybe ElectionResults)
  findUserByToken :: db -> AccessToken -> AppM String
  findVoteByUser :: db -> ElectionID -> String -> AppM (Maybe Ballot)
  addVote :: db -> ElectionID -> String -> Ballot -> AppM ()
