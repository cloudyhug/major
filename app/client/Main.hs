{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import API
import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))
import Brick
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Simple
import Data.Maybe (fromMaybe)
import Data.Array
import qualified Data.ByteString.Lazy.Char8 as BS
import Graphics.Vty.Input (Event(..), Modifier(..), Key(..))
import Network.HTTP.Types.Status (ok200)
import Network.HTTP.Client (RequestBody (RequestBodyLBS))
import Data.Function ((&))
import qualified Data.ByteString.Internal as BSI
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Graphics.Vty.Attributes (defAttr, withStyle, bold)

data ClientState = Command String ClientState
                 | Info String ClientState
                 | Vote { getCandidates :: [String]
                        , getCandidateIndex :: Int
                        , getRatings :: Array Int Rating
                        , hasVoted :: Bool }

data AppState = AppState { getIP :: String
                         , getPort :: Int
                         , getClientState :: ClientState }

type CustomEvent = ()
type RName = ()

data Method = GET | POST deriving Show

-- validate the arguments and start the app
main :: IO ()
main = getArgs >>= \case
  [ip, portStr] -> validateIPAndPort ip portStr >>= startApp ip
  args -> fail $ "expected 2 arguments, got " ++ show (length args)

validateIPAndPort :: String -> String -> IO Int
validateIPAndPort ip portStr =
  let validPort port = if port >= 1 && port <= 65535 then Just port else Nothing in
  case (ip =~ ipRegex, validPort =<< readMaybe portStr) of
    (True, Just port) -> return port
    (False, Just _) -> fail "invalid IP address"
    (True, Nothing) -> fail "invalid port"
    _ -> fail "invalid IP and port"

-- IPv4 matching regex
ipRegex :: String
ipRegex = "(" ++ ipNumberRegex ++ "\\.){3}" ++ ipNumberRegex
  where ipNumberRegex = "([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])"

-- get candidates from the server, setup initial data and start Brick
startApp :: String -> Int -> IO ()
startApp ip port = do
  let req = mkRequest ip port "/candidates" GET Nothing
  candidates <- map BS.unpack . BS.split ';' <$> executeRequest req 
  let nbCandidates = length candidates
      initialRatings = listArray (0, nbCandidates - 1) $ replicate nbCandidates Reject
      initialClientState = Vote { getCandidates = candidates
                                , getCandidateIndex = 0
                                , getRatings = initialRatings
                                , hasVoted = False }
  void . defaultMain app $ AppState ip port initialClientState

app :: App AppState CustomEvent RName
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const majorAttrMap }

majorAttrMap :: AttrMap
majorAttrMap =
  let boldN = attrName "bold"
      boldA = withStyle defAttr bold
  in
  attrMap defAttr [(boldN, boldA)]

drawUI :: AppState -> [Widget RName]
drawUI (AppState _ _ clientState) = [ borderWithLabel (str "Major") $ drawClientUI clientState ]

drawClientUI :: ClientState -> Widget RName
drawClientUI (Info msg clientState) =
  vBox [ drawClientUI clientState
       , str ("[INFO] " ++ msg) ]
                                           
drawClientUI (Command cmd clientState) =
  vBox [ drawClientUI clientState
       , str ("⏵ " ++ cmd) ]

drawClientUI (Vote candidates index ratings voted) =
  withBorderStyle (if voted then unicodeBold else unicode) .
    vBox . snd . foldr renderAccWithIndex (lastIndex, []) . zip candidates $ elems ratings
      where
        lastIndex = length candidates - 1
        paddingValue = maximum $ map length candidates
        render i (candidate, rating) =
          let nbSpaces = paddingValue - length candidate in
          (if i == index then withAttr (attrName "bold") else id) $
            hBox [ str "*"
                 , padLeftRight 3 . str $ candidate ++ replicate nbSpaces ' '
                 , padLeftRight 3 $ str (pp rating) ]
        renderAccWithIndex item (i, widgets) = (i - 1, render i item : widgets)

handleEvent :: AppState -> BrickEvent CustomEvent RName -> EventM RName (Next AppState)
-- CTRL+Q always quits the app
handleEvent appState (VtyEvent (EvKey (KChar 'q') [MCtrl])) = halt appState

-- an information message is erased on any key press
handleEvent appState@(AppState _ _ (Info _ state)) event =
  case event of
    -- '/' directly jumps into command mode
    VtyEvent (EvKey (KChar '/') []) -> continue $ appState { getClientState = Command "/" state }
    VtyEvent (EvKey _ _) -> continue $ appState { getClientState = state }
    _ -> continue appState

handleEvent appState@(AppState _ _ (Command cmd oldState)) event =
  case event of
    -- ESC restores the old state (before typing the command)
    VtyEvent (EvKey KEsc []) -> continue $ appState { getClientState = oldState }
    -- typing any other character appends it to the command
    VtyEvent (EvKey (KChar c) []) ->
      continue $ appState { getClientState = Command (cmd ++ [c]) oldState }
    -- ENTER executes the command
    VtyEvent (EvKey KEnter []) -> execute cmd $ appState { getClientState = oldState }
    -- BackSpace removes the last character
    VtyEvent (EvKey KBS []) ->
      if cmd == "/" then continue appState
      else continue $ appState { getClientState = Command (init cmd) oldState }
    _ -> continue appState

handleEvent appState@(AppState _ _ voteState@(Vote candidates index ratings _)) event =
  case event of
    -- typing '/' starts the command mode
    VtyEvent (EvKey (KChar '/') []) ->
      continue $ appState { getClientState = Command "/" voteState }
    -- horizontal arrows change the selected candidate's rating
    VtyEvent (EvKey KLeft []) ->
      let candidateRating = ratings ! index
          newRatings = ratings // [(index, decrRating candidateRating)]
      in continue $ appState { getClientState = voteState { getRatings = newRatings } }
    VtyEvent (EvKey KRight []) ->
      let candidateRating = ratings ! index
          newRatings = ratings // [(index, incrRating candidateRating)]
      in continue $ appState { getClientState = voteState { getRatings = newRatings } }
    -- vertical arrows change the candidate selection
    VtyEvent (EvKey KUp []) ->
      let nbCandidates = length candidates
          newIndex = if index == 0 then nbCandidates - 1 else index - 1
      in continue $ appState { getClientState = voteState { getCandidateIndex = newIndex } }
    VtyEvent (EvKey KDown []) ->
      let nbCandidates = length candidates
          newIndex = if index == nbCandidates - 1 then 0 else index + 1
      in continue $ appState { getClientState = voteState { getCandidateIndex = newIndex } }
    _ -> continue appState

-- execute a user command
execute :: String -> AppState -> EventM RName (Next AppState)
-- quit the app, same effect as CTRL+Q
execute "/quit" appState = halt appState

execute cmd (AppState ip port (Command _ oldState)) = execute cmd $ AppState ip port oldState
execute cmd (AppState ip port (Info _ state)) = execute cmd $ AppState ip port state

-- cannot vote twice
execute "/vote" appState@(AppState _ _ (Vote { hasVoted = True })) = continue appState
execute "/vote" appState@(AppState ip port voteState@(Vote candidates _ ratings _)) = do
  liftIO . void . executeRequest $ mkRequest ip port "/vote" POST (Just body)
  continue $ appState { getClientState = voteState { hasVoted = True } }
    where body = show $ zip candidates (elems ratings)

-- ask for the result (be it partial or final)
execute "/result" appState@(AppState ip port voteState) = do
  winner <- liftIO . fmap BS.unpack . executeRequest $ mkRequest ip port "/result" GET Nothing
  let msg = if winner == "" then "No vote cast yet." else "The current winner is " ++ winner ++ "."
  continue $ appState { getClientState = Info msg voteState }

-- generate the results PNG file
execute "/generate" appState@(AppState ip port _) = do
  liftIO . void . executeRequest $ mkRequest ip port "/generate" POST Nothing
  continue appState

execute _ appState@(AppState _ _ state) =
  continue $ appState { getClientState = Info "Invalid command." state }

-- wrapper to make a request value from user data
mkRequest :: String -> Int -> String -> Method -> Maybe String -> Request
mkRequest ip port path method maybeBody =
  defaultRequest
  & setRequestMethod (BSI.packChars $ show method)
  & setRequestHost (BSI.packChars ip)
  & setRequestPort port
  & setRequestPath (BSI.packChars path)
  & setRequestBody (RequestBodyLBS . BS.pack $ fromMaybe "" maybeBody)

-- send a request to the server and gives back the body of the response
executeRequest :: Request -> IO BS.ByteString
executeRequest req = httpLBS req >>= \resp ->
  if getResponseStatus resp /= ok200 then fail "invalid response"
  else return $ getResponseBody resp

