module Main where


import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import           Control.Concurrent.STM (atomically, STM(..))
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TQueue (TQueue(..))
import qualified Control.Concurrent.STM.TQueue as TQueue
import           Control.Concurrent.STM.TVar (TVar(..))
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad (forever, void)

import qualified DBus.Notify as N
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Clock


import qualified System.Console.ANSI as Console
import           System.Exit (exitSuccess)
import           System.IO (BufferMode(..), hSetBuffering, hSetEcho, stdin, stdout)


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  client <- N.connectSession

  Console.clearScreen
  Console.hideCursor

  state <- initialState
  tiId  <- tickLoop state
  thId  <- messageLoop client state
  inputLoop state

  killThread tiId
  killThread thId
  putStrLn "\nBYE"
  Console.showCursor


----------------------------------------------------------------------
-- der Zustand des Programms


data State = State
  { messages :: TQueue Messages
  , curModel :: TVar Model
  }


initialState :: IO State
initialState = atomically $ do
  model <- TVar.newTVar initialModel
  msgs  <- TQueue.newTQueue
  return $ State msgs model


data Model
  = Setting
  { setSeconds :: Int }
  | Running
  { countdownTo :: UTCTime }
  | Alarm
  deriving Show


initialModel :: Model
initialModel =
  Setting 0


----------------------------------------------------------------------
-- Kommandos, die den Zustand verändern

data Commands
  = Message Messages
  | Quit


-- | eine Message veranlasst eine Zustandsänderung im Model
data Messages
  = Add10Seconds
  | StartStop UTCTime
  | Tick UTCTime


data SideEffect
  = NotifyAlarm
  | None


-- | alle Zustandsänderungen am Model geschehen in dieser Funktion!
update :: Messages -> Model -> (Model, SideEffect)
update Add10Seconds model@(Setting secs) =
  (model { setSeconds = secs + 10 }, None)
update (StartStop now) (Setting secs) =
  (Running $ Clock.addUTCTime (fromIntegral secs) now, None)
update (Tick now) model@(Running till)
  | now >= till = (Alarm, NotifyAlarm)
  | otherwise   = (model, None)
update (StartStop now) (Running _) =
  (Setting 0, None)
update Add10Seconds Alarm =
  (Setting 10, None)
update _ model =
  (model, None)


execute :: N.Client -> SideEffect -> IO ()
execute _ None = pure ()
execute client NotifyAlarm =
  void $ showNotification client "A L A R M!!!" "countdown passed"


-- | die Input-Loop überwacht die Tastatureingaben und gibt Kommandos in die Message-Queue weiter
--   wenn diese Loop beendet wird sollte auch das Programm heruntergefahren werden
inputLoop :: State -> IO ()
inputLoop state = loop
  where
    loop = do
      c <- getChar
      now <- Clock.getCurrentTime
      case commandFrom now c of
        Nothing   -> loop
        Just Quit -> return ()
        Just (Message msg) -> do
          atomically $ flip TQueue.writeTQueue msg $ messages state
          loop
    commandFrom _   'q' = Just Quit
    commandFrom _   '+' = Just $ Message Add10Seconds
    commandFrom now 's' = Just $ Message (StartStop now)
    commandFrom _   _   = Nothing


-- | erzeugt einen neuen Thread der die Messagequeue überwacht
-- und das Modell auf den Bildschirm bringt
messageLoop :: N.Client -> State -> IO ThreadId
messageLoop client state = do
  model <- atomically $ TVar.readTVar $ curModel state
  view model
  forkIO $ forever $ do
    (model, seff) <- atomically $ updateState state
    view model
    execute client seff

-- | erzeugt einen neuen Thread der regelmäßig eine Tick-Message in die Queue stellt
tickLoop :: State -> IO ThreadId
tickLoop state = forkIO $ forever $ do
  threadDelay 1000000
  now <- Clock.getCurrentTime
  atomically $ flip TQueue.writeTQueue (Tick now) $ messages state


updateState :: State -> STM (Model, SideEffect)
updateState state = do
  msg <- TQueue.readTQueue $ messages state
  model <- TVar.readTVar $ curModel state
  let (model', seff) = update msg model
  flip TVar.writeTVar model' $ curModel state
  return (model', seff)

----------------------------------------------------------------------
-- View

view :: Model -> IO ()
view model = do
  Console.setCursorPosition 0 0
  Console.clearLine
  viewModel model

  Console.setCursorPosition 4 0
  Console.clearLine
  viewPrompt model
  

viewModel :: Model -> IO ()
viewModel (Setting secs) =
  viewSetting secs
viewModel (Running till) =
  viewCountdown till
viewModel Alarm =
  viewAlarm


viewAlarm :: IO ()
viewAlarm = do
  putStr "A L A R M !!!"


showNotification :: N.Client -> String -> String -> IO N.Notification
showNotification client sum bdy = do
  N.notify client startNote
  where
    appNote = N.blankNote { N.appName = "Haskell Countdown" }
    startNote = appNote { N.summary = sum
                        , N.body= (Just $ N.Text bdy) }


viewCountdown :: UTCTime -> IO ()
viewCountdown till = do
  now <- Clock.getCurrentTime
  putStr $ "Countdown: " ++ formatTime (round . max 0 $ Clock.diffUTCTime till now)


viewSetting :: Int -> IO ()
viewSetting secs = do
  putStr $ "Set: " ++ formatTime secs


viewPrompt :: Model -> IO ()
viewPrompt (Setting _) =
  putStr "q = Quit | + = +10secs | s = start"
viewPrompt (Running _) =
  putStr "q = Quit | s = stop"
viewPrompt Alarm =
  putStr "q = Quit | + = 10secs"


----------------------------------------------------------------------
-- Hilfsfunktionen

-- | Formatiert eine Sekunden-Zahl in der Form hh:mm:ss
formatTime :: (Show a, Integral a) => a -> String
formatTime totalSecs =
  let (hours, diffH) = totalSecs `divMod` (60*60)
      (minutes, seconds) = diffH `divMod` 60
 in show2 hours ++ ":" ++ show2 minutes ++ ":" ++ show2 seconds
  where
    show2 x =
      case show x of
        ""                -> "00"
        s | length s == 1 -> '0' : s
          | otherwise     -> s

