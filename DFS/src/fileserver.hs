module Main where
import System.IO
import Control.Exception
import Control.Concurrent 
import Control.Monad (when, forever)
import Control.Monad.Fix (fix)
import Network (PortID(..), accept, listenOn, withSocketsDo, Socket)
import Control.Concurrent.STM
import qualified Data.Map as M
import Data.Map (Map)

type Msg = (Int, String)
type Server = TVar (Map Int Room)

type RoomName = String
data Room = Room 
  { roomName :: RoomName
  }


newServer :: IO Server
newServer = newTVarIO M.empty

portNum :: Int
portNum = 3454

main :: IO()
main = do 
  putStrLn "What file should i open?"
  fileName <- getLine
  putStrLn "What should i write to it?"
  toWrite <- getLine
  writeFileandClose fileName toWrite
  --openSocket
  

runConn :: Handle -> Server -> Int -> IO ()
runConn handle server port = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle NoBuffering
  readNxt
  return ()
  where
    readNxt = do 
      hPutStrLn handle "HELLO"
    --deal with messages
    

openSocket :: IO()
openSocket = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral portNum))
  server <- newServer
  putStrLn "Waiting for connections\n"
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted: "++ host ++ "\n"
    forkFinally (runConn handle server portNum) (\_ -> hClose handle)
 
openFileandSend :: Handle -> String -> IO() 
openFileandSend clientHandle filename = do  
  fileHandle <- openFile filename ReadMode
  contents <- hGetContents fileHandle
  hClose fileHandle
  hPutStrLn clientHandle contents

writeFileandClose :: String -> String-> IO() 
writeFileandClose fileName toWrite = do
  fileHandle <- openFile fileName WriteMode
  hPutStr fileHandle toWrite
  hClose fileHandle
  putStrLn "Written to file"
