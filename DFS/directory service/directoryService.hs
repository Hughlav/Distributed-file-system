{-# LANGUAGE LambdaCase,RecordWildCards, OverloadedStrings #-} 

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
import Data.Sequence

type FileList = TVar [String]
type ServerList = TVar (Map String FileServer)

data FileServer = FileServer { serverName :: String
                             , portNo :: Int
                             , files :: FileList
                             }

newServer :: IO ServerList
newServer = newTVarIO M.empty

--newFileL :: IO FileList
--newFileL = new FileList

portNum :: Int
portNum = 8609

 ---
 --- Need to test
 ---
main :: IO()
main = do 
  serverList <- newServer 
  initalise serverList
  openSocket
  
initalise :: ServerList -> IO()
initalise serverList =  do
    handle <- openFile "servers.txt" ReadMode
    myloop handle 
    -- add files for each server
    where 
      myloop handle  = do
        test <- hIsEOF handle
        if test
          then do
            return()
          else do
            line <- hGetLine handle
            populateServers serverList line 
            myloop handle  
        

populateServers :: ServerList -> String -> IO()
populateServers serverList line = atomically $ do
  servers <- readTVar serverList
  case words line of 
    [serverName, portNo] -> do
      server <- newFileServer serverName (read portNo)
      let addList = M.insert serverName server servers
      writeTVar serverList addList
    _ -> do
      return ()

newFileServer :: String -> Int -> STM FileServer
newFileServer serverName port = do
  --fileL <- newFileL
  return FileServer { serverName = serverName
                    , portNo = port
                    --, files = fileL
                    }


openSocket :: IO()
openSocket = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral portNum))
  fileList <- newServer
  putStrLn "Waiting for connections\n"
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted: "++ host ++ "\n"
    forkFinally (runConn handle fileList portNum) (\_ -> hClose handle)


runConn :: Handle -> ServerList -> Int -> IO ()
runConn handle serverList port = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle NoBuffering
  readNxt
  return ()
  where
    readNxt = do 
      command <- hGetLine handle
      case words command of
        ["OPEN", fileName] -> do
          putStrLn "Writing file to client" --find which server has that file