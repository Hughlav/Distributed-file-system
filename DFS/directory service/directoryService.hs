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
type ServerList = TVar (Map Int FileServer)
type Count = TMVar Int

data FileServer = FileServer { serverName :: String
                             , numFiles :: Int
                             , portNo :: Int
                             , files :: FileList
                             } 
                            
newServer :: IO ServerList
newServer = newTVarIO M.empty

numServ :: Int 
numServ = 2

portNum :: Int
portNum = 8909


main :: IO()
main = do 
  count <- newTMVarIO (0 :: Int)
  serverList <- newServer 
  initalise serverList 
  openSocket serverList count
  
initalise :: ServerList -> IO()
initalise serverList =  do
    handle <- openFile "servers.txt" ReadMode
    count <- newTMVarIO (0 :: Int)
    myloop handle count
    -- add files for each server
    where 
      myloop handle count  = do
        test <- hIsEOF handle
        if test
          then do
            return()
          else do
            line <- hGetLine handle
            populateServers count serverList line 
            putStrLn $ "here" 
            myloop handle count  
        

populateServers :: Count -> ServerList -> String -> IO()
populateServers count serverList line = atomically $ do
  servers <- readTVar serverList
  case words line of 
    [serverName, portNo] -> do
      server <- newFileServer serverName (read portNo)
      countc <- takeTMVar count
      let addList = M.insert countc server servers
      writeTVar serverList addList
      putTMVar count (add countc)
    _ -> do
      return ()

newFileServer :: String -> Int -> STM FileServer
newFileServer serverName port = do
  fileL <- newTVar [""]
  return FileServer { serverName = serverName
                    , numFiles = 0
                    , portNo = port
                    , files = fileL
                    }


openSocket :: ServerList -> Count -> IO()
openSocket serverList count = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral portNum))
  --fileList <- newServer
  putStrLn "Waiting for connections\n"
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted: "++ host ++ "\n"
    forkFinally (runConn handle serverList portNum count) (\_ -> hClose handle)


runConn :: Handle -> ServerList -> Int -> Count -> IO ()
runConn handle serverList port count = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle NoBuffering
  readNxt
  return ()
  where
    readNxt = do 
      command <- hGetLine handle
      case words command of
        ["WRITE", fileName, fileContents, clientName] -> do
          portNoServ <- writeNewFile serverList fileName fileContents clientName count
          portNoSer <- portNoServ
          putStrLn $ show portNoSer
          hPutStrLn handle (show portNoSer)
          readNxt
        ["OPEN", fileName] -> do
          putStrLn "Writing file to client" --find which server has that file
          readNxt

writeNewFile :: ServerList -> String -> String -> String -> Count -> IO (IO Int)
writeNewFile serverList fileName fileContents clientName count =  atomically $ do
  countc <- takeTMVar count
  if (countc < numServ)
    then do
      --pNum <- portNumber serverList countc
      --counterN <- newTMVar (add countc)
      putTMVar count (add countc)
      
      return (portNumber serverList countc)
    else do
      --pNum <- 
      putTMVar count (0)
      return (portNumber serverList 0)
  
portNumber :: ServerList -> Int -> IO Int
portNumber serverList index = do
  servers <- atomically $ readTVar serverList
  let maybeServ = M.lookup index servers
  putStrLn $show index
  case maybeServ of
    Just aServ -> do 
      let port = (portNo aServ)
      return port

 



add :: Int ->  Int
add x  = x+1
  