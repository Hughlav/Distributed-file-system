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
import Data.List
import Data.Sequence
import Data.IntSet (findMax, fromList)

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
portNum = 6725


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
        ["WRITE", fileName, clientName] -> do
          portNoServ <- fileServerPortNum serverList fileName
          if (portNoServ /= 0)
            then do -- existing file
              putStrLn $ show portNoServ
              hPutStrLn handle (show portNoServ)
            else do -- new file
              portNoServ <- writeNewFile serverList fileName clientName count
              portNoSer <- portNoServ
              putStrLn $ show portNoSer
              hPutStrLn handle (show portNoSer)
          readNxt
        ["OPEN", fileName] -> do
          putStrLn "Writing file to client" --find which server has that file
          portNoServ <-fileServerPortNum serverList fileName
          putStrLn $ "PortNum of where file is stored (to open) is: " ++ (show portNoServ)
          hPutStrLn handle (show portNoServ)
          readNxt
        ["CLOSE", fileName] -> do
          putStrLn "getting port to close"
          portNoServ <- fileServerPortNum serverList fileName
          if (portNoServ /= 0)
            then do -- existing file
              hPutStrLn handle (show portNoServ)
              putStrLn $ show portNoServ
            else do
              putStrLn "error not on a server"


fileServerPortNum :: ServerList -> String -> IO Int
fileServerPortNum serverList fileName = do
  servers <- atomically $ readTVar serverList
  let fileServers = M.elems servers
  portList <- mapM (\a -> findPort (files a) (portNo a) fileName) fileServers
  let a = findMax (Data.IntSet.fromList portList)
  return a

findPort :: FileList -> Int -> String -> IO Int
findPort fileList portNum fileName = do 
  filesList <- atomically $ readTVar fileList
  let maybeFile = elem fileName filesList
  if maybeFile then
    return portNum
  else
    return 0


writeNewFile :: ServerList -> String -> String -> Count -> IO (IO Int)
writeNewFile serverList fileName clientName count =  atomically $ do
  countc <- takeTMVar count
  if (countc < numServ)
    then do
      putTMVar count (add countc)
      return (portNumber serverList fileName countc)
    else do
      putTMVar count (0)
      return (portNumber serverList fileName 0)
  
portNumber :: ServerList -> String -> Int -> IO Int
portNumber serverList fileName index = do
  servers <- atomically $ readTVar serverList
  let maybeServ = M.lookup index servers
  putStrLn $show index
  case maybeServ of
    Just aServ -> do 
      storeInServerFilesList fileName aServ --Store file name in file list for this server
      let port = (portNo aServ)
      return port

storeInServerFilesList :: String -> FileServer -> IO()
storeInServerFilesList fileName fileServer = atomically $ do
  filesList <- readTVar (files fileServer)
  let addFileList = insert fileName filesList
  writeTVar (files fileServer) addFileList --add new file to list of files


add :: Int ->  Int
add x  = x+1
  