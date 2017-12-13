{-# LANGUAGE LambdaCase,RecordWildCards, OverloadedStrings #-} 

module Main where
import System.IO
import Control.Exception
import Control.Concurrent 
import Control.Monad (when, forever)
import Control.Monad.Fix (fix)
import Network (PortID(..), accept, listenOn, withSocketsDo, Socket)
import Control.Concurrent.STM
import Network.Socket hiding (accept)
import qualified Data.Map as M
import Data.Map (Map) 
import Data.List
import Data.Sequence
import System.Exit
import Data.IntSet (findMax, fromList)
import System.Directory

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

--number of servers live. used for dynamically reading in 
--settings of server and spreading file load
numServ :: Int 
numServ = 2

--portnumber for connection to directory service
portNum :: Int
portNum = 6626

-- initalise directory service
main :: IO()
main = do 
  count <- newTMVarIO (0 :: Int) --used for spreading load across directory server
  serverList <- newServer 
  initalise serverList -- read in server settings from server settings file "servers.txt"
  openSocket serverList count -- open sockets for 
  

initalise :: ServerList -> IO()
initalise serverList =  do
    handle <- openFile "servers.txt" ReadMode
    count <- newTMVarIO (0 :: Int)
    myloop handle count
    servers <- atomically $ readTVar serverList
    let fileServers = M.elems servers
    mapM_ (\a -> getFilesForServer (files a) (serverName a)) fileServers -- add files for each server
    where 
      myloop handle count  = do
        test <- hIsEOF handle --read until end of file
        if test
          then do
            return()
          else do
            line <- hGetLine handle --get next line of settings
            populateServers count serverList line --add servers to ServerList
            myloop handle count  
        
-- adding server name and portnumber to serverList
populateServers :: Count -> ServerList -> String -> IO()
populateServers count serverList line = atomically $ do
  servers <- readTVar serverList
  case words line of 
    [serverName, portNo] -> do
      server <- newFileServer serverName (read portNo) --newserver type
      countc <- takeTMVar count --variable holding number of server so far
      let addList = M.insert countc server servers
      writeTVar serverList addList -- add to serverlist
      putTMVar count (add countc) --increment count
    _ -> do
      return ()

-- create new Fileserver object.
newFileServer :: String -> Int -> STM FileServer
newFileServer serverName port = do
  fileL <- newTVar [""]
  return FileServer { serverName = serverName
                    , numFiles = 0
                    , portNo = port
                    , files = fileL
                    }

--open socket to accept client connections.
openSocket :: ServerList -> Count -> IO()
openSocket serverList count = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral portNum))
  forkIO (readytoclose sock)
  putStrLn "Waiting for connections\n"
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted: "++ host ++ "\n"
    forkFinally (runConn handle serverList portNum count) (\_ -> hClose handle)

-- run each clients connection (in own thread)
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
        ["WRITE", fileName, clientName] -> do -- client wants to write file
          portNoServ <- fileServerPortNum serverList fileName --find portnumber of server that has that file (if any)
          if (portNoServ /= 0)
            then do -- existing file
              putStrLn $ show portNoServ
              hPutStrLn handle (show portNoServ) -- send port number of server to client.
            else do -- new file
              portNoServ <- writeNewFile serverList fileName clientName count --decide which server to allocate to new file.
              portNoSer <- portNoServ
              hPutStrLn handle (show portNoSer) --send portnum to client
          readNxt
        ["OPEN", fileName] -> do --client wants to open a file
          putStrLn "Writing file to client" --find which server has that file
          portNoServ <-fileServerPortNum serverList fileName -- find portnumber of server
          putStrLn $ "PortNum of where file is stored is: " ++ (show portNoServ)
          hPutStrLn handle (show portNoServ) -- send to client 
          readNxt
        ["CLOSE", fileName] -> do --close file
          putStrLn "getting port to close"
          portNoServ <- fileServerPortNum serverList fileName
          if (portNoServ /= 0)
            then do -- existing file
              hPutStrLn handle (show portNoServ)
              putStrLn $ show portNoServ --send port num to client
            else do 
              putStrLn "error not on a server"

--function for finding the server for a given file
fileServerPortNum :: ServerList -> String -> IO Int
fileServerPortNum serverList fileName = do
  servers <- atomically $ readTVar serverList
  let fileServers = M.elems servers
  portList <- mapM (\a -> findPort (files a) (portNo a) fileName) fileServers --compare file wanted with files on each fileserver
  let a = findMax (Data.IntSet.fromList portList) -- values will be 0 apart from where file is (unless file doesnt exist then all will be 0)
  return a -- return that portnum

-- compare filename with list of filenames on a server. if found return port number.
findPort :: FileList -> Int -> String -> IO Int
findPort fileList portNum fileName = do 
  filesList <- atomically $ readTVar fileList
  let maybeFile = elem fileName filesList
  if maybeFile then
    return portNum
  else
    return 0

--decide which fileserver to write to. Spreads the load evenly (in terms of amount of files.)
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
  
--return port number of file depending on value in count variable 
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

-- add file to servers fileList
storeInServerFilesList :: String -> FileServer -> IO()
storeInServerFilesList fileName fileServer = do
  filesList <- atomically $ readTVar (files fileServer)
  let addFileList = insert fileName filesList
  atomically $ writeTVar (files fileServer) addFileList --add new file to list of files
  addToServerTxtFile fileName (serverName fileServer)

--increment int
add :: Int ->  Int
add x  = x+1

--on kill command close socket.
readytoclose :: Socket ->  IO()
readytoclose soc = do 
    closeNow <- getLine
    case words closeNow of
        ["Kill"] -> do
            close soc
            putStrLn "Bye"
            exitSuccess
        [_] -> do
            readytoclose soc

getFilesForServer :: FileList -> String -> IO()
getFilesForServer fileList serverName = do
  let servfile = serverName ++ ".txt"
  handle <- openFile servfile ReadMode
  myloop handle fileList
  where 
    myloop handle fileList  = do
      test <- hIsEOF handle --read until end of file
      if test
        then do
          return()
        else do
          fileName <- hGetLine handle --get next line
          filesList <- atomically $ readTVar fileList
          let addFileList = insert fileName filesList
          atomically $ writeTVar fileList addFileList
  
  
addToServerTxtFile :: String -> String -> IO()
addToServerTxtFile fileName serverName = do
  let servfile = serverName ++ ".txt"
  checkNew <- doesFileExist servfile --see if file exists
  if checkNew
    then do
      fileHandle <- openFile servfile AppendMode
      hPutStrLn fileHandle fileName
    else do
      putStrLn "that txt file doesnt exist for that server."
      putStrLn servfile
