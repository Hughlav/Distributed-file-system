module FileServer where 

{-# LANGUAGE LambdaCase,RecordWildCards, OverloadedStrings #-} 

import System.IO
import Control.Exception
import Control.Concurrent 
import Control.Monad (when, forever)
import Control.Monad.Fix (fix)
import Network (PortID(..), accept, listenOn, withSocketsDo, Socket, connectTo)
import Control.Concurrent.STM
import qualified Data.Map as M
import Data.Map (Map)
import System.Exit
import Network.Socket hiding (accept)
import System.Exit
import Data.List
--import Data.Text hiding (words, head)
import Data.List.Split



type CachedBy = TVar [Int]
type FileList = TVar (Map String File)

data File = File
  { fileVersion :: TVar Int
  , cachedBy :: CachedBy
  }


newServer :: IO FileList
newServer = newTVarIO M.empty
  
--run client connection
runConn :: Handle -> FileList -> Int -> IO ()
runConn handle fileList port = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle NoBuffering
  readNxt
  return ()
  where
    readNxt = do 
      command <- hGetLine handle
      case words command of
        ["OPEN", fileName, clientPort] -> do -- Client wants to open a file
          putStrLn $ "Writing file to client " ++ fileName
          openFileandSend handle fileList fileName (read clientPort) --open file and send
          readNxt
        ["WRITE", fileName, clientPort] -> do --Client wants to write a file
          hPutStrLn handle "READY" -- tell client server is ready to accept file
          putStrLn "Waiting for file"
          let fileStr = ""
          fileStr <- myLoop fileStr handle
          putStrLn "Saving file"
          putStrLn $ "Portnum is: " ++ clientPort
          writeFileList handle fileName fileStr (read clientPort) fileList
          readNxt
          where
              myLoop fileStr handle = do 
                file <- hGetLine handle
                if file == "!EOF!"
                  then do
                    return (fileStr)
                  else do 
                    myLoop (fileStr ++ file ++ "\n") handle
        ["READY", fileName] -> do --Client ready to recieve updated file (after this server issuing a callback)
            contents <- readFile fileName
            hPutStrLn handle (contents ++ "!EOF!") -- \n before !EOF!
        ["CLOSE", fileName, clientPort] -> do -- client done with file, remove client from update cache list.
            removeClientfromCacheList fileName (read clientPort) fileList
            removeClientToTxt fileName (read clientPort)
        ["VER", fileName] -> do  --print version number. note currently not in use.
          ver <- checkVer fileName fileList
          case ver of 
            0 -> putStrLn "That file does not exist"
            _ -> do
              putStrLn $ "File Version is: " ++ show ver
          readNxt
        _ -> do -- incase unrecognised command comes in
          putStrLn $ "do not recognise command: " ++ command
          readNxt

-- Client done with a file. remove them from the cache list.
removeClientfromCacheList :: String -> Int -> FileList -> IO()
removeClientfromCacheList fileName clientPort fileList = atomically $ do
    list <- readTVar fileList
    case M.lookup fileName list of 
        Nothing -> do
            error "That file list does not exist"
        Just aFile -> do
            cacheList <- readTVar (cachedBy aFile)
            let contains = elem clientPort cacheList 
            if contains
              then do
                let addCacheList = delete clientPort cacheList
                writeTVar (cachedBy aFile) addCacheList
              else do 
                error "Client was already off cacheList"
              
                
 
-- check version number of a given file
checkVer :: String -> FileList -> IO Int
checkVer fileName fileList = atomically $ do
  list <- readTVar fileList
  case M.lookup fileName list of 
    Nothing -> do 
      return (0 :: Int)
    Just aFile -> do
      ver <- readTVar (fileVersion aFile) 
      return (ver :: Int)

--starting point for Server. opens socket to accept connections. 
openSocket :: Int -> IO()
openSocket portNum = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral portNum))
  forkIO (readytoclose sock)
  fileList <- newServer
  initServer fileList
  putStrLn "Waiting for connections\n"
  -- readytoclose close properly
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted: "++ host ++ "\n"
    forkFinally (runConn handle fileList portNum) (\_ -> hClose handle)
 
--read a file and send contents to client.
openFileandSend :: Handle -> FileList -> String -> Int -> IO() 
openFileandSend clientHandle fileList filename clientPort = do
    fileExist <- addToFileListRead clientHandle filename fileList clientPort 
    putStrLn $ "fileExist " ++ (show fileExist)
    if fileExist
        then do
            contents <- readFile filename
            hPutStrLn clientHandle (contents ++ "!EOF!") -- \n before !EOF!
        else do
            let reply = "That file does not exist"
            putStrLn reply
            hPutStrLn clientHandle reply

--open file for sending undated file version to client.
openFileandSend' :: Int -> FileList -> String -> IO() 
openFileandSend' clientPort fileList fileName = do
  clientHandle <- connectTo "localhost" (PortNumber (fromIntegral clientPort))
  putStrLn "updating client cache"
  --hPutStrLn clientHandle $ "Hi "  ++ fileName
  hPutStrLn clientHandle $ "UPDATE "  ++ fileName
  loop clientHandle fileName
  where
    loop clientHandle fileName = do
      response <- hGetLine clientHandle 
      case words response of --client ready for update file.
        ["READY", _] -> do 
          contents <- readFile fileName
          putStrLn $ "Updated file contents are: \n" ++ contents
          hPutStrLn clientHandle (contents ++ "!EOF!") -- \n before !EOF!
        _-> do
          putStrLn $ "not Ready but: " ++ response
          loop clientHandle fileName
    
--Write to file list new file (if it is indeed new), write file contents to file. Update any clients that have that file 
-- open that the file contents have been updated
writeFileList :: Handle -> String -> String -> Int -> FileList -> IO()
writeFileList handle fileName fileContents clientPort fileList = do
  addToFileListWrite handle fileName fileList clientPort
  writeFileandClose fileName fileContents 
  updateClientCache fileList fileName clientPort
  return()

-- update all clients on a files cache list that the file has been updated.
updateClientCache :: FileList -> String -> Int ->IO()
updateClientCache fileList fileName hasUpdatedVer = do
  list <- atomically $ readTVar fileList
  case M.lookup fileName list of
    Nothing -> do
      --should not be possible
      error "Cannot update cache for file not on fileList"
    Just aFile -> do
      cacheList <- atomically $ readTVar (cachedBy aFile)
      let oldCaches = delete hasUpdatedVer cacheList
      putStrLn (show oldCaches)
      mapM_ (\x -> openFileandSend' x fileList fileName) oldCaches 

--Write the file locally
writeFileandClose :: String -> String-> IO() 
writeFileandClose fileName toWrite = do
  fileHandle <- openFile fileName WriteMode
  hPutStr fileHandle toWrite
  hClose fileHandle

-- add client to list of clients that have this file cached
addToFileListWrite :: Handle -> String -> FileList -> Int -> IO()
addToFileListWrite handle fileName fileList clientPort = do
  list <- atomically $ readTVar fileList
  case M.lookup fileName list of 
    Nothing -> do --new file, create file object and and add this client to the list
      file <- atomically $ newFile fileName clientPort -------------------------------------
      atomically $ modifyTVar (fileVersion file) (add 1)
      let addList = M.insert fileName file list
      atomically $ writeTVar fileList addList
      createTxtForInit fileName clientPort
    Just aFile -> do --exising file , check if client on list already
      cacheList <- atomically $ readTVar (cachedBy aFile)
      let contains = elem clientPort cacheList
      if contains
        then do --old client, update verion number
          atomically $modifyTVar (fileVersion aFile) (add 1)
        else do -- new client, add client to list and increment version number
          atomically $ modifyTVar (fileVersion aFile) (add 1)
          ver <- atomically $ readTVar (fileVersion aFile) 
          let addCacheList = insert clientPort cacheList
          atomically $ writeTVar (cachedBy aFile) addCacheList
          addClientToTxt fileName clientPort
        
          
 
--Client opens file, as above check if client is on cachelist of file and act accordingly.
addToFileListRead :: Handle -> String -> FileList -> Int -> IO Bool
addToFileListRead handle fileName fileList clientPort = do
  list <- atomically $ readTVar fileList
  case M.lookup fileName list of 
    Nothing -> do --file does not exist
      error "Cannot open a file that does not exist"
      return False
    Just aFile -> do --exising file 
      cacheList <- atomically $ readTVar (cachedBy aFile) 
      let contains = elem clientPort cacheList
      if contains
        then do 
          return True
        else do -- add client to list
          let addCacheList = insert clientPort cacheList
          atomically $ writeTVar (cachedBy aFile) addCacheList
          addClientToTxt fileName clientPort
          putStrLn "returning True"
          return True
      return True
          

--create a new file and add client creating file to its cachelist
newFile :: String -> Int -> STM File
newFile fileName clientPort = do
  cacheList <- newTVar [clientPort]
  vers <- newTVar 0
  return File { fileVersion = vers
              , cachedBy = cacheList
              }

newFile' :: String -> Int -> Int -> STM File
newFile' fileName clientPort vers = do
  cacheList <- newTVar [clientPort]
  versi <- newTVar vers
  return File { fileVersion = versi
              , cachedBy = cacheList
              }

-- add two ints
add :: Int -> Int -> Int 
add x y = x+y

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


initServer :: FileList -> IO()
initServer fileList = do
  handle <- openFile "init/files.txt" ReadMode
  myloop handle fileList
  hClose handle
  where 
    myloop handle fileList= do
      test <- hIsEOF handle --read until end of file
      if test
        then do
          return()
        else do
          line <- hGetLine handle --get next line of settings
          populateList fileList line --add servers to ServerList
          myloop handle fileList  

populateList :: FileList -> String -> IO()
populateList fileList line = do
  list <- atomically $ readTVar fileList
  putStrLn $ "Line is: " ++ line
  case words line of
    [fileName, vers, portnums] -> do
      let ports = splitOn "-" portnums
      let portInts = map read ports
      let first = head ports
      file <- atomically $ newFile' fileName (read first) (read vers)
      cacheList <- atomically $ readTVar (cachedBy file)
      let addCacheList =  cacheList ++ portInts
      atomically $ writeTVar (cachedBy file) addCacheList
      let addList = M.insert fileName file list
      atomically $ writeTVar fileList addList
    [_] -> do
      putStrLn "ERROR unknown line in files.txt file while initiating fileList"
 
createTxtForInit :: String -> Int -> IO()
createTxtForInit fileName port = do
  handle <- openFile "init/files.txt" AppendMode
  let line = fileName ++ " 0 " ++ (show port)
  hPutStrLn handle line 
  hClose handle

addClientToTxt :: String -> Int -> IO()
addClientToTxt fileName port = do
  handle <- openFile "init/files.txt" ReadMode
  let returnStr = ""
  finalStr <- myloop handle fileName port returnStr
  hClose handle
  writeFile "init/files.txt" finalStr
  where 
    myloop handle fileName port returnStr = do
      test <- hIsEOF handle --read until end of file
      if test
        then do
          return returnStr
        else do
          line <- hGetLine handle --get next line of settings
          case words line of
            [filen, vers, ports] -> do 
              if (filen == fileName)
                then do
                  let newports = ports ++ "-" ++ (show port)
                  let newlineStr = unwords [filen, vers, newports]
                  let newReturnStr = returnStr ++ newlineStr ++ "\n"
                  myloop handle fileName port newReturnStr
                else do
                  let newReturnStr = returnStr ++ line ++ "\n"
                  myloop handle fileName port newReturnStr
            [_] -> do 
              putStrLn "unrecgonised line"
              myloop handle fileName port returnStr
  
removeClientToTxt :: String -> Int -> IO() ---
removeClientToTxt fileName port = do
  handle <- openFile "init/files.txt" ReadMode
  let returnStr = ""
  finalStr <- myloop handle fileName port returnStr
  hClose handle
  writeFile "init/files.txt" finalStr
  where 
    myloop handle fileName port returnStr = do
      test <- hIsEOF handle --read until end of file
      if test
        then do
          return returnStr
        else do
          line <- hGetLine handle --get next line of settings
          case words line of
            [filen, vers, ports] -> do 
              if (filen == fileName)
                then do
                  let portsSplit = splitOn "-" ports
                  let newPortInts = delete (show port) portsSplit
                  let finalPort = intercalate "-" newPortInts
                  let newlineStr = unwords [filen, vers, finalPort]
                  let newReturnStr = returnStr ++ newlineStr ++ "\n"
                  myloop handle fileName port newReturnStr
                else do
                  let newReturnStr = returnStr ++ line ++ "\n"
                  myloop handle fileName port newReturnStr
            [_] -> do 
              putStrLn "unrecgonised line"
              myloop handle fileName port returnStr


             