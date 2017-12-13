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



type CachedBy = TVar (Map Int Handle)
type FileList = TVar (Map String File)

data File = File
  { fileVersion :: TVar Int
  , cachedBy :: CachedBy
  }


newServer :: IO FileList
newServer = newTVarIO M.empty
  

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
        ["OPEN", fileName, clientPort] -> do
          putStrLn "Writing file to client"
          openFileandSend handle fileList fileName (read clientPort)
          putStrLn "readingNxt"
          readNxt
        ["WRITE", fileName, clientPort] -> do
          hPutStrLn handle "READY"
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
        ["READY", fileName] -> do
            contents <- readFile fileName
            putStrLn $ "Updated file contents are: \n" ++ contents
            hPutStrLn handle (contents ++ "\n!EOF!")
        ["CLOSE", fileName, clientPort] -> do
            removeClientfromCacheList fileName (read clientPort) fileList
        ["VER", fileName] -> do 
          ver <- checkVer fileName fileList
          case ver of 
            0 -> putStrLn "That file does not exist"
            _ -> do
              putStrLn $ "File Version is: " ++ show ver
          readNxt
        _ -> do
          putStrLn $ "do not recognise command: " ++ command
          readNxt

removeClientfromCacheList :: String -> Int -> FileList -> IO()
removeClientfromCacheList fileName clientPort fileList = atomically $ do
    list <- readTVar fileList
    case M.lookup fileName list of 
        Nothing -> do
            error "That file list does not exist"
        Just aFile -> do
            cacheList <- readTVar (cachedBy aFile)
            case M.lookup clientPort cacheList of
              Nothing -> do -- add client to list
                error "Client was already of cacheList"
              Just aClient -> do--update verion number
                let addCacheList = M.delete clientPort cacheList
                writeTVar (cachedBy aFile) addCacheList
 

checkVer :: String -> FileList -> IO Int
checkVer fileName fileList = atomically $ do
  list <- readTVar fileList
  case M.lookup fileName list of 
    Nothing -> do 
      return (0 :: Int)
    Just aFile -> do
      ver <- readTVar (fileVersion aFile) 
      return (ver :: Int)


openSocket :: Int -> IO()
openSocket portNum = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral portNum))
  fileList <- newServer
  putStrLn "Waiting for connections\n"
  -- readytoclose close properly
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted: "++ host ++ "\n"
    forkFinally (runConn handle fileList portNum) (\_ -> hClose handle)
 
openFileandSend :: Handle -> FileList -> String -> Int -> IO() 
openFileandSend clientHandle fileList filename clientPort = do
    putStrLn "checking file exist"
    fileExist <- addToFileListRead clientHandle filename fileList clientPort ----STUCK
    putStrLn $ "file exist: " ++ (show fileExist)
    if fileExist
        then do
            contents <- readFile filename
            putStrLn $ "File contents are: \n" ++ contents
            hPutStrLn clientHandle (contents ++ "\n!EOF!")
        else do
            let reply = "That file does not exist"
            putStrLn reply
            hPutStrLn clientHandle reply

openFileandSend' :: Int -> FileList -> String -> IO() 
openFileandSend' clientPort fileList fileName = do
  clientHandle <- connectTo "localhost" (PortNumber (fromIntegral clientPort))
  putStrLn "updating client cache"
  hPutStrLn clientHandle $ "Hi "  ++ fileName
  hPutStrLn clientHandle $ "UPDATE "  ++ fileName
  loop clientHandle fileName
  where
    loop clientHandle fileName = do
      response <- hGetLine clientHandle -- not getting response?
      case words response of
        ["READY", _] -> do -- fileName
          contents <- readFile fileName
          putStrLn $ "Updated file contents are: \n" ++ contents
          hPutStrLn clientHandle (contents ++ "\n!EOF!")
        _-> do
          putStrLn $ "not Ready but: " ++ response
          loop clientHandle fileName
    
  
writeFileList :: Handle -> String -> String -> Int -> FileList -> IO()
writeFileList handle fileName fileContents clientPort fileList = do
  addToFileListWrite handle fileName fileList clientPort
  writeFileandClose fileName fileContents 
  updateClientCache fileList fileName clientPort
  return()

updateClientCache :: FileList -> String -> Int ->IO()
updateClientCache fileList fileName hasUpdatedVer = do
  list <- atomically $ readTVar fileList
  case M.lookup fileName list of
    Nothing -> do
      --should not be possible
      error "Cannot update cache for file not on fileList"
    Just aFile -> do
      cacheList <- atomically $ readTVar (cachedBy aFile)
      let oldCaches = M.delete hasUpdatedVer cacheList
      let ports = M.keys oldCaches
      putStrLn (show ports)
      mapM_ (\x -> openFileandSend' x fileList fileName) ports 


writeFileandClose :: String -> String-> IO() 
writeFileandClose fileName toWrite = do
  fileHandle <- openFile fileName WriteMode
  hPutStr fileHandle toWrite
  hClose fileHandle
  putStrLn "Written to file"

addToFileListWrite :: Handle -> String -> FileList -> Int -> IO()
addToFileListWrite handle fileName fileList clientPort = do
  list <- atomically $ readTVar fileList
  case M.lookup fileName list of 
    Nothing -> do --new file
      putStrLn "add nothing"
      file <- atomically $ newFile handle fileName clientPort
      atomically $ modifyTVar (fileVersion file) (add 1)
      let addList = M.insert fileName file list
      atomically $ writeTVar fileList addList
      -- cacheList <- readTVar (cachedBy file)
      -- let addCacheList = M.insert clientPort handle cacheList --do i do this twice?
      -- writeTVar (cachedBy file) addCacheList
    Just aFile -> do --exising file 
      putStrLn "add just"
      cacheList <- atomically $ readTVar (cachedBy aFile)
      case M.lookup clientPort cacheList of
        Nothing -> do -- add client to list
          putStrLn "add just nothing"
          atomically $ modifyTVar (fileVersion aFile) (add 1)
          ver <- atomically $ readTVar (fileVersion aFile) 
          let addCacheList = M.insert clientPort handle cacheList
          atomically $ writeTVar (cachedBy aFile) addCacheList
        Just aClient -> do--update verion number
          putStrLn "add just just"
          atomically $modifyTVar (fileVersion aFile) (add 1)
      
addToFileListRead :: Handle -> String -> FileList -> Int -> IO Bool
addToFileListRead handle fileName fileList clientPort = do
  list <- atomically $ readTVar fileList
  case M.lookup fileName list of 
    Nothing -> do --file does not exist
      putStrLn "in Nothing"
      error "Cannot open a file that does not exist"
      return False
    Just aFile -> do --exising file 
      putStrLn "in Just"
      cacheList <- atomically $ readTVar (cachedBy aFile) -- stuck here
      putStrLn $ "cache list: " ++ (show cacheList)
      case M.lookup clientPort cacheList of
        Nothing -> do -- add client to list
          putStrLn "in Just Nothin"
          let addCacheList = M.insert clientPort handle cacheList
          atomically $ writeTVar (cachedBy aFile) addCacheList
          return True
        Just aClient -> do --update verion number
          putStrLn "in Just Just"
          return True


newFile :: Handle -> String -> Int -> STM File
newFile handle fileName clientPort = do
  cacheList <- newTVar $ M.insert clientPort handle M.empty
  vers <- newTVar 0
  return File { fileVersion = vers
              , cachedBy = cacheList
              }

add :: Int -> Int -> Int 
add x y = x+y

-- readytoclose :: IO()
-- readytoclose = do 
--     closeNow <- getLine
--     case words closeNow of
--         ["Kill"] -> do
--             exitSuccess
--         [_] -> do
--             readytoclose