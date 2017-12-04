module FileServer where 

{-# LANGUAGE LambdaCase,RecordWildCards, OverloadedStrings #-} 

import System.IO
import Control.Exception
import Control.Concurrent 
import Control.Monad (when, forever)
import Control.Monad.Fix (fix)
import Network (PortID(..), accept, listenOn, withSocketsDo, Socket)
import Control.Concurrent.STM
import qualified Data.Map as M
import Data.Map (Map)



type CachedBy = TVar (Map String Handle)
type FileList = TVar (Map String File)

data File = File
  { fileVersion :: TVar Int
  , cachedBy :: CachedBy
  }


newServer :: IO FileList
newServer = newTVarIO M.empty

-- portNum :: Int
-- portNum = 8800

-- main :: IO()
-- main = do 
--   openSocket
  

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
        ["OPEN", fileName, clientName] -> do
          putStrLn "Writing file to client"
          openFileandSend handle fileList fileName clientName
          putStrLn "readingNxt"
          readNxt
        ["WRITE", fileName, clientName] -> do
          hPutStrLn handle "READY"
          putStrLn "Waiting for file"
          let fileStr = ""
          fileStr <- myLoop fileStr handle
          putStrLn "Saving file"
          writeFileList handle fileName fileStr clientName fileList
          readNxt
          where
              myLoop fileStr handle = do 
                file <- hGetLine handle
                if file == "!EOF!"
                  then do
                    return (fileStr)
                  else do 
                    myLoop (fileStr ++ "\n" ++ file) handle
        ["READY", fileName] -> do
            contents <- readFile fileName
            putStrLn $ "Updated file contents are: \n" ++ contents
            hPutStrLn handle (contents ++ "\n!EOF!")
        ["CLOSE", fileName, clientName] -> do
            removeClientfromCacheList fileName clientName fileList
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

removeClientfromCacheList :: String -> String -> FileList -> IO()
removeClientfromCacheList fileName clientName fileList = atomically $ do
    list <- readTVar fileList
    case M.lookup fileName list of 
        Nothing -> do
            error "That file list does not exist"
        Just aFile -> do
            cacheList <- readTVar (cachedBy aFile)
            case M.lookup clientName cacheList of
              Nothing -> do -- add client to list
                error "Client was already of cacheList"
              Just aClient -> do--update verion number
                let addCacheList = M.delete clientName cacheList
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
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted: "++ host ++ "\n"
    forkFinally (runConn handle fileList portNum) (\_ -> hClose handle)
 
openFileandSend :: Handle -> FileList -> String -> String ->IO() 
openFileandSend clientHandle fileList filename clientName = do
    fileExist <- addToFileListRead clientHandle filename fileList clientName
    if fileExist
        then do
            contents <- readFile filename
            putStrLn $ "File contents are: \n" ++ contents
            hPutStrLn clientHandle (contents ++ "\n!EOF!")
        else do
            let reply = "That file does not exist"
            putStrLn reply
            hPutStrLn clientHandle reply

openFileandSend' :: Handle -> FileList -> String -> IO() --not in use?
openFileandSend' clientHandle fileList fileName = do
  hPutStrLn clientHandle $ "UPDATE "  ++ fileName
  response <- hGetLine clientHandle
  case words response of
    ["READY"] -> do
      contents <- readFile fileName
      putStrLn $ "Updated file contents are: \n" ++ contents
      hPutStrLn clientHandle (contents ++ "\n!EOF!")
    _-> do 
      error "Client not ready to recieve cache update"
    
  
writeFileList :: Handle -> String -> String -> String -> FileList -> IO()
writeFileList handle fileName fileContents clientName fileList = do
  addToFileListWrite handle fileName fileList clientName
  writeFileandClose fileName fileContents 
  updateClientCache fileList fileName clientName
  return()

updateClientCache :: FileList -> String -> String ->IO()
updateClientCache fileList fileName hasUpdatedVer = do
  list <- atomically $ readTVar fileList
  case M.lookup fileName list of
    Nothing -> do
      --should not be possible
      error "Cannot update cache for file not on fileList"
    Just aFile -> do
      cacheList <- atomically $ readTVar (cachedBy aFile)
      let oldCaches = M.delete hasUpdatedVer cacheList
      let handles = M.elems oldCaches
      mapM_ (\x -> openFileandSend' x fileList fileName) handles 


writeFileandClose :: String -> String-> IO() 
writeFileandClose fileName toWrite = do
  fileHandle <- openFile fileName WriteMode
  hPutStr fileHandle toWrite
  hClose fileHandle
  putStrLn "Written to file"

addToFileListWrite :: Handle -> String -> FileList -> String -> IO()
addToFileListWrite handle fileName fileList clientName = atomically $ do
  list <- readTVar fileList
  case M.lookup fileName list of 
    Nothing -> do --new file
      file <- newFile handle fileName clientName
      modifyTVar (fileVersion file) (add 1)
      let addList = M.insert fileName file list
      writeTVar fileList addList
      cacheList <- readTVar (cachedBy file)
      let addCacheList = M.insert clientName handle cacheList --do i do this twice?
      writeTVar (cachedBy file) addCacheList
    Just aFile -> do --exising file 
      cacheList <- readTVar (cachedBy aFile)
      case M.lookup clientName cacheList of
        Nothing -> do -- add client to list
          modifyTVar (fileVersion aFile) (add 1)
          ver <- readTVar (fileVersion aFile) 
          let addCacheList = M.insert clientName handle cacheList
          writeTVar (cachedBy aFile) addCacheList
        Just aClient -> --update verion number
          modifyTVar (fileVersion aFile) (add 1)
      
addToFileListRead :: Handle -> String -> FileList -> String -> IO Bool
addToFileListRead handle fileName fileList clientName = atomically $ do
  list <- readTVar fileList
  case M.lookup fileName list of 
    Nothing -> do --file does not exist
      error "Cannot open a file that does not exist"
      return False
    Just aFile -> do --exising file 
      cacheList <- readTVar (cachedBy aFile)
      case M.lookup clientName cacheList of
        Nothing -> do -- add client to list
          let addCacheList = M.insert clientName handle cacheList
          writeTVar (cachedBy aFile) addCacheList
          return True
        Just aClient -> --update verion number
          return True


newFile :: Handle -> String -> String -> STM File
newFile handle fileName clientName = do
  cacheList <- newTVar $ M.insert clientName handle M.empty
  vers <- newTVar 0
  return File { fileVersion = vers
              , cachedBy = cacheList
              }

add :: Int -> Int -> Int 
add x y = x+y
