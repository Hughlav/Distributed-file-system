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


type CachedBy = TVar (Map String Int)
type FileList = TVar (Map String File)

data File = File
  { fileVersion :: TVar Int
  , cachedBy :: CachedBy
  }


newServer :: IO FileList
newServer = newTVarIO M.empty

portNum :: Int
portNum = 8800

main :: IO()
main = do 
  
  openSocket
  

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
        ["OPEN", fileName] -> do
          putStrLn "Writing file to client"
          openFileandSend handle fileList fileName
          putStrLn "readingNxt"
          readNxt
        ["WRITE", fileName, fileContents, clientName] -> do
          putStrLn "Saving file"
          writeFileList fileName fileContents clientName fileList
          readNxt
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




checkVer :: String -> FileList -> IO Int
checkVer fileName fileList = atomically $ do
  list <- readTVar fileList
  case M.lookup fileName list of 
    Nothing -> do 
      return (0 :: Int)
    Just aFile -> do
      ver <- readTVar (fileVersion aFile) 
      return (ver :: Int)




openSocket :: IO()
openSocket = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral portNum))
  fileList <- newServer
  putStrLn "Waiting for connections\n"
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted: "++ host ++ "\n"
    forkFinally (runConn handle fileList portNum) (\_ -> hClose handle)
 
openFileandSend :: Handle -> FileList -> String -> IO() 
openFileandSend clientHandle fileList filename = do 
  contents <- readFile filename
  putStrLn $ "File contents are: \n" ++ contents
  hPutStrLn clientHandle (contents ++ "\n!EOF!")
  -- store that client has this file cached add to filesCached
  
writeFileList :: String -> String -> String -> FileList -> IO()
writeFileList fileName fileContents clientName fileList = do
  addToFileList fileName fileList clientName
  writeFileandClose fileName fileContents 


writeFileandClose :: String -> String-> IO() 
writeFileandClose fileName toWrite = do
  fileHandle <- openFile fileName WriteMode
  hPutStr fileHandle toWrite
  hClose fileHandle
  putStrLn "Written to file"

addToFileList :: String -> FileList -> String -> IO()
addToFileList fileName fileList clientName = atomically $ do
  list <- readTVar fileList
  case M.lookup fileName list of 
    Nothing -> do --new file
      file <- newFile 1 fileName clientName
      modifyTVar (fileVersion file) (add 1)
      let addList = M.insert fileName file list
      writeTVar fileList addList
      cacheList <- readTVar (cachedBy file)
      let addCacheList = M.insert clientName (1 :: Int) cacheList
      writeTVar (cachedBy file) addCacheList
    Just aFile -> do --exising file 
      cacheList <- readTVar (cachedBy aFile)
      case M.lookup clientName cacheList of
        Nothing -> do -- add client to list
          modifyTVar (fileVersion aFile) (add 1)
          ver <- readTVar (fileVersion aFile) 
          let addCacheList = M.insert clientName ver cacheList
          writeTVar (cachedBy aFile) addCacheList
          --check others cached are up to date
        Just aClient -> --update verion number
          modifyTVar (fileVersion aFile) (add 1)
          --check others cached are up to date
      
--updateCaches
  

newFile :: Int -> String -> String -> STM File
newFile version fileName clientName = do
  cacheList <- newTVar $ M.insert clientName version M.empty
  vers <- newTVar 0
  return File { fileVersion = vers
              , cachedBy = cacheList
              }

add :: Int -> Int -> Int 
add x y = x+y