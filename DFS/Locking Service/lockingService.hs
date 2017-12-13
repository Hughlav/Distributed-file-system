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
import Network.Socket hiding (accept)
import Data.Map (Map) 
import Data.List
import Data.Sequence
import Data.IntSet (findMax, fromList)
import System.Exit

type FileList = TVar (Map String Bool)

portNum :: Int
portNum = 5545

newServer :: IO FileList
newServer = newTVarIO M.empty

--entry point
main :: IO()
main = do 
  openSocket 
  

--opens socket to accept connections for clients requesting locks.
openSocket :: IO()
openSocket = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral portNum))
  forkIO (readytoclose sock)
  fileList <- newServer
  putStrLn "Waiting for connections\n"
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted: "++ host ++ "\n"
    forkFinally (runConn handle fileList portNum) (\_ -> hClose handle)

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
        ["Write", fileName] -> do
            putStrLn "isNew"
            isNewFile fileName fileList -- check if it is a new file
            putStrLn "checkLock"
            checkLock fileName fileList -- check if lock is free, function only returns when lock is free
            putStrLn "True"
            hPutStrLn handle "TRUE"
        ["LOCK", fileName] -> do --client done with file, set lock to free (true)
            putStrLn "changing lock to True"
            files <- atomically $ readTVar fileList
            let maybeLock = M.lookup fileName files
            case maybeLock of 
                Nothing -> putStrLn "error... no lock for that file" --unlikely to happen but just incase.
                Just aLock -> atomically $ do --set lock to True
                    files <- readTVar fileList
                    let oldLock = M.delete fileName files
                    let updateLock = M.insert fileName True oldLock
                    writeTVar fileList updateLock
            

--check if file exists yet
isNewFile :: String -> FileList -> IO ()
isNewFile fileName fileList = atomically $ do
    files <- readTVar fileList
    let maybeFile = M.lookup fileName files
    case maybeFile of 
        Nothing -> do --new file 
            let newFileList = M.insert fileName True files --add file to list of files set bool to TRUE (indicating file lock is free)
            writeTVar fileList newFileList
            return ()
        Just aFile -> do
            return()
            

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

--check if file is locked and if it is wait for it to be unlocked.
checkLock :: String -> FileList -> IO()
checkLock fileName fileList = do
    files <- atomically $ readTVar fileList
    let maybeLock = M.lookup fileName files
    case maybeLock of 
        Nothing -> putStrLn "error... no lock for that file" --incase no lock variable was made for file
        Just aLock -> do
            myloop aLock fileName fileList
            where 
                myloop aLock fileName fileList = do
                    if aLock --if lock is free 
                        then atomically $ do --then change to taken (false)
                            files <- readTVar fileList
                            let oldLock = M.delete fileName files
                            let updateLock = M.insert fileName False oldLock
                            writeTVar fileList updateLock
                            return()
                        else do --else wait till it is free.
                            checkLock fileName fileList
