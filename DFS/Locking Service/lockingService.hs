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
import System.Exit

type FileList = TVar (Map String Bool)

portNum :: Int
portNum = 5645

newServer :: IO FileList
newServer = newTVarIO M.empty

main :: IO()
main = do 
  openSocket 
  readytoclose

openSocket :: IO()
openSocket = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral portNum))
  fileList <- newServer
  putStrLn "Waiting for connections\n"
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted: "++ host ++ "\n"
    forkFinally (runConn handle fileList portNum) (\_ -> hClose handle)


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
            isNewFile fileName fileList
            checkLock fileName fileList
            hPutStrLn handle "TRUE"
        ["LOCK", fileName] -> do 
            putStrLn "changing lock to True"
            files <- atomically $ readTVar fileList
            let maybeLock = M.lookup fileName files
            case maybeLock of 
                Nothing -> putStrLn "error... no lock for that file"
                Just aLock -> atomically $ do
                    files <- readTVar fileList
                    let oldLock = M.delete fileName files
                    let updateLock = M.insert fileName True oldLock
                    writeTVar fileList updateLock
            


isNewFile :: String -> FileList -> IO ()
isNewFile fileName fileList = atomically $ do
    files <- readTVar fileList
    let maybeFile = M.lookup fileName files
    case maybeFile of 
        Nothing -> do 
            --new file 
            let newFileList = M.insert fileName True files
            writeTVar fileList newFileList
            return ()
        Just aFile -> do
            return()
            

readytoclose :: IO()
readytoclose = do 
    closeNow <- getLine
    case words closeNow of
        ["Kill"] -> do
            exitSuccess
        [_] -> do
            readytoclose

checkLock :: String -> FileList -> IO()
checkLock fileName fileList = do
    files <- atomically $ readTVar fileList
    let maybeLock = M.lookup fileName files
    case maybeLock of 
        Nothing -> putStrLn "error... no lock for that file"
        Just aLock -> do
            myloop aLock fileName fileList
            where 
                myloop aLock fileName fileList = do
                    if aLock
                        then atomically $ do
                            files <- readTVar fileList
                            let oldLock = M.delete fileName files
                            let updateLock = M.insert fileName False oldLock
                            writeTVar fileList updateLock
                            return()
                        else do
                            checkLock fileName fileList
