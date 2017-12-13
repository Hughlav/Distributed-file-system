module Client where

import System.IO
import Control.Exception
import Control.Concurrent 
import Control.Monad (when, forever, replicateM)
import Control.Monad.Fix (fix)
import Network (PortID(..), accept, listenOn, withSocketsDo, Socket,connectTo)
import Control.Concurrent.STM
import qualified Data.Map as M
import Data.Map (Map)
import Data.Typeable
import Data.List
import Text.Read
import System.Directory
import System.Exit
    
type Msg = (Int, String)
type Server = TVar (Map Int Room)
    
type RoomName = String
data Room = Room 
  { roomName :: RoomName
  }
    
    
newServer :: IO Server
newServer = newTVarIO M.empty
  

portNumLocking :: Int
portNumLocking = 5645   

runConn :: Handle -> String -> Int -> IO ()
runConn handle clientName updatePort = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle NoBuffering
  readNxt
  hClose handle
  exitSuccess
  return ()
  where
    readNxt = do 
      putStrLn "What should i do?"
      command <- getLine  
      case words command of
        ["Open", fileName] -> do
          readFileandCache handle fileName clientName updatePort --check if i have file cached
          putStrLn "File Cahced"
          readNxt
        ["Write", fileName] -> do
          allowedWrite <- requestLock fileName
          checkNew <- doesFileExist fileName
          if checkNew 
            then do -- file cached
              putStrLn "Ready to update local copy. Enter Replace or Append"
              reply <- getLine
              replaceOrAppend reply fileName handle clientName updatePort
              readNxt
            else do -- new file
              readFileandCache handle fileName clientName updatePort
              checkNew' <- doesFileExist fileName
              if checkNew'
                then do
                  putStrLn "That file already existed. Do you want to Replace or Append"
                  reply <- getLine
                  replaceOrAppend reply fileName handle clientName updatePort
                  readNxt
                else do 
                  putStrLn "Creating new file.\n Please enter contents. \nEnter !EOF! on final line to indicate the end of the file."
                  let fileStr = ""
                  fileStr <- myLoop fileStr 
                  writeNewFileandCache handle fileName fileStr clientName updatePort
                  readNxt
          readNxt
        ["Close", fileName] -> do
          closeFileonServer handle fileName clientName updatePort
          readNxt
        ["Kill"] ->
          exitSuccess
        ["Version", fileName] -> do 
          let req =  "VER " ++ fileName
          hPutStrLn handle req
          readNxt
        _ -> do
          putStrLn $ "Do not recognise command: " ++ command
          putStrLn "Options are: \"Open\" \"Write\" \"Close\" \"Kill\" " 
          readNxt

replaceOrAppend :: String -> String -> Handle -> String -> Int -> IO()
replaceOrAppend reply fileName handle clientName updatePort = do
  case words reply of 
    ["Replace"] -> do
      putStrLn "Replacing file conents.\n Please enter contents. \nEnter !EOF! on final line to indicate the end of the file."
      let fileStr = ""
      fileStr <- myLoop fileStr 
      writeNewFileandCache handle fileName fileStr clientName updatePort
    ["Append"] -> do
      putStrLn "Adding to file conents.\nPlease enter contents. \nEnter !EOF! on final line to indicate the end of the file."
      let fileStr = ""
      fileStr <- myLoop fileStr
      appendFileandCache handle fileName fileStr clientName updatePort
    [_] -> do
      putStrLn "Unrecognised command. aborting write."
      
requestLock :: String -> IO()
requestLock fileName = do 
  handle <- connectTo "localhost" (PortNumber (fromIntegral portNumLocking))
  hPutStrLn handle $ "Write " ++ fileName
  putStrLn $ "waiting for lock on: " ++ fileName
  myloop handle
  where 
    myloop handle = do
      waitForReply <- hReady handle
      if waitForReply
        then do
          true <- hGetLine handle
          putStrLn true
        else 
          myloop handle

  

myLoop :: String -> IO String
myLoop fileStr  = do 
  file <- getLine
  if file == "!EOF!"
    then do
      putStrLn "File Chached!"
      return (fileStr)
    else do 
      myLoop (fileStr ++ "\n" ++ file) 

closeFileonServer :: Handle -> String -> String -> Int -> IO()
closeFileonServer handle fileName clientsName updatePort = do
  let requestClose = "CLOSE " ++ fileName
  hPutStrLn handle requestClose
  portServ <- hGetLine handle --check for error
  sendClose portServ fileName clientsName updatePort
    
sendClose :: String -> String -> String -> Int -> IO()
sendClose portServ fileName clientsName updatePort = do 
  handleServ <- connectTo "localhost" (PortNumber (fromIntegral (read portServ)))
  let request = "CLOSE " ++ fileName ++ " " ++ (show updatePort)
  hPutStrLn handleServ request
  putStrLn $ "File " ++ fileName ++ " Closed"


openSocket :: Int -> String -> Int -> IO()
openSocket portNum clientName updatePort = withSocketsDo $ do
  handle <- connectTo "localhost" (PortNumber (fromIntegral portNum)) 
  putStrLn "Connected to server\n"
  forkIO (awaitUpdate updatePort)
  putStrLn "running con"
  runConn handle clientName updatePort
  return()

     
readFileandCache :: Handle -> String -> String ->  Int -> IO()
readFileandCache directoryHandle fileName clientsName updatePort = do
  let requestDirectoryService = "OPEN " ++ fileName
  hPutStrLn directoryHandle requestDirectoryService
  portServ <- hGetLine directoryHandle
  if (portServ == "0")
    then do
      putStrLn "That file does not exist yet."
    else do
      handle <- connectTo "localhost" (PortNumber (fromIntegral (read portServ)))
      putStrLn "Connected to fileServer, opening file"
      let request = "OPEN " ++ fileName ++ " " ++ (show updatePort)
      hPutStrLn handle request
      --putStrLn "Waiting for file"
      let fileStr = ""
      fileStr <- myLoop fileStr handle
      writeFile fileName fileStr
      where
        myLoop fileStr handle = do
          file <- hGetLine handle
          if file == "!EOF!"
            then do
              --putStrLn "File Chached!"
              return (fileStr)
            else do 
              myLoop (fileStr ++ "\n" ++ file) handle
    
writeNewFileandCache :: Handle -> String -> String -> String -> Int -> IO()
writeNewFileandCache handle fileName contents clientName updatePort = do 
  putStrLn "getting port of server"
  let request = "WRITE " ++ fileName ++ " " ++ clientName --contents with no space
  hPutStrLn handle request
  portServ <- hGetLine handle
  putStrLn $ "File: " ++ fileName ++ " sent to cache"
  writeFileandClose fileName contents
  handleServ <- connectTo "localhost" (PortNumber (fromIntegral (read portServ)))
  let request' = "WRITE " ++ fileName ++ " " ++ (show updatePort)
  hPutStrLn handleServ request'
  reply <- hGetLine handleServ
  case words reply of
    ["READY"] -> do
      hPutStrLn handleServ (contents ++ "\n!EOF!")
      putStrLn $ "File: " ++ fileName ++ " sent to a fileserver"
      lockFile fileName
      --forkFinally (awaitUpdate handleServ) (\_ -> hClose handleServ) -- fork thread for getting updates on cached file
      return()
    [_] -> do 
      putStrLn $ "Fileserver says " ++ reply
      putStrLn "Fileserver not ready for file. aborting"
      return()
    
appendFileandCache :: Handle -> String -> String -> String -> Int -> IO()   
appendFileandCache handle fileName fileStr clientName updatePort = do
  putStrLn "getting port of server"
  let request = "WRITE " ++ fileName ++ " " ++ clientName --contents with no space
  hPutStrLn handle request
  portServ <- hGetLine handle
  putStrLn $ "File: " ++ fileName ++ " sent to cache"
  appendFileandClose fileName fileStr
  handleServ <- connectTo "localhost" (PortNumber (fromIntegral (read portServ)))
  contentNow <- readFile fileName
  let request' = "WRITE " ++ fileName ++ " " ++ (show updatePort)
  hPutStrLn handleServ request'
  reply <- hGetLine handleServ
  case words reply of
    ["READY"] -> do
      hPutStrLn handleServ (contentNow ++ "\n!EOF!")
      putStrLn $ "File: " ++ fileName ++ " sent to a fileserver"
      lockFile fileName
      return()
    [_] -> do 
      putStrLn "Fileserver not ready for file. aborting"
      return()

writeFileandClose :: String -> String-> IO() 
writeFileandClose fileName toWrite = do
  fileHandle <- openFile fileName WriteMode
  hPutStr fileHandle toWrite
  hClose fileHandle
  putStrLn "Written to file"

appendFileandClose :: String -> String -> IO()
appendFileandClose fileName toAdd = do
  fileHandle <- openFile fileName AppendMode
  hPutStr fileHandle toAdd
  hClose fileHandle

awaitUpdate :: Int -> IO()
awaitUpdate updatePort = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral updatePort))
  putStrLn "awaiting updates" --does not appear to actually wait.
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted: "++ host ++ "\n"
    forkFinally (runUpdate handle) (\_ -> hClose handle)
  
runUpdate :: Handle -> IO()
runUpdate handleUpdate = do
  putStrLn "waiting for updates"
  myloop
  where
    myloop = do
        update <- hGetLine handleUpdate
        hPutStrLn handleUpdate $ "Hi, sending update"
        putStrLn $ "Update: " ++ update
        case words update of
          ["UPDATE", fileName] -> do
            hPutStrLn handleUpdate  $ "READY " ++ fileName
            updateFile fileName handleUpdate
          _ -> do 
            putStrLn "Not UPDATE as expected"
            myloop


updateFile :: String -> Handle -> IO()
updateFile fileName handleServ = do
  let fileStr = ""
  fileStr <- myLoop fileStr
  writeFile fileName fileStr
  where
      myLoop fileStr = do 
        file <- hGetLine handleServ
        if file == "!EOF!"
          then do
            putStrLn "File Cache Updated!"
            return (fileStr)
          else do 
            myLoop (fileStr ++ file ++ "\n")

lockFile :: String -> IO()
lockFile fileName = do 
  putStrLn "telling lock server im done with file"
  handle <- connectTo "localhost" (PortNumber (fromIntegral portNumLocking))
  hPutStrLn handle $ "LOCK " ++ fileName