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
  
portUpdate :: Int
portUpdate = 5566

portNumLocking :: Int
portNumLocking = 5646   

runConn :: Handle -> String -> IO ()
runConn handle clientName = do
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
          readFileandCache handle fileName clientName --check if i have file cached
          putStrLn "File Cahced"
          readNxt
        ["Write", fileName] -> do
          allowedWrite <- requestLock fileName
          checkNew <- doesFileExist fileName
          if checkNew 
            then do -- file cached
              putStrLn "Ready to update local copy. Enter Replace or Append"
              reply <- getLine
              replaceOrAppend reply fileName handle clientName
              readNxt
            else do -- new file
              readFileandCache handle fileName clientName
              checkNew' <- doesFileExist fileName
              if checkNew'
                then do
                  putStrLn "That file already existed. Do you want to Replace or Append"
                  reply <- getLine
                  replaceOrAppend reply fileName handle clientName
                  readNxt
                else do 
                  putStrLn "Creating new file.\n Please enter contents. \nEnter !EOF! on final line to indicate the end of the file."
                  let fileStr = ""
                  fileStr <- myLoop fileStr 
                  writeNewFileandCache handle fileName fileStr clientName 
                  readNxt
          readNxt
        ["Close", fileName] -> do
          closeFileonServer handle fileName clientName
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

replaceOrAppend :: String -> String -> Handle -> String -> IO()
replaceOrAppend reply fileName handle clientName = do
  case words reply of 
    ["Replace"] -> do
      putStrLn "Replacing file conents.\n Please enter contents. \nEnter !EOF! on final line to indicate the end of the file."
      let fileStr = ""
      fileStr <- myLoop fileStr 
      writeNewFileandCache handle fileName fileStr clientName
    ["Append"] -> do
      putStrLn "Adding to file conents.\nPlease enter contents. \nEnter !EOF! on final line to indicate the end of the file."
      let fileStr = ""
      fileStr <- myLoop fileStr
      appendFileandCache handle fileName fileStr clientName
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

closeFileonServer :: Handle -> String -> String -> IO()
closeFileonServer handle fileName clientsName = do
  let requestClose = "CLOSE " ++ fileName
  hPutStrLn handle requestClose
  portServ <- hGetLine handle --check for error
  sendClose portServ fileName clientsName
    
sendClose :: String -> String -> String -> IO()
sendClose portServ fileName clientsName = do 
  handleServ <- connectTo "localhost" (PortNumber (fromIntegral (read portServ)))
  let request = "CLOSE " ++ fileName ++ " " ++ clientsName
  hPutStrLn handleServ request
  putStrLn $ "File " ++ fileName ++ " Closed"


openSocket :: Int -> String -> IO()
openSocket portNum clientName = withSocketsDo $ do
  handle <- connectTo "localhost" (PortNumber (fromIntegral portNum)) 
  putStrLn "Connected to server\n"
  runConn handle clientName
  forkIO (awaitUpdate)

      
     
readFileandCache :: Handle -> String -> String -> IO()
readFileandCache directoryHandle fileName clientsName = do
  let requestDirectoryService = "OPEN " ++ fileName
  hPutStrLn directoryHandle requestDirectoryService
  portServ <- hGetLine directoryHandle
  if (portServ == "0")
    then do
      putStrLn "That file does not exist yet."
    else do
      handle <- connectTo "localhost" (PortNumber (fromIntegral (read portServ)))
      putStrLn "Connected to fileServer, opening file"
      let request = "OPEN " ++ fileName ++ " " ++ clientsName
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
    
writeNewFileandCache :: Handle -> String -> String -> String -> IO()
writeNewFileandCache handle fileName contents clientName = do 
  putStrLn "getting port of server"
  let request = "WRITE " ++ fileName ++ " " ++ clientName --contents with no space
  hPutStrLn handle request
  portServ <- hGetLine handle
  putStrLn $ "File: " ++ fileName ++ " sent to cache"
  writeFileandClose fileName contents
  handleServ <- connectTo "localhost" (PortNumber (fromIntegral (read portServ)))
  hPutStrLn handleServ request
  reply <- hGetLine handleServ
  case words reply of
    ["READY"] -> do
      hPutStrLn handleServ (contents ++ "\n!EOF!")
      putStrLn $ "File: " ++ fileName ++ " sent to a fileserver"
      lockFile fileName
      forkFinally (awaitUpdate handleServ) (\_ -> hClose handleServ) -- fork thread for getting updates on cached file
      return()
    [_] -> do 
      putStrLn $ "Fileserver says " ++ reply
      putStrLn "Fileserver not ready for file. aborting"
      return()
    
appendFileandCache :: Handle -> String -> String -> String -> IO()   
appendFileandCache handle fileName fileStr clientName = do
  putStrLn "getting port of server"
  let request = "WRITE " ++ fileName ++ " " ++ clientName --contents with no space
  hPutStrLn handle request
  portServ <- hGetLine handle
  putStrLn $ "File: " ++ fileName ++ " sent to cache"
  appendFileandClose fileName fileStr
  handleServ <- connectTo "localhost" (PortNumber (fromIntegral (read portServ)))
  contentNow <- readFile fileName
  hPutStrLn handleServ request
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

awaitUpdate :: IO()
openSocket portNum = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral portNum))
  putStrLn "awaiting updates" --does not appear to actually wait.
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted: "++ host ++ "\n"
    forkFinally (runUpdate handle) (\_ -> hClose handle)
  
runUpdate :: Handle -> IO()
runUpdate handleUpdate = do
  myloop
  where
    myloop = do
        update <- hGetLine handleServ
        case words update of
          ["UPDATE", fileName] -> do
            hPutStrLn handleServ  $ "READY " ++ fileName
            updateFile fileName handleServ
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