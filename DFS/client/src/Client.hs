module Client where

import System.IO
import Control.Exception
import Control.Concurrent 
import Control.Monad (when, forever, replicateM)
import Control.Monad.Fix (fix)
import Network (PortID(..), accept, listenOn, withSocketsDo, Socket,connectTo)
import Network.Socket hiding (accept)
import Control.Concurrent.STM
import qualified Data.Map as M
import Data.Map (Map)
import Data.Typeable
import Data.List
import Text.Read
import System.Directory
import System.Exit

    


portNumLocking :: Int
portNumLocking = 5545   

--Takes user input from terminal to Open(read), Close, or Write.
runConn :: Handle -> String -> Int -> Socket -> IO ()
runConn handle clientName updatePort sock = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle NoBuffering
  readNxt
  hClose handle
  exitSuccess
  return ()
  where
    readNxt = do 
      putStrLn "\nWhat should i do?"
      putStrLn "Options are: \"Open <filename>\" \"Write <filename>\" \"Close <filename>\" \"Kill\" " 
      command <- getLine  
      case words command of
        ["Open", fileName] -> do
          readFileandCache handle fileName clientName updatePort --check if i have file cached. if not and it exists somewhere find and get from fileserver
          readNxt
        ["Write", fileName] -> do
          allowedWrite <- requestLock fileName --Ask for lock before writing a file
          checkNew <- doesFileExist fileName --see if file is cached locally
          if checkNew 
            then do -- file cached
              putStrLn "Ready to update local copy. Enter Replace or Append"
              reply <- getLine
              replaceOrAppend reply fileName handle clientName updatePort --replace contents of file or append.
              readNxt
            else do -- new file
              putStrLn "here"
              readFileandCache handle fileName clientName updatePort --check if file exists, find and get from fileserver
              checkNew' <- doesFileExist fileName -- check if it existed on a fileserver
              if checkNew' --the file did exist on a fileserver, has been cached and ready to be replaced or appended
                then do
                  putStrLn "That file already existed. Do you want to Replace or Append" 
                  reply <- getLine
                  replaceOrAppend reply fileName handle clientName updatePort --replace contents of file or append.
                  readNxt
                else do --the file exists nowhere so it is being created from scratch. 
                  putStrLn "Creating new file.\nPlease enter contents. \nEnter !EOF! on final line to indicate the end of the file."
                  let fileStr = ""
                  fileStr <- readFromTerminal fileStr -- very basic terminal text editor to create file contents. Cannot remove lines, only append or delete.
                  writeNewFileandCache handle fileName fileStr clientName updatePort -- write file to fileserver.
                  readNxt
          readNxt
        ["Close", fileName] -> do
          closeFileonServer handle fileName clientName updatePort --close a file on server (get no updates)
          readNxt
        ["Kill"] -> do -- kill client.
          close sock
          exitSuccess
        ["Version", fileName] -> do --currently not in use, if handle is changed to fileserver handle where file exists, server will print version of file. 
          let req =  "VER " ++ fileName
          hPutStrLn handle req
          readNxt
        _ -> do --unrecognised command.
          putStrLn $ "Do not recognise command: " ++ command
          putStrLn "Options are: \"Open <filename>\" \"Write <filename>\" \"Close <filename>\" \"Kill\" " 
          readNxt

-- self explanitory. When writing a file have the option to replace the entire contents or append. Essentially just to display functionality of fileserver
replaceOrAppend :: String -> String -> Handle -> String -> Int -> IO()
replaceOrAppend reply fileName handle clientName updatePort = do
  case words reply of 
    ["Replace"] -> do
      putStrLn "Replacing file conents.\nPlease enter contents. \nEnter !EOF! on final line to indicate the end of the file."
      let fileStr = ""
      fileStr <- readFromTerminal fileStr 
      writeNewFileandCache handle fileName fileStr clientName updatePort
    ["Append"] -> do
      putStrLn "Adding to file conents.\nPlease enter contents. \nEnter !EOF! on final line to indicate the end of the file."
      let fileStr = ""
      fileStr <- readFromTerminal fileStr
      appendFileandCache handle fileName fileStr clientName updatePort
    [_] -> do
      lockFile fileName
      putStrLn "Unrecognised command. aborting write."
      
-- connent to fileserver and request for lock on a file
requestLock :: String -> IO()
requestLock fileName = do 
  handle <- connectTo "localhost" (PortNumber (fromIntegral portNumLocking))
  hPutStrLn handle $ "Write " ++ fileName
  putStrLn $ "waiting for lock on: " ++ fileName --wait for lock
  myloop handle
  where 
    myloop handle = do
      waitForReply <- hReady handle --when there is something on the handle fileserver is giving permission
      if waitForReply
        then do
          true <- hGetLine handle --read
          putStrLn "Lock recieved"
        else 
          myloop handle

  
--Basic text editor for giving file contents. indicate end of file by entering line containg only !EOF!
readFromTerminal :: String -> IO String
readFromTerminal fileStr  = do 
  file <- getLine
  if file == "!EOF!"
    then do
      putStrLn "File Chached!"
      return (fileStr)
    else do 
      readFromTerminal (fileStr ++ "\n" ++ file) 

--To close a file, askek directory service for location of file. Then send close request to fileserver.
closeFileonServer :: Handle -> String -> String -> Int -> IO()
closeFileonServer handle fileName clientsName updatePort = do
  let requestClose = "CLOSE " ++ fileName
  hPutStrLn handle requestClose
  portServ <- hGetLine handle --check for error
  sendClose portServ fileName clientsName updatePort
    
--send close request to fileserver
sendClose :: String -> String -> String -> Int -> IO()
sendClose portServ fileName clientsName updatePort = do 
  handleServ <- connectTo "localhost" (PortNumber (fromIntegral (read portServ)))
  let request = "CLOSE " ++ fileName ++ " " ++ (show updatePort)
  hPutStrLn handleServ request
  putStrLn $ "File " ++ fileName ++ " Closed"

-- Entry point for a new Client. Opens connection to directory sevice and starts
-- thread that accepts updates for cached files client has open.
openSocket :: Int -> String -> Int -> IO()
openSocket portNum clientName updatePort = withSocketsDo $ do
  handle <- connectTo "localhost" (PortNumber (fromIntegral portNum)) 
  putStrLn "Connected to Directory Server"
  sock <- listenOn (PortNumber (fromIntegral updatePort))
  forkIO (awaitUpdate sock)
  runConn handle clientName updatePort sock
  return()

-- Ask directory service for location of a file. Send read request to the fileserver.
readFileandCache :: Handle -> String -> String ->  Int -> IO()
readFileandCache directoryHandle fileName clientsName updatePort = do
  let requestDirectoryService = "OPEN " ++ fileName
  hPutStrLn directoryHandle requestDirectoryService
  portServ <- hGetLine directoryHandle
  if (portServ == "0") --directory service sends back 0 then file does not exist anywhere.
    then do
      putStrLn "That file does not exist yet."
    else do --send request to fileserver and read across contents of file
      handle <- connectTo "localhost" (PortNumber (fromIntegral (read portServ)))
      putStrLn "Connected to fileServer, opening file"
      let request = "OPEN " ++ fileName ++ " " ++ (show updatePort)
      hPutStrLn handle request
      let fileStr = ""
      fileStr <- myLoop fileStr handle
      writeFile fileName fileStr
      where
        myLoop fileStr handle = do
          file <- hGetLine handle
          if file == "!EOF!"
            then do
              return (fileStr)
            else do 
              myLoop (fileStr ++ "\n" ++ file) handle
    
--Write a new file. Ask directory service which fileserver to write to (to spread load)
writeNewFileandCache :: Handle -> String -> String -> String -> Int -> IO()
writeNewFileandCache handle fileName contents clientName updatePort = do 
  putStrLn "getting port of server"
  let request = "WRITE " ++ fileName ++ " " ++ clientName 
  hPutStrLn handle request
  portServ <- hGetLine handle --get port of server
  putStrLn $ "File: " ++ fileName ++ " sent to cache"
  writeFileandClose fileName contents --write file locally to cache
  handleServ <- connectTo "localhost" (PortNumber (fromIntegral (read portServ)))
  let request' = "WRITE " ++ fileName ++ " " ++ (show updatePort) -- ask fileserver can i write file
  hPutStrLn handleServ request'
  reply <- hGetLine handleServ
  case words reply of
    ["READY"] -> do --fileserver is ready, send contents
      hPutStrLn handleServ (contents ++ "\n!EOF!")
      putStrLn $ "File: " ++ fileName ++ " sent to a fileserver"
      lockFile fileName
      return()
    [_] -> do  -- unrecognised abort write.
      putStrLn $ "Fileserver says " ++ reply
      putStrLn "Fileserver not ready for file. aborting"
      return()
    
-- same as above but for appending to a file.
appendFileandCache :: Handle -> String -> String -> String -> Int -> IO()   
appendFileandCache handle fileName fileStr clientName updatePort = do
  putStrLn "getting port of server"
  let request = "WRITE " ++ fileName ++ " " ++ clientName 
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

-- write file to directory client is in (cache). close here talking about closing handle to file 
-- (as function name writeFile taken and used)
writeFileandClose :: String -> String-> IO() 
writeFileandClose fileName toWrite = do
  fileHandle <- openFile fileName WriteMode
  hPutStr fileHandle toWrite
  hClose fileHandle
  putStrLn "Written to file"

-- same as above but for appending.
appendFileandClose :: String -> String -> IO()
appendFileandClose fileName toAdd = do
  fileHandle <- openFile fileName AppendMode
  hPutStr fileHandle toAdd
  hClose fileHandle

--thread waiting for callback from server when a file is updated by another Client
awaitUpdate :: Socket -> IO()
awaitUpdate sock = withSocketsDo $ do
  forever $ do 
    (handle, host, port) <- accept sock
    putStrLn $ "Connection accepted for updating cache: "++ host ++ "\n"
    forkFinally (runUpdate handle) (\_ -> hClose handle)
  
--for each connection an update is coming.
runUpdate :: Handle -> IO()
runUpdate handleUpdate = do
  putStrLn "waiting for updates"
  myloop
  where
    myloop = do
        update <- hGetLine handleUpdate
        --hPutStrLn handleUpdate $ "Hi, sending update"
        putStrLn $ "Update: " ++ update
        case words update of
          ["UPDATE", fileName] -> do
            hPutStrLn handleUpdate  $ "READY " ++ fileName
            updateFile fileName handleUpdate
          _ -> do 
            putStrLn "Not UPDATE as expected"
            myloop

--Reads updated file (replaces entire contents) from fileserver.
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

--Tell locking server i am done with the file so thay can unlock it.
lockFile :: String -> IO()
lockFile fileName = do 
  putStrLn "telling lock server im done with file"
  handle <- connectTo "localhost" (PortNumber (fromIntegral portNumLocking))
  hPutStrLn handle $ "LOCK " ++ fileName