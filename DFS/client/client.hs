module Main where
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
    
    type Msg = (Int, String)
    type Server = TVar (Map Int Room)
    
    type RoomName = String
    data Room = Room 
      { roomName :: RoomName
      }
    
    clientName :: String
    clientName = "clientA"
    
    newServer :: IO Server
    newServer = newTVarIO M.empty
    
    portNum :: Int
    portNum = 8701
    
    main :: IO()
    main = do 
      openSocket
    
    runConn :: Handle -> IO ()
    runConn handle  = do
      hSetNewlineMode handle universalNewlineMode
      hSetBuffering handle NoBuffering
      readNxt
      return ()
      where
        readNxt = do 
          putStrLn "What should i do?"
          command <- getLine 
          
          case words command of
            ["Open", fileName] -> do
              readFileandCache handle fileName
              readNxt
            ["WriteNew", fileName, contents] -> do
              writeNewFileandCache handle fileName contents
              readNxt
            ["Version", fileName] -> do 
              let req =  "VER " ++ fileName
              hPutStrLn handle req
              readNxt
            _ -> do
              putStrLn $ "Do not recognise command: " ++ command
              readNxt

          
    
    openSocket :: IO()
    openSocket = withSocketsDo $ do
      handle <- connectTo "localhost" (PortNumber (fromIntegral portNum))
      
      putStrLn "Connected to server\n"
      runConn handle
      
     
    readFileandCache :: Handle -> String -> IO()
    readFileandCache handle fileName = do
      let request = "OPEN " ++ fileName
      hPutStrLn handle request
      putStrLn "Waiting for file"
      let fileStr = ""
      fileStr <- myLoop fileStr
      writeFile fileName fileStr
      where
          myLoop fileStr = do 
            file <- hGetLine handle
            if file == "!EOF!"
              then do
                putStrLn "File Chached!"
                return (fileStr)
              else do 
                myLoop (fileStr ++ "\n" ++ file)
    
    writeNewFileandCache :: Handle -> String -> String -> IO()
    writeNewFileandCache handle fileName contents = do 
      let request = "WRITE " ++ fileName ++ " " ++ contents ++ " " ++ clientName --contents with no space
      hPutStrLn handle request
      putStrLn $ "File: " ++ fileName ++ " sent to cache"
      writeFileandClose fileName contents
    
    
    writeFileandClose :: String -> String-> IO() 
    writeFileandClose fileName toWrite = do
      fileHandle <- openFile fileName WriteMode
      hPutStr fileHandle toWrite
      hClose fileHandle
      putStrLn "Written to file"
      