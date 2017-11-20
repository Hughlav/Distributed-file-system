module Main where
    import System.IO
    import Control.Exception
    import Control.Concurrent 
    import Control.Monad (when, forever)
    import Control.Monad.Fix (fix)
    import Network (PortID(..), accept, listenOn, withSocketsDo, Socket,connectTo)
    import Control.Concurrent.STM
    import qualified Data.Map as M
    import Data.Map (Map)
    
    type Msg = (Int, String)
    type Server = TVar (Map Int Room)
    
    type RoomName = String
    data Room = Room 
      { roomName :: RoomName
      }
    
    
    newServer :: IO Server
    newServer = newTVarIO M.empty
    
    portNum :: Int
    portNum = 3454
    
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
          putStrLn "Server says:"
          msg <- hGetLine handle
          putStrLn msg

          
        
        
    
    openSocket :: IO()
    openSocket = withSocketsDo $ do
      handle <- connectTo "localhost" (PortNumber (fromIntegral portNum))
      
      putStrLn "Connected to server\n"
      runConn handle
      
     
        