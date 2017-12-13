
import Client

portNum :: Int
portNum = 6725

clientName :: String
clientName = "clientB"

portUpdate :: Int
portUpdate = 6516

main :: IO()
main = do 
  openSocket portNum clientName portUpdate