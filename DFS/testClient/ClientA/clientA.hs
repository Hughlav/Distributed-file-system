
import Client

portNum :: Int --for directory service
portNum = 6626

clientName :: String
clientName = "clientA"

portUpdate :: Int
portUpdate = 99960

main :: IO()
main = do 
  openSocket portNum clientName portUpdate