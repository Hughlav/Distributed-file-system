
import Client

portNum :: Int
portNum = 6626

clientName :: String
clientName = "clientB"

portUpdate :: Int
portUpdate = 9908

main :: IO()
main = do 
  openSocket portNum clientName portUpdate