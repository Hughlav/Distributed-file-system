
import Client

portNum :: Int
portNum = 6626

clientName :: String
clientName = "clientB"

portUpdate :: Int
portUpdate = 4008

main :: IO()
main = do 
  openSocket portNum clientName portUpdate