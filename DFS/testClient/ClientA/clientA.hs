
import Client

portNum :: Int
portNum = 6725

clientName :: String
clientName = "clientA"

portUpdate :: Int
portUpdate = 6590

main :: IO()
main = do 
  openSocket portNum clientName portUpdate