
import Client

portNum :: Int
portNum = 9111

clientName :: String
clientName = "clientA"

main :: IO()
main = do 
  openSocket portNum clientName