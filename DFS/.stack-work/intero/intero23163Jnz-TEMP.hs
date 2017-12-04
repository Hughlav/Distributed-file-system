
import Client

portNum :: Int
portNum = 8910

main :: IO()
main = do
  openSocket portNum
  