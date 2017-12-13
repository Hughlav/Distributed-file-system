import FileServer

portNum :: Int
portNum = 9470


main :: IO()
main = do 
  openSocket portNum 