import FileServer

portNum :: Int
portNum = 7431


main :: IO()
main = do 
  openSocket portNum 