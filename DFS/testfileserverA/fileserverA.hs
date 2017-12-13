import FileServer

portNum :: Int
portNum = 7430


main :: IO()
main = do 
  openSocket portNum 