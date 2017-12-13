import FileServer

portNum :: Int
portNum = 17690


main :: IO()
main = do 
  openSocket portNum 