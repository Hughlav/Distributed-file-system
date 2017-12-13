import FileServer

portNum :: Int
portNum = 9471


main :: IO()
main = do 
  openSocket portNum 