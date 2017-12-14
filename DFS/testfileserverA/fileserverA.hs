import FileServer

portNum :: Int
portNum = 17790


main :: IO()
main = do 
  openSocket portNum 