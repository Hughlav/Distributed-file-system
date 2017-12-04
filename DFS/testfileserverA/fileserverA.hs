import FileServer

portNum :: Int
portNum = 5600

main :: IO()
main = do 
  openSocket portNum