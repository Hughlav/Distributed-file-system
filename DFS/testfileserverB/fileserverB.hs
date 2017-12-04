import FileServer

portNum :: Int
portNum = 5601

main :: IO()
main = do 
  openSocket portNum