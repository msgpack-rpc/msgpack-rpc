import Network.MessagePackRpc.Server

add :: Int -> Int -> IO Int
add x y = return $ x+y

echo :: String -> IO String
echo s = return s

main :: IO ()
main = do
  serve 8081 [ ("add", fun add), ("echo", fun echo) ]
