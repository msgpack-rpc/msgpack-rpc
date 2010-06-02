import Network.MessagePackRpc.Client

import Control.Concurrent
import Control.Monad

add :: RpcMethod (Int -> Int -> IO Int)
add  = method "add"

echo :: RpcMethod (String -> IO String)
echo = method "echo"

main :: IO ()
main = do
  conn <- connect "127.0.0.1" 8081
  forM_ [0..10000] $ \i -> do
    print =<< add conn i (i*2)
    print =<< add conn (i*2) (i*3)
