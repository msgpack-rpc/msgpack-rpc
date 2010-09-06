module Network.MessagePackRpc.Server (
  RpcMethod,
  fun,
  serve,
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.MessagePack
import Network
import System.IO
import Text.Printf

import Prelude hiding (catch)

type RpcMethod = [Object] -> IO Object

class RpcMethodType f where
  toRpcMethod :: f -> RpcMethod

instance OBJECT o => RpcMethodType (IO o) where
  toRpcMethod m = \[] -> toObject <$> m

instance (OBJECT o, RpcMethodType r) => RpcMethodType (o -> r) where
  toRpcMethod f = \(x:xs) -> toRpcMethod (f (fromObject' x)) xs

fromObject' :: OBJECT o => Object -> o
fromObject' o = let Right r = fromObject o in r

--

fun :: RpcMethodType f => f -> RpcMethod
fun = toRpcMethod

serve :: Int -> [(String, RpcMethod)] -> IO ()
serve port methods = withSocketsDo $ do
  sock <- listenOn (PortNumber $ fromIntegral port)
  forever $ do
    (h, host, hostport) <- accept sock
    forkIO $
      (processRequests h >> print "owata") `finally` hClose h
      `catch` \(SomeException e) -> printf "%s:%s: %s" host (show hostport) (show e)
  
  where
    processRequests h = unpackFromHandleI h $ do
      forever $ do
        processRequest h
        liftIO $ print "done."
    
    processRequest h = do
      liftIO $ print "reading req"
      (rtype, msgid, method, args) <- getI
      when (rtype /= (0 :: Int)) $
        fail "request type is not 0"
      
      liftIO $ do
        print (msgid, method, args)
        ret <- callMethod (method :: String) (args :: [Object])
        packToHandle h $ put (1 :: Int, msgid :: Int, (), ret)
        hFlush h
    
    callMethod methodName args = do
      let method = fromJust $ lookup methodName methods
      method args
