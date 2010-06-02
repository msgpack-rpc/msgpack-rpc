module Network.MessagePackRpc.Server (
  RpcMethod,
  fun,
  serve,
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.MessagePack
import Network
import System.IO

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
    (h, host, port) <- accept sock
    forkIO $
      processRequests h `finally` hClose h
      `catch` \(SomeException e) -> print e
  
  where
    processRequests h =
      forever $ processRequest h
    
    processRequest h = do
      (msgid, method, args) <- unpackFromHandle h $ do
        [ rtype, rmsgid, rmethod, rargs ] <- get
        0      <- return (fromObject' rtype :: Int)
        msgid  <- return (fromObject' rmsgid :: Int)
        method <- return (fromObject' rmethod :: String)
        args   <- return (fromObject' rargs :: [Object])
        return (msgid, method, args)
      
      ret <- callMethod method args
      
      packToHandle h $ do
        put [ toObject (1 :: Int)
            , toObject msgid
            , toObject ()
            , ret
            ]
    
    callMethod methodName args = do
      let method = fromJust $ lookup methodName methods
      method args
