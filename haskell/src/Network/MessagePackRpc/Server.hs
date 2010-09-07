module Network.MessagePackRpc.Server (
  RpcMethod,
  fun,
  serve,
  ) where

import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Data.Iteratee.Exception as I
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
  toRpcMethod f = \(x:xs) -> toRpcMethod (f $! fromObject' x) xs

fromObject' :: OBJECT o => Object -> o
fromObject' o =
  case fromObject o of
    Left err -> error $ "argument type error: " ++ err
    Right r -> r

--

fun :: RpcMethodType f => f -> RpcMethod
fun = toRpcMethod

serve :: Int -> [(String, RpcMethod)] -> IO ()
serve port methods = withSocketsDo $ do
  sock <- listenOn (PortNumber $ fromIntegral port)
  forever $ do
    (h, host, hostport) <- accept sock
    forkIO $
      (processRequests h `finally` hClose h) `catches`
      [ Handler $ \e -> let _ = (e :: I.EofException) in return ()
      , Handler $ \e -> hPutStrLn stderr $ host ++ ":" ++ show hostport ++ ": "++ show (e :: SomeException)]

  where
    processRequests h =
      unpackFromHandleI h $ forever $ processRequest h
    
    processRequest h = do
      (rtype, msgid, method, args) <- getI
      liftIO $ do
        resp <- try $ getResponse rtype method args
        case resp of
          Left err ->
            packToHandle' h $
            put (1 :: Int, msgid :: Int, show (err :: SomeException), ())
          Right ret ->
            packToHandle' h $
            put (1 :: Int, msgid :: Int, (), ret)

    getResponse rtype method args = do
      when (rtype /= (0 :: Int)) $
        fail "request type is not 0"
      
      r <- callMethod (method :: String) (args :: [Object])
      r `deepseq` return r
    
    callMethod methodName args =
      case lookup methodName methods of
        Nothing ->
          fail $ "method '" ++ methodName ++ "' not found"
        Just method ->
          method args
