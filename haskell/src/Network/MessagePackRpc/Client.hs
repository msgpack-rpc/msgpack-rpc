module Network.MessagePackRpc.Client (
  Connection,
  connect,
  
  RpcMethod,
  call,
  method,
  ) where

import Control.Monad
import Data.Functor
import Data.MessagePack
import Network
import System.IO
import System.Random
import Text.Printf

data Connection
  = Connection
    { connHandle :: Handle }
    
connect :: String -> Int -> IO Connection
connect addr port = withSocketsDo $ do
  h <- connectTo addr (PortNumber $ fromIntegral port)
  return $ Connection
    { connHandle = h
    }

class RpcType r where
  rpcc :: Connection -> String -> [Object] -> r

fromObject' :: OBJECT o => Object -> IO o
fromObject' o =
  case fromObject o of
    Left err -> fail err
    Right r -> return r

instance OBJECT o => RpcType (IO o) where
  rpcc c m args = fromObject' =<< rpcCall c m (reverse args)

instance (OBJECT o, RpcType r) => RpcType (o -> r) where
  rpcc c m args arg = rpcc c m (toObject arg:args)

rpcCall :: Connection -> String -> [Object] -> IO Object
rpcCall Connection{ connHandle = h } m args = do
  msgid <- (`mod`2^(32::Int)) <$> randomIO :: IO Int
  packToHandle h $ put (0 ::Int, msgid, m, args)
  hFlush h
  unpackFromHandleI h $ do
    (rtype, rmsgid, rerror, rresult) <- getI
    when (rtype /= (1 :: Int)) $
      fail "response type is not 1"
    when (rmsgid /= msgid) $
      fail $ printf "msgid mismatch: expect %d but got %d" msgid rmsgid
    case fromObject rerror of
      Left _ ->
        fail $ printf "server error: %s" (show rerror)
      Right () ->
        return rresult

--

call :: RpcType a => Connection -> String -> a
call c m = rpcc c m []

method :: RpcType a => String -> Connection -> a
method c m = call m c

type RpcMethod a = Connection -> a
