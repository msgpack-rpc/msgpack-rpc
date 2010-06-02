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

fromObject' :: OBJECT o => Object -> o
fromObject' o = let Right r = fromObject o in r

instance OBJECT o => RpcType (IO o) where
  rpcc c m args = fromObject' <$> rpcCall c m (reverse args)

instance (OBJECT o, RpcType r) => RpcType (o -> r) where
  rpcc c m args arg = rpcc c m (toObject arg:args)

rpcCall :: Connection -> String -> [Object] -> IO Object
rpcCall Connection{ connHandle = h } m args = do
  msgid <- (`mod`2^(32::Int)) <$> randomIO :: IO Int
  packToHandle h $ do
    put [ toObject (0 :: Int)
        , toObject msgid
        , toObject m
        , toObject args
        ]
  unpackFromHandle h $ do
    [ rtype, rmsgid, rerror, rresult ] <- get
    Right 1 <- return (fromObject rtype :: Result Int)
    Right rmsgid <- return $ fromObject rmsgid
    when (rmsgid /= msgid) $ fail $ "msgid mismatch: " ++ show msgid ++ " <-> " ++ show rmsgid
    Right () <- return $ fromObject rerror
    Right rresult <- return $ fromObject rresult
    return rresult

--

call :: RpcType a => Connection -> String -> a
call c m = rpcc c m []

method :: RpcType a => String -> Connection -> a
method c m = call m c

type RpcMethod a = Connection -> a
