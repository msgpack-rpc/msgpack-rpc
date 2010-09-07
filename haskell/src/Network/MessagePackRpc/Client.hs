{-# Language DeriveDataTypeable #-}

-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Client
-- Copyright : (c) Hideyuki Tanaka, 2010
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- This module is client library of MessagePack-RPC.
-- The specification of MessagePack-RPC is at <http://redmine.msgpack.org/projects/msgpack/wiki/RPCProtocolSpec>.
--
-- A simple example:
--
-- >import Network.MessagePackRpc.Client
-- >
-- >add :: RpcMethod (Int -> Int -> IO Int)
-- >add = method "add"
-- >
-- >main = do
-- >  conn <- connect "127.0.0.1" 1234
-- >  print =<< add conn 123 456
--
--------------------------------------------------------------------

module Network.MessagePackRpc.Client (
  -- * RPC connection
  Connection,
  connect,
  
  -- * RPC error
  RpcError(..),
  
  -- * Call RPC method
  RpcMethod,
  call,
  method,
  ) where

import Control.Exception
import Control.Monad
import Data.Functor
import Data.MessagePack
import Data.Typeable
import Network
import System.IO
import System.Random

-- | RPC connection type
data Connection
  = Connection
    { connHandle :: Handle }

-- | Connect to RPC server
connect :: String -- ^ Host name
           -> Int -- ^ Port number
           -> IO Connection -- ^ Connection
connect addr port = withSocketsDo $ do
  h <- connectTo addr (PortNumber $ fromIntegral port)
  return $ Connection
    { connHandle = h
    }

-- | RPC error type
data RpcError
  = ServerError Object -- ^ An error occurred at server
  | ResultTypeError String -- ^ Result type mismatch
  | ProtocolError String -- ^ A protocol error occurred
  deriving (Eq, Ord, Typeable)

instance Exception RpcError

instance Show RpcError where
  show (ServerError err) =
    "server error: " ++ show err
  show (ResultTypeError err) =
    "result type error: " ++ err
  show (ProtocolError err) =
    "protocol error: " ++ err

class RpcType r where
  rpcc :: Connection -> String -> [Object] -> r

fromObject' :: OBJECT o => Object -> o
fromObject' o =
  case fromObject o of
    Left err -> throw $ ResultTypeError err
    Right r -> r

instance OBJECT o => RpcType (IO o) where
  rpcc c m args = return . fromObject' =<< rpcCall c m (reverse args)

instance (OBJECT o, RpcType r) => RpcType (o -> r) where
  rpcc c m args arg = rpcc c m (toObject arg:args)

rpcCall :: Connection -> String -> [Object] -> IO Object
rpcCall Connection{ connHandle = h } m args = do
  msgid <- (`mod`2^(32::Int)) <$> randomIO :: IO Int
  packToHandle' h $ put (0 ::Int, msgid, m, args)
  unpackFromHandleI h $ do
    (rtype, rmsgid, rerror, rresult) <- getI
    when (rtype /= (1 :: Int)) $
      throw $ ProtocolError $ "response type is not 1 (got " ++ show rtype ++ ")"
    when (rmsgid /= msgid) $
      throw $ ProtocolError $ "message id mismatch: expect " ++ show msgid ++ ", but got " ++ show rmsgid
    case fromObject rerror of
      Left _ ->
        throw $ ServerError rerror
      Right () ->
        return rresult

--

-- | Call an RPC Method
call :: RpcType a =>
        Connection -- ^ Connection
        -> String -- ^ Method name
        -> a
call c m = rpcc c m []

-- | Create an RPC Method (call c m == method m c)
method :: RpcType a => String -> RpcMethod a
method c m = call m c

type RpcMethod a = Connection -> a
