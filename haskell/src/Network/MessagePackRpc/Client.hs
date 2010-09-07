{-# Language DeriveDataTypeable #-}

module Network.MessagePackRpc.Client (
  Connection,
  connect,
  
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

data Connection
  = Connection
    { connHandle :: Handle }
    
connect :: String -> Int -> IO Connection
connect addr port = withSocketsDo $ do
  h <- connectTo addr (PortNumber $ fromIntegral port)
  return $ Connection
    { connHandle = h
    }

data RpcError
  = ServerError Object
  | ResultTypeError String
  | InvalidResponseType Int
  | InvalidMessageId Int Int
  deriving (Eq, Ord, Typeable)

instance Exception RpcError

instance Show RpcError where
  show (ServerError err) =
    "server error: " ++ show err
  show (ResultTypeError err) =
    "result type error: " ++ err
  show (InvalidResponseType rtype) =
    "response type is not 1 (got " ++ show rtype ++ ")"
  show (InvalidMessageId expect got) =
    "message id mismatch: expect " ++ show expect ++ ", but got " ++ show got

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
      throw $ InvalidResponseType rtype
    when (rmsgid /= msgid) $
      throw $ InvalidMessageId msgid rmsgid
    case fromObject rerror of
      Left _ ->
        throw $ ServerError rerror
      Right () ->
        return rresult

--

call :: RpcType a => Connection -> String -> a
call c m = rpcc c m []

method :: RpcType a => String -> Connection -> a
method c m = call m c

type RpcMethod a = Connection -> a
