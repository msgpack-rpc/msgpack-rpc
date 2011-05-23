-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Server
-- Copyright : (c) Hideyuki Tanaka, 2010
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- This module is server library of MessagePack-RPC.
-- The specification of MessagePack-RPC is at <http://redmine.msgpack.org/projects/msgpack/wiki/RPCProtocolSpec>.
--
-- A simple example:
--
-- >import Network.MessagePackRpc.Server
-- >
-- >add :: Int -> Int -> IO Int
-- >add x y = return $ x + y
-- >
-- >main =
-- >  serve 1234 [("add", fun add)]
--
--------------------------------------------------------------------

module Network.MessagePackRpc.Server (
  -- * RPC method types
  RpcMethod,
  RpcMethodType(..),
  -- * Create RPC method
  fun,
  -- * Start RPC server
  serve,
  ) where

import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import Control.Exception as E
import Data.Enumerator
import Data.Enumerator.Binary
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.Enumerator
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.MessagePack
import Network
import System.IO

import Prelude hiding (catch)

bufferSize :: Integer
bufferSize = 4096

type RpcMethod = [Object] -> IO Object

class RpcMethodType f where
  toRpcMethod :: f -> RpcMethod

instance OBJECT o => RpcMethodType (IO o) where
  toRpcMethod m = \[] -> toObject <$> m

instance (OBJECT o, RpcMethodType r) => RpcMethodType (o -> r) where
  toRpcMethod f = \(x:xs) -> toRpcMethod (f $! fromObject' x) xs

fromObject' :: OBJECT o => Object -> o
fromObject' o =
  case tryFromObject o of
    Left err -> error $ "argument type error: " ++ err
    Right r -> r

--

-- | Create a RPC method from a Haskell function.
fun :: RpcMethodType f => f -> RpcMethod
fun = toRpcMethod

-- | Start RPC server with a set of RPC methods.
serve :: Int -- ^ Port number
         -> [(String, RpcMethod)] -- ^ list of (method name, RPC method)
         -> IO ()
serve port methods = withSocketsDo $ do
  sock <- listenOn (PortNumber $ fromIntegral port)
  forever $ do
    (h, host, hostport) <- accept sock
    forkIO $
      (processRequests h `finally` hClose h) `catches`
      [ Handler $ \e ->
         case e of
           ParseError ["demandInput"] _ -> return ()
           _ -> hPutStrLn stderr $ host ++ ":" ++ show hostport ++ ": " ++ show e
      , Handler $ \e ->
         hPutStrLn stderr $ host ++ ":" ++ show hostport ++ ": " ++ show (e :: SomeException)]

  where
    processRequests h =
      run_ $ enumHandle bufferSize h $$ forever $ processRequest h
    
    processRequest h = do
      (rtype, msgid, method, args) <- iterParser get
      liftIO $ do
        resp <- try $ getResponse rtype method args
        case resp of
          Left err ->
            BL.hPutStr h $ pack (1 :: Int, msgid :: Int, show (err :: SomeException), ())
          Right ret ->
            BL.hPutStr h $ pack (1 :: Int, msgid :: Int, (), ret)
        hFlush h

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
