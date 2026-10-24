-- Reacting to commits.
--
-- A subscriber is a thread that blocks until the basis
-- moves, then catches up from the log... Writers never wait on subscribers and
-- a slow callback only holds up its own thread
module Datomlite.Watch
  ( listen
  , watch
  ) where

import Control.Concurrent     (forkIO)
import Control.Concurrent.STM (atomically, check, newTVarIO, orElse, readTVar,
                               readTVarIO, writeTVar)
import Control.Exception      (SomeAsyncException, SomeException, catch,
                               displayException, evaluate, fromException, throwIO)
import Control.Monad          (forM_, unless, when)
import qualified Data.ByteString    as BS
import Data.IORef             (newIORef, readIORef, writeIORef)
import Data.Maybe             (isJust)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import System.IO              (stderr)

import Datomlite.Core

-- | Call @f@ with every commit from now on, in tx order.
--
-- One 'TxReport' per commit even when several land at once. @f@ runs on its
-- own thread so it can read or commit. If it throws the error goes to stderr
-- and the listener keeps going.
--
-- Returns the action that stops it. Safe to call twice or from inside @f@.
--
-- @
-- stop <- listen conn $ \\r -> putStrLn (show (length (trDatoms r)) <> " datoms")
-- @
listen :: Conn -> (TxReport -> IO ()) -> IO (IO ())
listen conn f = subscribe conn (const []) (\seen db -> map f (txsAfter seen db))

-- | A live query. Calls @f@ with @q@'s result now... then again whenever it
-- changes.
--
-- @q@ runs on the latest 'Db' so if commits outpace @f@ it just skips to the
-- current answer. Errors are handled as in 'listen'.
--
-- @
-- stop <- watch conn (length . every \@Employee) $ \\n -> putStrLn (show n <> " employees")
-- @
watch :: Eq a => Conn -> (Db -> a) -> (a -> IO ()) -> IO (IO ())
watch conn q f = do
  lastSeen <- newIORef Nothing   -- only the watch's own thread touches it
  let run db = [ do r    <- evaluate (q db)
                    prev <- readIORef lastSeen
                    when (prev /= Just r) (writeIORef lastSeen (Just r) >> f r) ]
  subscribe conn run (const run)

-- Fork a thread that runs @start@ on the current 'Db', then @step@ on each
-- newer one... until stopped. The start is read before returning... so no later
-- commit is missed
subscribe :: Conn -> (Db -> [IO ()]) -> (TxId -> Db -> [IO ()]) -> IO (IO ())
subscribe conn start step = do
  stop <- newTVarIO False
  db0  <- snapshot conn
  let deliver = mapM_ $ \act -> do
        stopped <- readTVarIO stop
        unless stopped (guarded act)
      loop seen = do
        next <- atomically $ (Nothing <$ (readTVar stop >>= check))
                    `orElse` (Just <$> awaitAfter conn seen)
        forM_ next $ \db -> deliver (step seen db) >> loop (basisT db)
  _ <- forkIO (deliver (start db0) >> loop (basisT db0))
  pure (atomically (writeTVar stop True))

-- Report and swallow a callback's exception. Async exceptions still end the
-- thread. One write... so it can't interleave with other output on stderr
guarded :: IO () -> IO ()
guarded act = act `catch` \e ->
  if isAsync e then throwIO e
  else BS.hPut stderr . T.encodeUtf8 . T.pack $
         "datomlite: subscriber threw: " <> displayException e <> "\n"
  where
    isAsync :: SomeException -> Bool
    isAsync e = isJust (fromException e :: Maybe SomeAsyncException)
