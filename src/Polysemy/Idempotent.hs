{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Idempotent where
  -- ( Idempotent
  -- , idempotently
  -- , Idempotency ()
  -- , newIdempotency
  -- , runIdempotentInIO
  -- ) where

import           Control.Exception (evaluate)
import           Data.Dynamic
import           Data.IORef
import qualified Data.IntMap as M
import           Data.Maybe
import           Polysemy
import           System.Mem.StableName

data Idempotent m a where
  Idempotently :: Typeable a => key -> m a -> Idempotent m a

makeSem ''Idempotent

data Idempotency = Idempotency (IORef (M.IntMap Dynamic))

newIdempotency :: IO Idempotency
newIdempotency = Idempotency <$> newIORef M.empty


makeStableKey :: a -> IO Int
makeStableKey a = do
    st <- makeStableName a
    return $ hashStableName st


runIdempotentInIO
    :: Member (Lift IO) r
    => Idempotency
    -> Sem (Idempotent ': r) a
    -> Sem r a
runIdempotentInIO idem@(Idempotency ref) = interpretH $ \case
  Idempotently k m -> do
    inspect' <- inspect <$> getInspectorT
    (key, v) <- sendM $ (,) <$> makeStableKey k
                            <*> readIORef ref

    case M.lookup key v of
      Nothing -> do
        m' <- runT m
        fa <- raise $ runIdempotentInIO idem m'
        a <- sendM $ evaluate $ fromMaybe (error "non-pure idempotency block") $ inspect' fa
        sendM . modifyIORef ref $ M.insert key $ toDyn a
        pure fa
      Just a -> pureT $ fromMaybe (error "idempotency key collision") $ fromDynamic a



test :: IO ()
test = do
  let mykey = "hello"
  idem <- newIdempotency
  runM $ runIdempotentInIO idem $ do
    idempotently mykey $ do
      sendM $ putStrLn "only one time!"
    idempotently mykey $ do
      sendM $ putStrLn "only one time!"

-- $> test


