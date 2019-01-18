{-# LANGUAGE GADTs, ScopedTypeVariables, RecordWildCards, TypeApplications, DuplicateRecordFields, RankNTypes #-}
module Pure.Fetch where

import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Async
import Pure.Cache
import Pure.Data.Try

import Control.Concurrent
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Typeable

data Fetcher key value = Fetcher
  { request :: (value -> IO ()) -> IO ()
  , cached  :: Bool
  , key     :: key
  , view    :: value -> View
  }

data Updater value update = Updater
  { every     :: Int
  , immediate :: Bool
  , request   :: (update -> IO ()) -> IO ()
  , update    :: value -> update -> value
  }

fetch :: forall key value update. (Caching, Eq key, Ord key, Typeable key, Typeable value) => Fetcher key value -> View
fetch Fetcher {..} = (if cached then (id :: (Caching => View) -> View) else caching) $
  let
    lookup :: Maybe (Maybe value)
    lookup = load key

    fetch | isJust lookup = return ()
          | otherwise = do
            seed key (Nothing :: Maybe value)
            request (store key . Just)

  in
    asyncOnceAs @(key,value) fetch
      (maybe Null view (join lookup))

fetchAndUpdate :: forall key value update. (Caching, Eq key, Ord key, Typeable key, Typeable value, Typeable update) => Fetcher key value -> Updater value update -> View
fetchAndUpdate Fetcher {..} Updater { request = updateRequest, ..} = (if cached then (id :: (Caching => View) -> View) else caching) $
  let
    lookup :: Maybe (Maybe value)
    lookup = load key

    fetch | isJust lookup = return ()
          | otherwise = do
            seed key (Nothing :: Maybe value)
            request (store key . Just)

    updating
      | immediate = updater >> forever (threadDelay every >> updater)
      | otherwise = forever (threadDelay every >> updater)
      where
        updater = do
          mv <- loadIO key
          for_ (join mv) $ \v ->
            updateRequest (store key . Just . update v)

  in
    asyncOnceAs @(key,value,update) (fetch >> updating)
        (maybe Null view (join lookup))
