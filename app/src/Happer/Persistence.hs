{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}

module Happer.Persistence ( withDatastore
                          , runQuery
                          , runUpdate
                          , Datastore
                          , TraceCount(..)
                          , AddTrace(..)
                          , TraceById(..)
                          , AddSpans(..)
) where

import Happer.Types
import qualified Happer.Types as HT

import Control.Exception    ( bracket )
import Control.Monad.List   ( guard )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )

import Data.Maybe         ( fromJust, isJust )
import Data.List          ( nub )
import Data.Map           ( Map, fromListWith, foldrWithKey, findWithDefault )
import Data.Acid          hiding ( runQuery )
import Data.Acid.Advanced ( query', update' )
import Data.Acid.Local    ( createCheckpointAndClose )
import Data.IxSet         ( (@=), getOne )
import qualified Data.IxSet as IxSet

import Control.Monad.IO.Class (MonadIO)

-- AcidState persists regular haskell data structures to disk with ACID
-- guarantees. It does have a remote server option which I need to investigate
-- more.
type Datastore = AcidState HapperState

-- All clients should wrap datastore access in this method to ensure correct
-- setup and teardown.
withDatastore f = bracket (openLocalState initialState)
                          (createCheckpointAndClose)
                          f

-- Alias acid-state query methods so we don't leak them into other modules.
-- Type signatures are unfortunately required by the compiler.
runQuery ::    (MonadIO m, QueryEvent event)
            => AcidState (EventState event)
            -> event
            -> m (EventResult event)
runQuery  = query'

runUpdate ::   (UpdateEvent event, MonadIO m)
             => AcidState (EventState event)
             -> event
             -> m (EventResult event)
runUpdate = update'

-- Helper function to add a list of new spans to a trace.
updateTrace :: Trace -> [FreeSpan] -> HapperState -> HapperState
updateTrace trace freeSpans state =
  state {
    traces = IxSet.updateIx (spanId trace)
                            (addSpansToTrace trace freeSpans)
                            (traces state)
  }

addSpansToTrace :: Span -> [FreeSpan] -> Span
addSpansToTrace trace freeSpans =
  addChildSpans spanMap trace
  where
    spanMap :: Map SpanId [Span]
    spanMap = fromListWith (++) [(parentId x, [HT.span x]) | x <- freeSpans]

    -- Recursive rebuilds a span heirachy, adding new spans from the map.
    addChildSpans :: Map SpanId [Span] -> Span -> Span
    addChildSpans spanMap span =
      span {
        children = nub $ map (addChildSpans spanMap)
                               (findWithDefault [] (spanId span) spanMap) ++
                               (children span)
      }

traceById :: SpanId -> Query HapperState (Maybe Trace)
traceById id =
  do state <- ask
     return $ getOne $ traces state @= id

traceCount :: Query HapperState Int
traceCount = do
    state <- ask
    return $ IxSet.size (traces state)

addTrace :: Trace -> Update HapperState Trace
addTrace newTrace =
  do state <- get
     put $ state { traces = IxSet.insert newTrace (traces state) }
     return newTrace

-- Inserts a list of unsorted free spans into their correctly nested location
-- in the world. Free spans with invalid parentIds are currently ignored.
addSpans :: [FreeSpan] -> Update HapperState [FreeSpan]
addSpans spans =
  do state <- get
     put $ foldrWithKey updateTrace state (spansByTrace state)
     return []
  where
    spansByTrace :: HapperState -> Map Trace [FreeSpan]
    spansByTrace state = fromListWith (++) $ do
      x <- spans
      let mk = getOne $ traces state @= traceId x
      guard  (isJust mk)
      return (fromJust mk, [x])

$(makeAcidic ''HapperState ['addTrace, 'traceById, 'addSpans, 'traceCount])
