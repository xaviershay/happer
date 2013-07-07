{-# LANGUAGE NoMonomorphismRestriction #-}

module Happer.Templates ( mkViewHandler, viewServe ) where

import Happer.Types
import Happer.Persistence

import Happstack.Server.Heist (heistServe, initHeistCompiled)
import Happstack.Server

import Heist.Compiled         (Splice, yieldRuntimeText)
import qualified Text.XmlHtml as X
import qualified Data.Text    as T

traceCountSplice :: (Monad m) => Datastore -> Splice m
traceCountSplice datastore = do
    count <- runQuery datastore TraceCount
    let res = yieldRuntimeText $ do
                                 return $ T.pack $ show count
    return res

happerSplices datastore = [(T.pack "trace-count", traceCountSplice datastore)]

mkViewHandler datastore templateDir =
  initHeistCompiled (happerSplices datastore) [] templateDir

viewServe = heistServe
