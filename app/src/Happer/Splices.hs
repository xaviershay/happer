module Happer.Splices where

import Happer.Types
import Happer.Persistence

-- TODO: push these down into persistence layer?
import Data.Acid          ( AcidState )
import Data.Acid.Advanced ( query', update' )

import Heist.Compiled         (Splice, yieldRuntimeText)
import qualified Text.XmlHtml as X
import qualified Data.Text    as T

traceCountSplice :: (Monad m) => AcidState HapperState -> Splice m
traceCountSplice datastore = do
    count <- query' datastore TraceCount
    let res = yieldRuntimeText $ do
                                 return $ T.pack $ show count
    return res

happerSplices datastore = [(T.pack "trace-count", traceCountSplice datastore)]
