{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Happer.Types where

-- Support for generic programming of data structures. Used by persistence and
-- serialization libraries. This requires the two language options above.
import Data.Data

-- A trace is a root level span. For all intents and purposes it can be treated
-- as a regular span.
type Trace = Span

-- Timestamps are milliseconds since epoch.
type Timestamp = Integer

-- 64-bit integer as recommended by the Dapper paper
newtype SpanId = SpanId { unSpanId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, Show)

-- A Span is the core data structure of Happer, representing a named interval
-- of time, typically a request, method call, or other discrete unit of work.
-- Spans can be arbitrarily nested to allow drill-down into the details of any
-- particular action.
data Span = Span {
                 -- Identifier unique across all spans
                   spanId    :: SpanId

                 -- Traces with the same name can be grouped together, but on
                 -- individual spans the name is only a convenient label for
                 -- the UI.
                 , name      :: String
                 , startTime :: Timestamp
                 , endTime   :: Timestamp

                 -- Child spans that were generated in the creation of this
                 -- one.
                 , children :: [Span]
                 }
                 deriving (Ord, Eq, Data, Typeable, Show)


-- A span entering the system, containing metadata about where it should be
-- filed. This is a transport data structure which should not be persisted. It
-- is short lived and only exists until it can be converted to a span in the
-- correct trace.
data FreeSpan = FreeSpan { traceId  :: SpanId
                         , parentId :: SpanId
                         , span     :: Span
                         }
                         deriving (Ord, Eq, Data, Typeable, Show)