{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

-- JSON converters for all basic Happer types.
module Happer.Json ( fromJson, toJson ) where

import Happer.Types

import Control.Applicative ( (<$>), (<*>), pure )

 -- a JSON library
import Data.Aeson
import Data.Attoparsec.Number ( Number(I) )

-- Alias aeson methods so as not to leak them into other modules.
fromJson = eitherDecode
toJson   = encode

instance FromJSON FreeSpan where
  parseJSON (Object v) = FreeSpan                        <$>
                         v .: "traceId"                  <*>
                         v .:? "parentId" .!= (SpanId 0) <*>
                         v .: "span"
  parseJSON _          = fail "Unknown error parsing new span"

instance FromJSON SpanId where
  parseJSON (Number (I x)) = pure $ SpanId x
  parseJSON _              = fail "Span ID is not an integer"

instance ToJSON SpanId where
  toJSON (SpanId x) = Number (I x)

instance FromJSON Span where
  parseJSON (Object v) = Span                    <$>
                         v .: "spanId"           <*>
                         v .: "name"             <*>
                         v .: "startTime"        <*>
                         v .: "endTime"          <*>
                         v .:? "children" .!= []

instance ToJSON Span where
  toJSON s = object [ "spanId"    .= (spanId s)
                    , "name"      .= (name s)
                    , "startTime" .= (startTime s)
                    , "endTime"   .= (startTime s)
                    , "children"  .= (children s)
                    ]

instance ToJSON RequestError where
  toJSON (RequestError err) = object [ "err" .= err ]

instance ToJSON RequestSuccess where
  toJSON RequestSuccess = object [ ]
