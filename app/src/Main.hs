{-# LANGUAGE  OverloadedStrings #-}

module Main where

import Happer.Types
import Happer.Persistence
import Happer.Json
import Happer.Templates

import Happstack.Server
import Happstack.Helpers

import Control.Monad ( msum )

main :: IO ()
main = do
  withDatastore (simpleHTTP nullConf . handlers)

handlers datastore = do
  viewHandler <- do
    r <- mkViewHandler datastore "app/views"
    case r of
      Left e      -> error $ unlines e
      Right state -> return state
  msum [ dir "trace" $ method PUT >> path (putTrace  datastore)
       , dir "trace" $ method GET >> path (getTrace  datastore)
       , dir "spans" $                     postSpans datastore
       , viewServe viewHandler -- TODO: Collapse these two functions into one.
       , nullDir >> seeOther ("/index" :: String) (toResponse ())
       ]

putTrace :: Datastore -> Integer -> ServerPart Response
putTrace datastore id =
    do body <- getBody
       case fromJson body of
         Left err    -> badRequest $ toResponse $
                        toJson (RequestError err)
         Right trace -> do _ <- runUpdate datastore (AddTrace trace)
                           ok $ toResponse $ toJson trace

getTrace :: Datastore -> Integer -> ServerPart Response
getTrace datastore id =
  do trace <- runQuery datastore $ TraceById (SpanId id)
     case trace of
       Just x  -> ok       $ toResponse $ toJson trace
       Nothing -> notFound $ toResponse $ toJson RequestSuccess

postSpans :: Datastore -> ServerPart Response
postSpans datastore =
  do body <- getBody
     case fromJson body of
       Left err    -> badRequest $ toResponse $
                      toJson (RequestError err)
       Right spans -> do _ <- runUpdate datastore (AddSpans spans)
                         ok $ toResponse $ toJson RequestSuccess
