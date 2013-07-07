{-# LANGUAGE  OverloadedStrings #-}

module Main where

import Happer.Types
import Happer.Persistence
import Happer.Json
import Happer.Splices

import Happstack.Server
import Happstack.Server.Heist (heistServe, initHeistCompiled)

import Control.Monad          ( msum )
import Control.Monad.IO.Class ( liftIO )

-- TODO: push down into Happer.Json
import Data.Aeson         ( encode, eitherDecode, object, (.=) )

-- TODO: Move to helpers
getBody = do
  req <- askRq
  body <- liftIO $ takeRequestBody req
  case body of
    Just rqbody -> return (unBody rqbody)
    Nothing     -> return ""

putTrace :: Datastore -> Integer -> ServerPart Response
putTrace datastore id =
    do body <- getBody
       case eitherDecode body of
         Left err    -> badRequest $ toResponse $
                        encode (object [ "error" .= err ])
         Right trace -> do _ <- runUpdate datastore (AddTrace trace)
                           ok $ toResponse $ encode trace

getTrace :: Datastore -> Integer -> ServerPart Response
getTrace datastore id =
  do trace <- runQuery datastore $ TraceById (SpanId id)
     case trace of
       Just x  -> ok       $ toResponse $ encode trace
       Nothing -> notFound $ toResponse $ encode (object [] )

postSpans :: Datastore -> ServerPart Response
postSpans datastore =
  do body <- getBody
     case eitherDecode body of
       Left err    -> badRequest $ toResponse $
                      encode (object [ "error" .= err ])
       Right spans -> do _ <- runUpdate datastore (AddSpans spans)
                         ok $ toResponse $ encode (object [])

handlers datastore = do
  heistState <- do
      r <- initHeistCompiled (happerSplices datastore) [] "app/views"
      case r of
        Left e      -> error $ unlines e
        Right state -> return state
  msum [ dir "trace" $ method PUT >> path (putTrace datastore)
       , dir "trace" $ method GET >> path (getTrace datastore)
       , dir "spans" $ postSpans datastore
       , heistServe heistState
       , nullDir >> seeOther ("/index" :: String) (toResponse ())
       ]

main :: IO ()
main = do
  withDatastore (simpleHTTP nullConf . handlers)
