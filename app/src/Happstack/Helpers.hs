{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

-- Miscellaneous helpers that should probably be provided by happstack itself.
module Happstack.Helpers ( getBody ) where

import Happstack.Server

import Control.Monad.IO.Class ( liftIO )

-- Extract the HTTP body from a request.
getBody = do
  req  <- askRq
  body <- liftIO $ takeRequestBody req
  case body of
    Just rqbody -> return (unBody rqbody)
    Nothing     -> return ""
