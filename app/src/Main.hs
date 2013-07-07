module Main where

import Happstack.Server
import Happer.Types
import Happer.Persistence
import Happer.Json

main :: IO ()
main = simpleHTTP nullConf $ ok "Hello, World!"
