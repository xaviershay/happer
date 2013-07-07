module Main where

import Happstack.Server
import Happer.Types

main :: IO ()
main = simpleHTTP nullConf $ ok "Hello, World!"
