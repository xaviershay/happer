module Main where

import Happstack.Server

main :: IO ()
main = simpleHTTP nullConf $ ok "Hello, World!"
