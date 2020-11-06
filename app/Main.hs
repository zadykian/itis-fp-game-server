module Main where

import Servant
import HttpApi
import Server
import Network.Wai.Handler.Warp (run)

application :: Application
application = serve httpApiWithSwaggerProxy httpServer

main :: IO ()
main = run 8000 application