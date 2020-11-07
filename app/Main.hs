module Main where

import Servant
import HttpApi
import Server
import Network.Wai.Handler.Warp
import ServerOptions
import Options
import Control.Lens.Lens ((&))

main :: IO ()
main = runCommand $ \options _ -> do
    print options
    let warpSettings = cliOptionsToWarpSettings options
    let application = serve httpApiWithSwaggerProxy httpServer  
    runSettings warpSettings application

{-|
    Преобразовать параметры CLI в настройки warp.
-}
cliOptionsToWarpSettings :: ServerOptions -> Settings
cliOptionsToWarpSettings options
    = defaultSettings
        & setHost (options & host)
        & setPort (options & port)
        & setTimeout (options & timeout)