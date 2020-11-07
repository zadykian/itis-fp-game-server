module Main where

import HttpApi

import Options
import Control.Lens.Lens ((&))
import System.IO

import Servant
import Server
import Network.Wai.Handler.Warp
import ServerOptions
import Data.Streaming.Network.Internal (HostPreference (Host))

main :: IO ()
main = runCommand $ \options _ -> do
    putStrLn $ "Running API on " ++ show options
    hFlush stdout

    let warpSettings = cliOptionsToWarpSettings options
    let application = serve (Proxy :: Proxy HttpApiWithSwagger) httpServer
    runSettings warpSettings application

{-|
    Преобразовать параметры CLI в настройки warp.
-}
cliOptionsToWarpSettings :: ServerOptions -> Settings
cliOptionsToWarpSettings options
    = defaultSettings
        & setHost (Host $ options & host)
        & setPort (options & port)
        & setTimeout (options & timeout)