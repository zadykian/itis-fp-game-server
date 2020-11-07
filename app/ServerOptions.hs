module ServerOptions where

import Options
import Control.Lens.Lens ((&))

{-|
    Параметры CLI-конфигурации приложения.
-}
data ServerOptions = ServerOptions
    {
        host :: String,
        port :: Int,
        timeout :: Int
    }

instance Options ServerOptions where
    defineOptions = pure ServerOptions
        <*> simpleOption "host" "127.0.0.1" "API host address."
        <*> simpleOption "port" 8000 "API port."
        <*> simpleOption "timeout" 60 "API timeout in seconds."
        
instance Show ServerOptions where
    show options = "Running API on " ++ (options & host) ++ ":" ++ show (options & port)