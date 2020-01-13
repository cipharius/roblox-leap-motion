{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where
    
import Data.Default
import Data.ByteString.Lazy.Internal
import Data.Aeson hiding (json)

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM

import System.Hardware.Leap

import Network.Wai
import Network.WebSockets
import Network.Wai.Middleware.Local
import Network.HTTP.Types.Status
import Web.Spock
import Web.Spock.Config

data AppSession = EmptySession

main :: IO ()
main = do
    ref <- atomically $ newTVar [] :: IO (TVar [Value])
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase ref
    _ <- runLeapWorker ref
    runSpock 8080 (spock spockCfg app)

app :: SpockM () AppSession (TVar [Value]) ()
app = do
    middleware $ local $ responseLBS status403 [] "Local requests only"
    get root $ do
        state <- getState
        evQueue <- liftIO $ popWhenReady state
        json $ toJSON evQueue

-- Will run out of memory if not read for too long
runLeapWorker :: TVar [Value] -> IO ThreadId
runLeapWorker ref = forkIO $ run def {host = "127.0.0.1"} $ \conn -> do
    mapM_ ($ conn) [setFocused True, setGestures True]
    forever $ do
        msg <- receiveData conn :: IO ByteString
        case (decode msg) of
            Just ev' -> atomically $ readTVar ref >>= (writeTVar ref).(ev':)
            Nothing -> return ()

popWhenReady :: TVar [Value] -> IO [Value]
popWhenReady x = atomically $ do
    current <- readTVar x
    check $ (/=[]) current
    writeTVar x []
    return current
