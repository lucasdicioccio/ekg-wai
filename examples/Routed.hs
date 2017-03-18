{-# LANGUAGE OverloadedStrings #-}

-- | Example program that continously computes the mean of a list of
-- numbers.
module Main where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Exception
import Data.Int (Int64)
import Data.List
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wai.Handler.Warp (run)
import Network.Wai
import Network.Wai.UrlMap
import Network.HTTP.Types
import qualified System.Metrics as Metrics
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Label as Label
import System.Remote.Monitoring.Wai (monitor)

-- 'sum' is using a non-strict lazy fold and will blow the stack.
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

mean :: Fractional a => [a] -> a
mean xs = sum' xs / fromIntegral (length xs)

monitorApp :: IO (Application, Metrics.Store)
monitorApp = do
    store <- Metrics.newStore
    Metrics.registerGcMetrics store
    Metrics.registerCounter "ekg.server_timestamp_ms" getTimeMs store
    return (monitor store, store)
  where
    getTimeMs :: IO Int64
    getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

helloApp :: Counter.Counter -> Application
helloApp counter _ respond = do
    putStrLn "I've done some IO here"
    Counter.inc counter
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

main :: IO ()
main = do
    (ekg, store) <- monitorApp
    reqCount <- Metrics.createCounter "http.request_count" store
    let hello = helloApp reqCount
    let app = mapUrls (mount "hello" hello <|> mountRoot ekg)
    
    counter <- Metrics.createCounter "iterations" store
    label <- Metrics.createLabel "args" store
    event <- Metrics.createDistribution "runtime" store
    Label.set label "some text string"
    let loop n = do
            t <- timed $ evaluate $ mean [1..n]
            Distribution.add event t
            threadDelay 2000
            Counter.inc counter
            loop n
    _ <- forkIO $ loop 1000000
    run 8000 app

timed :: IO a -> IO Double
timed m = do
    start <- getTime
    m
    end <- getTime
    return $! end - start

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime
