module Lib
    ( someFunc
    ) where

import           ClassyPrelude

import           Control.Arrow
import           Control.Arrow.Machine

someFunc :: IO ()
someFunc = do
  putStrLn "start"
  go
  putStrLn "finish"

go :: IO ()
go = runT_ (source' >>> pipe' >>> sink') (repeat ())


source' :: ProcessT IO (Event ()) (Event Int)
source' = repeatedly $ do
  _ <- await
  yield 1

pipe' :: ProcessT IO (Event Int) (Event Int)
pipe' = construct $ do
  s1 <- await
  s2 <- await
  yield $ s1 + s2

sink' :: ProcessT IO (Event Int) (Event ())
sink' = repeatedlyT plan

plan :: MonadIO m => PlanT Int () m ()
plan = do
  x <- await
  liftIO $ print x
