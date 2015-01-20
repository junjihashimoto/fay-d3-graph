{-# LANGUAGE EmptyDataDecls    #-}

module Main where

import Graph

main :: Fay ()
main = ready $ do
  withCSV "data.csv" "id" "val" $ \dat -> do
    g <- graph 640 480 dat 35
    appendPoint g
    appendLine g
    appendAxis g
    appendLabel g "hogex" "hogey"
  return ()
