module Main where

import qualified Lib

main :: IO ()
main = Lib.pipeline "/tmp/in.txt" 10 >>= Lib.printReport
