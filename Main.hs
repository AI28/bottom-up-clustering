{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Pure
import qualified Data.Vector as V
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    csvData <- BL.readFile $ head args
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> case last args of
                        "single_linkage" -> print $ solve single_linkage $ V.foldr (\x y -> V.toList x : y) [] v
                        "complete_linkage" -> print $ solve complete_linkage $ V.foldr (\x y -> V.toList x : y) [] v
                        _ -> putStrLn "Format: bottom-up.exe <data-set> (single_linkage|complete_linkage)"
