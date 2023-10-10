{-
-- EPITECH PROJECT, 2023
-- ImageCompressor
-- File description:
-- Main
-}

module Main (main) where

import Arguments
import Calculs
import System.Environment
import System.Exit (exitWith, ExitCode(..))

main :: IO ()
main = getArgs >>= \args ->
    case getOpts defaultConf args 0 of
        Nothing -> printUsage >> exitWith (ExitFailure 84)
        Just conf -> checkOpts conf >>= \handle -> case handle of
            False -> printUsage >> exitWith (ExitFailure 84)
            True -> do
                result <- kMeans conf
                output result
                exitWith ExitSuccess
