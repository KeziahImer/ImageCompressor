{-
-- EPITECH PROJECT, 2023
-- ImageCompressor
-- File description:
-- Arguments
-}

module Arguments
    (Conf(..),
    defaultConf,
    getOpts,
    checkOpts,
    printUsage
    ) where

import Text.Read (readMaybe)
import Data.String (fromString)
import Data.Maybe (isJust, fromJust)
import Control.Exception (try, SomeException)

data Conf = Conf {
    color        :: Maybe Int,
    convergence  :: Maybe Float,
    filepath     :: Maybe FilePath
} deriving (Show)

printUsage :: IO ()
printUsage = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n"
    >> putStrLn "\tN\tnumber of colors in the final image"
    >> putStrLn "\tL\tconvergence limit"
    >> putStrLn "\tF\tpath to the file containing the colors of the pixels"

defaultConf :: Conf
defaultConf = Conf {
    color        = Nothing,
    convergence  = Nothing,
    filepath     = Nothing
}

listOfPixels :: String -> [((Int, Int), (Int, Int, Int))]
listOfPixels file = [(read x, read y) | [x, y] <- map words $ lines file]

validColor :: [((Int, Int), (Int, Int, Int))] -> Bool
validColor [] = True
validColor ((_, (r, g, b)):xs)
    | r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 = False
    | otherwise = validColor xs

checkOpts :: Conf -> IO Bool
checkOpts cnf
    | isJust (color cnf) && isJust (convergence cnf) && isJust (filepath cnf)
    = do
        result <- try (readFile $ fromJust (filepath cnf))
            :: IO (Either SomeException String)
        case result of
            Left _ -> return False
            Right value -> case validColor (listOfPixels value) of
                True -> return True
                False -> return False
    | otherwise = return False

getOpts :: Conf -> [String] -> Int -> Maybe Conf
getOpts _ [] 0 = Nothing
getOpts conf [] _ = Just conf
getOpts conf ("-n":arg:args) _ =
    getOpts (conf {color = readMaybe arg :: Maybe Int}) args 1
getOpts conf ("-l":arg:args) _ =
    getOpts (conf {convergence = readMaybe arg :: Maybe Float}) args 1
getOpts conf ("-f":arg:args) _ =
    getOpts (conf {filepath = Just (fromString arg)}) args 1
getOpts _ _ _ = Nothing
