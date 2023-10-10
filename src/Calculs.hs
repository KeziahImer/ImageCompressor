{-
-- EPITECH PROJECT, 2023
-- ImageCompressor
-- File description:
-- Calculs
-}

module Calculs
    ( kMeans,
    output
    ) where

import Data.Maybe (fromJust)
import System.Random (getStdGen, randomRs)
import Arguments (Conf(..))

type Coord = (Int, Int)
type Color = (Int, Int, Int)
type Pixel = (Coord, Color)
type Cluster = (Pixel, [Pixel])

defaultCluster :: Cluster
defaultCluster = (((0, 0), (0, 0, 0)), [])

sq :: Int -> Int
sq x = x * x

distance :: Color -> Color -> Float
distance (f1, f2, f3) (s1, s2, s3) =
    sqrt $ fromIntegral $ sq (f1 - s1) + sq (f2 - s2) + sq (f3 - s3)

listOfPixels :: String -> [Pixel]
listOfPixels file = [(read x, read y) | [x, y] <- map words $ lines file]

showPixels :: [Pixel] -> String
showPixels [] = ""
showPixels ((x, y):xs) = show x ++ " " ++ show y ++ "\n" ++ showPixels xs

showPixel :: Pixel -> String
showPixel (_, y) = "--\n" ++ show y ++ "\n-\n"

showClusters :: [Cluster] -> String
showClusters [] = ""
showClusters ((x, y):xs) = showPixel x ++ showPixels y ++ showClusters xs

output :: [Cluster] -> IO ()
output clusters = putStr $ showClusters clusters

closestCluster :: Pixel -> [Cluster] -> Cluster -> Int -> Cluster
closestCluster _ [] result _ = result
closestCluster pixel (cluster:xs) _ 0 = closestCluster pixel xs cluster 1
closestCluster pixel (cluster:xs) result _ =
    let oldResult = distance (snd pixel) (snd (fst result))
        newResult = distance (snd pixel) (snd (fst cluster))
    in if oldResult > newResult
        then closestCluster pixel xs cluster 1
        else closestCluster pixel xs result 1

closest :: Pixel -> [Cluster] -> [Cluster]
closest pixel clusters =
    let closestClust = closestCluster pixel clusters defaultCluster 0
        updatedCluster = (fst closestClust, snd closestClust ++ [pixel])
    in updatedCluster : filter (\c -> fst c /= fst closestClust) clusters

assign :: [Pixel] -> [Cluster] -> [Cluster]
assign [] clusters = clusters
assign (x:xs) clusters =
    let news = closest x clusters
    in assign xs news

meanColors :: [Color] -> Int -> Color -> Color
meanColors [] index (r, g, b) = (r `div` index, g `div` index, b `div` index)
meanColors ((r1, g1, b1):xs) index (r2, g2, b2) =
    meanColors xs (index + 1) (r1 + r2, g1 + g2, b1 + b2)

mean :: Cluster -> Cluster
mean cluster =
    let pixels = snd cluster
        coords = fst $ fst cluster
        colors = map snd pixels
        meanColor = meanColors colors 0 (0, 0, 0)
    in ((coords, meanColor), [])

newClusters :: [Cluster] -> [Cluster]
newClusters [] = []
newClusters (cluster:xs) =
    let meanCluster = mean cluster
    in meanCluster : newClusters xs

verif :: [Cluster] -> [Cluster] -> Float -> Bool
verif [] _ _ = True
verif _ [] _ = True
verif (x:xs) (y:ys) limit
    | (distance (snd (fst x)) (snd (fst y))) < limit = verif xs ys limit
    | otherwise = False

clustering :: [Pixel] -> [Cluster] -> [Cluster] -> Int -> Float -> IO [Cluster]
clustering list clusters_n clusters_o 1 limit
    | verif clusters_n clusters_o limit = return (assign list clusters_n)
clustering list cluster_n _ _ limit =
    let newCluster = assign list cluster_n
    in clustering list (newClusters newCluster) newCluster 1 limit

randClusters :: Int -> [Pixel] -> IO [Cluster]
randClusters nbr list = do
    gen <- getStdGen
    let indexList = take nbr $ randomRs (0, (length list) - 1) gen :: [Int]
    return $ map (\i -> (list !! i, [])) indexList

formatFile :: FilePath -> IO [Pixel]
formatFile path = readFile path >>= return . listOfPixels

kMeans :: Conf -> IO [Cluster]
kMeans conf = do
    list <- formatFile (fromJust $ filepath conf)
    clusters <- randClusters (fromJust $ color conf) list
    clustering list clusters [defaultCluster] 0 (fromJust $ convergence conf)
