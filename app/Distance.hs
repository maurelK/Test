module Distance (parseInput, kmeans, formatOutput) where

import System.Environment
import System.Exit
import Text.Read
import System.IO
import Control.Monad
import Data.Ord (comparing)
-- import System.Random (random)
import Data.List (minimumBy, groupBy, sortBy, delete)
import Data.Function (on)
import Data.Maybe (fromMaybe)

-- Type alias pour un vecteur (couleur RGB)
type Vector = (Double, Double, Double)

-- Type alias pour un point (position du pixel)
type Point = (Int, Int)

-- Type alias pour un pixel (position + couleur)
type Pixel = (Point, Vector)

-- Type alias pour un cluster (couleur moyenne + liste de pixels)
type Cluster = (Vector, [Pixel])

-- Fonction pour calculer le carré de la différence de deux nombres
opSquare :: Double -> Double -> Double
opSquare a x = (a - x) * (a - x)

-- Fonction pour calculer la distance euclidienne entre deux vecteurs
distance :: Vector -> Vector -> Double
distance (x, y, z) (a, b, c) = sqrt $ opSquare a x + opSquare b y + opSquare c z

-- Parse le fichier d'entrée
parseInput :: String -> [Pixel]
parseInput content =
  map parseLine $ lines content
  where
    parseLine line =
      let [point, color] = words line
          (x, y) = readPoint point
          (r, g, b) = readColor color
      in ((x, y), (fromIntegral r, fromIntegral g, fromIntegral b))

    readPoint :: String -> Point
    readPoint s =
      let [x, y] = map read $ splitOn ',' $ init $ tail s
      in (x, y)

    readColor :: String -> Vector
    readColor s =
      let [r, g, b] = map read $ splitOn ',' $ init $ tail s
      in (fromIntegral r, fromIntegral g, fromIntegral b)

    splitOn :: Char -> String -> [String]
    splitOn delimiter = foldr f [""]
      where
        f c acc@(x:xs)
          | c == delimiter = "":acc
          | otherwise = (c:x):xs

-- Initialisation aléatoire des centroïdes
initializeCentroids :: Int -> [Pixel] -> [Vector]
initializeCentroids k pixels =
  take k $ map snd $ randomSample pixels
  where
    randomSample :: [a] -> [a]
    randomSample xs = map (xs !!) $ take k $ randomRs 0 (length xs - 1) (mkStdGen 42) -- Seed fixe pour la reproductibilité

-- Assignation des pixels aux clusters les plus proches
assignPixels :: [Vector] -> [Pixel] -> [Cluster]
assignPixels centroids pixels =
  let clusters = map (\centroid -> (centroid, [])) centroids
  in foldl (\acc pixel -> assignPixel acc pixel) clusters pixels
  where
    assignPixel :: [Cluster] -> Pixel -> [Cluster]
    assignPixel clusters ((x, y), color) =
      let nearest = minimumBy (comparing (\(centroid, _) -> distance centroid color)) clusters
          updatedCluster = (fst nearest, ((x, y), color) : snd nearest)
      in updatedCluster : filter (\(centroid, _) -> centroid /= fst nearest) clusters

-- Mise à jour des centroïdes
updateCentroids :: [Cluster] -> [Vector]
updateCentroids clusters =
  map (\(_, pixels) -> meanColor pixels) clusters
  where
    meanColor :: [Pixel] -> Vector
    meanColor pixels =
      let (rs, gs, bs) = unzip3 $ map (\(_, (r, g, b)) -> (r, g, b)) pixels
          rMean = sum rs / fromIntegral (length rs)
          gMean = sum gs / fromIntegral (length gs)
          bMean = sum bs / fromIntegral (length bs)
      in (rMean, gMean, bMean)

-- Algorithme k-means
kmeans :: [Pixel] -> Int -> Double -> [Cluster]
kmeans pixels k convergenceLimit =
  let initialCentroids = initializeCentroids k pixels
  in kmeansIteration pixels initialCentroids convergenceLimit
  where
    kmeansIteration :: [Pixel] -> [Vector] -> Double -> [Cluster]
    kmeansIteration pixels centroids limit =
      let clusters = assignPixels centroids pixels
          newCentroids = updateCentroids clusters
      in if converged centroids newCentroids limit
         then clusters
         else kmeansIteration pixels newCentroids limit

    converged :: [Vector] -> [Vector] -> Double -> Bool
    converged old new limit =
      all (\(c1, c2) -> distance c1 c2 < limit) $ zip old new
-- Formate la sortie
formatOutput :: [Cluster] -> String
formatOutput clusters =
  unlines $ map formatCluster clusters
  where
    formatCluster (color, pixels) =
      unlines $ ["--", showColor color, "-"] ++ map formatPixel pixels

    showColor (r, g, b) = "(" ++ show (round r) ++ "," ++ show (round g) ++ "," ++ show (round b) ++ ")"
    formatPixel ((x, y), (r, g, b)) = "(" ++ show x ++ "," ++ show y ++ ") (" ++ show (round r) ++ "," ++ show (round g) ++ "," ++ show (round b) ++ ")"