module BD_algorithm where
import Debug.Trace
import Data.List (sortBy, transpose)
import Data.Array.Base (listArray)
import Munkres  -- import Hungarian Algorithm

type Minutia = (Double, Double, Double) -- (x, y, z)
type Vicinity = [Minutia]
type ScoreMatrix = [[Double]]

minutiaDistance :: Minutia -> Minutia -> Double
minutiaDistance (x1, y1, z1) (x2, y2, z2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^2

minutiaTheta :: Minutia -> Double
minutiaTheta (x, y, z) = acos (z / minutiaDistance (x, y, z) (0.0, 0.0, 0.0))

kNearestNeighbors :: [Minutia] -> Minutia -> Int -> [Minutia]
kNearestNeighbors allMinutiae m k =
    take k $ sortBy (compareDistance) allMinutiae
    where
    compareDistance :: Minutia -> Minutia -> Ordering
    compareDistance m1 m2 = compare (minutiaDistance m1 m) (minutiaDistance m2 m)

createVicinities :: [Minutia] -> Int -> [[Minutia]]
createVicinities minutiae k =
    map (\m -> kNearestNeighbors minutiae m k) minutiae

scoreMatrix :: [Minutia] -> [Minutia] -> Double -> Double -> ScoreMatrix
scoreMatrix candidateVicinity templateVicinity sigX sigTheta =
    [[ minutiaeScore ai bj sigX sigTheta | bj <- templateVicinity] | ai <- candidateVicinity]

minutiaeScore :: Minutia -> Minutia -> Double -> Double -> Double
minutiaeScore (x1, y1, z1) (x2, y2, z2) sigX sigTheta =
    (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^2 + (sigX / sigTheta) * (theta1 - theta2) ^ 2
    where
    theta1 = minutiaTheta (x1, y1, z1)
    theta2 = minutiaTheta (x2, y2, z2)
-- need to caculate the variance sigs by experiment -> for now just try random number or just 1....

retrieveHungarian :: ScoreMatrix -> Double
retrieveHungarian scoreMat = snd $ hungarianMethodDouble $ listArray ((1,1), (k,k)) $ concat scoreMat
    where
    k = length scoreMat

vicinitySimilarityScore :: [Minutia] -> [Minutia] -> Double -> Double -> Double
vicinitySimilarityScore candidate template sigX sigTheta =
    let scoreMat = scoreMatrix candidate template sigX sigTheta
        matchingScore = retrieveHungarian scoreMat
    in matchingScore

-- reference Vicinities (set of stars): refVicinities

createBinaryVector :: [Minutia] -> [[Minutia]] -> Int -> Double -> Double -> Double -> [Int]
createBinaryVector candidate refVicinities k sigX sigTheta t =
    let candidateVicinities = createVicinities candidate k
        binaryVector = [binaryElement candidateVicinities ri t | ri <- refVicinities]
        binaryElement :: [[Minutia]] -> [Minutia] -> Double -> Int
        binaryElement vicinities refVicinity threshold =
            if any (\vicinity -> isAboveThreshold vicinity refVicinity threshold) vicinities
                then 1
                else 0

        isAboveThreshold :: [Minutia] -> [Minutia] -> Double -> Bool
        isAboveThreshold vicinity refVicinity threshold =
            vicinitySimilarityScore vicinity refVicinity sigX sigTheta > threshold
    in trace (show (head candidate)) binaryVector

hammingDistance :: [Int] -> [Int] -> Int
hammingDistance [] [] = 0
hammingDistance (x:xs) (y:ys)
    | x /= y = 1 + hammingDistance xs ys 
    | otherwise = hammingDistance xs ys
hammingDistance _ _ = error "Lists must have the same length"


