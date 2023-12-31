{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import BD_algorithm
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Map as Map

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Parallel.Strategies
import Data.List (sortOn)

newtype Constellation = Constellation (Map String Minutia)
    deriving (Generic, Show)

instance FromJSON Constellation
instance ToJSON Constellation

getTemplates :: FilePath -> IO (Maybe (Map String Constellation))
getTemplates filePath = do
    fileContent <- B.readFile filePath
    let result = decode fileContent :: Maybe (Map String Constellation)
    return result

getMinutia :: String -> Map.Map String Constellation -> [Minutia]
getMinutia name constellations = do
  case Map.lookup name constellations of
    Just (Constellation c) -> Prelude.map snd $ toList c
    Nothing -> []

-- getConstellation :: String -> Map.Map String Constellation -> Maybe Constellation
-- getConstellation name constellations = Map.lookup name constellations

sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortOn snd

printOutput :: (String, Int) -> IO()
printOutput (name, score) = putStrLn $ name ++ " " ++ show score

mapToTupleList :: Map String Constellation -> [(String, [Minutia])]
mapToTupleList = toList . fmap (\(Constellation stars) -> Prelude.map snd $ toList stars)

readT :: IO Double
readT = do
    putStrLn "Enter a double t:"
    inputT <- getLine
    let maybeT = readMaybe inputT :: Maybe Double
    case maybeT of
        Just t -> do
            putStrLn $ "You entered t = " ++ show t
            return t
        Nothing -> do
            putStrLn "Invalid input for t. Please enter a double."
            readT

readInput :: IO (Int, Double, [Minutia])
readInput = do
    putStrLn "Enter an integer k:"
    inputK <- getLine
    let maybeK = readMaybe inputK :: Maybe Int
    case maybeK of
        Just k -> do
            putStrLn $ "You entered k = " ++ show k
            t <- readT
            minutiae <- processMinutiae []
            return (k, t, minutiae)
        Nothing -> do
            putStrLn "Invalid input for k. Please enter an integer."
            readInput

processMinutiae :: [Minutia] -> IO [Minutia]
processMinutiae minutiaeSoFar = do
    putStrLn "Enter a minutia (double, double, double) or type 'End' to finish:"
    input <- getLine
    if input == "End"
        then do
            putStrLn "Input completed."
            return minutiaeSoFar
        else do
            let maybeMinutia = readMaybeTuple input
            case maybeMinutia of
                Just minutia -> do
                    putStrLn $ "You entered minutia: " ++ show minutia
                    processMinutiae (minutia : minutiaeSoFar)
                Nothing -> do
                    putStrLn "Invalid input for minutia. Please enter a valid tuple."
                    processMinutiae minutiaeSoFar

readMaybeTuple :: String -> Maybe Minutia
readMaybeTuple s = case reads s of
    [(minutia, "")] -> Just minutia
    _               -> Nothing


main :: IO ()
main = do
    let sigX = 0.1
        sigTheta = 0.05
    (k, t, candidateFingerprint) <- readInput
    constellations <- getTemplates "./database generation/constellation data/cartesian.json"
    case constellations of
      Just templates -> do
        let templateFingerprints = mapToTupleList templates
            refVicinity = getMinutia "Triangulum" templates
            refVicinities = createVicinities refVicinity k
            candidateBinaryVector = createBinaryVector candidateFingerprint refVicinities k sigX sigTheta t
            binaryVectors = parMap rdeepseq (\(n, stars) -> (n , createBinaryVector stars refVicinities k sigX sigTheta t)) templateFingerprints
            hammingDistances = parMap rdeepseq (\(n, templateVector) -> (n, hammingDistance templateVector candidateBinaryVector)) binaryVectors
            scoreMap = sortByScore hammingDistances
        mapM_ printOutput $ sortByScore scoreMap
      Nothing -> putStrLn "Error parsing the file"
