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

type Star = (Double, Double, Double)
newtype Constellation = Constellation (Map String Star)
    deriving (Generic, Show)

instance FromJSON Constellation
instance ToJSON Constellation

getTemplates :: FilePath -> IO (Maybe (Map String Constellation))
getTemplates filePath = do
    fileContent <- B.readFile filePath
    let result = decode fileContent :: Maybe (Map String Constellation)
    return result

getConstellation :: String -> Map.Map String Constellation -> [Minutia]
getConstellation name constellations = do
  let temp = Map.lookup name constellations
  case temp of
    Just cons -> constellationToMList cons
    Nothing -> []

constellationToMList :: Constellation -> [Minutia]
constellationToMList Constellation stars = Prelude.map snd $ toList stars

sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortOn snd

printOutput :: (String, Int) -> IO()
printOutput (name, score) = putStrLn $ name ++ " " ++ show score

mapToTupleList :: Map String Constellation -> [(String, [Minutia])]
mapToTupleList = toList . fmap (\(Constellation stars) -> Prelude.map snd $ toList stars)

readInput :: IO (Int, [Minutia])
readInput = do
    putStrLn "Enter an integer k:"
    inputK <- getLine
    let maybeK = readMaybe inputK :: Maybe Int
    case maybeK of
        Just k -> do
            putStrLn $ "You entered k = " ++ show k
            minutiae <- processMinutiae []
            return (k, minutiae)
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
        t = 1.531
    (k, candidateFingerprint) <- readInput
    constellations <- getTemplates "./database generation/constellation data/cartesian.json"
    case constellations of
      Just templates -> do
        let templateFingerprints = mapToTupleList templates
            refVicinity = constellations >>= getConstellation "Triangulum"
            refVicinities = createVicinities refVicinity k
            candidateBinaryVector = createBinaryVector candidateFingerprint refVicinities k sigX sigTheta t
            binaryVectors = parMap rpar (\(n, stars) -> (n , createBinaryVector stars refVicinities k sigX sigTheta t)) templateFingerprints
            hammingDistances = parMap rpar (\(n, templateVector) -> (n, hammingDistance templateVector candidateBinaryVector)) binaryVectors
            scoreMap = sortByScore hammingDistances
        mapM_ printOutput $ sortByScore scoreMap
      Nothing -> putStrLn "Error parsing the file"
