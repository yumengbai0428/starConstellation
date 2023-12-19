{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Map (Map, toList)
import Text.Read (readMaybe)
import Control.Parallel.Strategies
import Data.List (sortOn)

type Minutia = (Double, Double, Double) -- (x, y, theta)
type Star = (Double, Double, Double)
newtype Constellation = Constellation (Map String Star)
    deriving (Generic, Show)

instance FromJSON Constellation
instance ToJSON Constellation

getCandidates :: FilePath -> IO (Maybe (Map String Constellation))
getCandidates filePath = do
    fileContent <- B.readFile filePath
    let result = decode fileContent :: Maybe (Map String Constellation)
    return result

sortByScore :: [(String, Double)] -> [(String, Double)]
sortByScore = reverse . sortOn snd

printOutput :: (String, Double) -> IO()
printOutput (name, score) = putStrLn $ name ++ " " ++ show score

mapToTupleList :: Map String Constellation -> [(String, [Star])]
mapToTupleList = toList . fmap (\(Constellation stars) -> map snd $ toList stars)

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


{-
main :: IO ()
main = do
    (k, minutiae) <- readInput
    let sigX = 0.1
      sigTheta = 0.05
      k = 4
    constellations <- getCandidates "./database generation/constellation data/cartesian.json"
    case constellations of
      Just candidates -> do
        let allCandidates = mapToTupleList candidates
            scoreMap = parMap rpar (\(Constellation n s) -> (n, similarityScore s templateFingerprint sigX sigTheta k)) allCandidates
        mapM_ printOutput $ sortByScore scoreMap
      Nothing -> putStrLn "Error parsing the file"
-}