{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Map (Map)

import Control.Parallel.Strategies
import Data.List (sortOn)
-- import BD_algorithm   -- imports Stella's script as a module to use functions

-- data Minutia = Minutia {x :: Double, y :: Double, z :: Double} deriving (Generic, Show)
-- data Constellation = Constellation { name :: String, stars :: [Minutia] } deriving (Generic, Show)

type Star = (Double, Double, Double)
newtype Constellation = Constellation (Map String Star)
    deriving (Generic, Show)

-- instance FromJSON Minutia
-- instance ToJSON Minutia
instance FromJSON Constellation
instance ToJSON Constellation

-- getCandidates :: FilePath -> IO (Maybe [Constellation])
-- getCandidates filePath = do
--   fileContent <- B.readFile filePath
--   let result = decode fileContent :: Maybe [Constellation]

--   return result

getCandidates :: FilePath -> IO (Maybe (Map String Constellation))
getCandidates filePath = do
    fileContent <- B.readFile filePath
    let result = decode fileContent :: Maybe (Map String Constellation)
    return result

sortByScore :: [(String, Double)] -> [(String, Double)]
sortByScore = reverse . sortOn snd

printOutput :: (String, Double) -> IO()
printOutput (name, score) = putStrLn $ name ++ " " ++ show score

-- main :: IO ()
-- main = do
--     let sigX = 0.1
--         sigTheta = 0.05
--         k = 4
--     constellations <- getCandidates "cartesian.json"
--     case constellations of
--         Just candidates -> do
--             let scoreMap = parMap rpar (\(Constellation n s) -> (n, similarityScore s templateFingerprint sigX sigTheta k)) candidates
--             mapM_ printOutput $ sortByScore scoreMap
--         Nothing -> putStrLn "Error parsing the file"
