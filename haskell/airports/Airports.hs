module Main where

import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies

import Control.Applicative ((<$>))
import Control.Monad (forM_)

import Data.List.Split (chunksOf)

import qualified Data.HashSet as HS
import qualified Data.Hashable as H

data Grid = Grid (Maybe String) (Maybe String) (Maybe String)
    deriving (Eq, Ord, Show)

instance H.Hashable Grid where
    hashWithSalt s (Grid x y z) = H.hashWithSalt s (x, y, z)

instance NFData Grid where
    rnf (Grid x y z) = rnf x `seq` rnf y `seq` rnf z

fromTwoVisitedAirports :: [Grid]
fromTwoVisitedAirports = concat [ f a b  | a <- airports, b <- airports]
  where
    f a b = [ Grid (Just a) (Just b) Nothing
            , Grid (Just a) Nothing  (Just b)
            , Grid Nothing  (Just a) (Just b)
            ]

-- extendGrid :: [String] -> Grid -> [Grid]
extendGrid iata (Grid (Just a) (Just b) Nothing)  = [ (Grid (Just a) (Just b) (Just x)) | x <- HS.toList iata ]
extendGrid iata (Grid (Just a) Nothing  (Just b)) = [ (Grid (Just a) (Just x) (Just b)) | x <- HS.toList iata ]
extendGrid iata (Grid Nothing  (Just a) (Just b)) = [ (Grid (Just x) (Just a) (Just b)) | x <- HS.toList iata ]
extendGrid _ g = error $ "Can't extend this grid: " ++ show g

------------------------

score' :: Grid -> Integer
score' (Grid (Just a) (Just b) (Just c)) = sum [f x | x <- [a, b, c] ++ verticals [a, b, c]]
  where
    f x = if x `HS.member` blinco then 1 else 0
score' g = error $ "Can't compute score of incomplete grid: " ++ show g

dostuff' iataFull x = map (\g -> (score' g, g)) goodTriples
  where
    goodTriples = filter isValid $ concat $ map (extendGrid iataFull) fromTwoVisitedAirports

    isValid :: Grid -> Bool
    isValid (Grid (Just a) (Just b) (Just c)) = and [v `HS.member` iataFull && length (uniq $ [a, b, c] ++ verticals [a, b, c]) >= 5 | v <- verticals [a, b, c]]
    isValid _ = False

------------------------


uniq :: (Ord t, H.Hashable t) => [t] -> [t]
uniq = HS.toList . HS.fromList

airports :: [String]
airports = [ "BNE", "SYD", "ADL", "PER", "CNB", "NTL", "UDG", "CTL"
           , "ULP", "TWB", "WNR", "BVI", "LRE", "EMD", "BLT", "THG"
           , "ROK", "MKY", "PPP", "TSV", "CNS", "HID", "SIN", "NRT"
           , "PVG", "BKK", "CNX", "KBV", "BOM", "DEL", "UDR", "AMM"
           , "BWN", "AKL", "CHC", "ZQN", "LAX", "SFO", "SLC", "LAS"
           , "ORD", "BMI", "JFK", "IAD", "ATL", "BHN", "DEN", "STL"
           , "SJU", "MEX", "SJO", "LIM", "LPB", "SCL", "PMO", "PUQ"
           , "ZCO", "EZE", "MDZ", "AEP", "COR", "MVD", "LHR", "LGW"
           , "STN", "LCY", "CDG", "TXL", "FRA", "MUC", "BCN", "BIO"
           , "FCO", "VCE", "MXP", "GVA", "CPH", "SVO", "LED", "HGK"
           , "HBA"
           ]

blinco :: HS.Set String
blinco = HS.fromList airports

score :: [String] -> Integer
score [a, b, c] = sum [f x | x <- [a, b, c] ++ verticals [a, b, c]]
  where
    f x = if x `HS.member` blinco then 1 else 0

score _ = error "Malformed 3x3 grid?"

verticals :: [String] -> [String]
verticals [a, b, c] = [ [a !! 0, b !! 0, c !! 0]
                      , [a !! 1, b !! 1, c !! 1]
                      , [a !! 2, b !! 2, c !! 2]
                      ]
verticals _ = error "verticals: malformed 3x3 grid?"

airportPairs = [[a, b] | a <- airports, b <- airports, a < b]

dostuff iataFull x = map (\g -> (score g, g)) goodTriples
  where
    goodTriples = filter isValid triples'
    triples' = concat $ filter (/= []) $ map makeTriple [(ab, x) | ab <- airportPairs]

    makeTriple ([a, b], c) = [[a, b, c]]

    isValid :: [String] -> Bool
    isValid = \[a, b, c] -> and [v `HS.member` iataFull && length (uniq $ [a, b, c] ++ verticals [a, b, c]) >= 5 | v <- verticals [a, b, c]]

main = do
    -- IATA list taken from http://aircargotracking.skysthelimitsd.com/air-freight-news/international-airport-codes/
    iataFull <- lines <$> readFile "iata.txt"

    -- let x = concat $ parMap rdeepseq (dostuff $ HS.fromList iataFull) iataFull
    let x = concat $ parMap rdeepseq (dostuff' $ HS.fromList iataFull) iataFull

    forM_ (filter (\(s, _) -> s >= 5) x) $ \(_, (Grid (Just x) (Just y) (Just z))) -> do
        putStrLn $ x ++ y ++ z
