{-# LANGUAGE RankNTypes #-}

module Main where

import Control.DeepSeq
import Control.Parallel()
import Control.Parallel.Strategies

import Control.Applicative ((<$>))
import Control.Monad (forM_)

import qualified Data.HashSet as HS
import qualified Data.Hashable as H

data Grid = Grid (Maybe String) (Maybe String) (Maybe String)
    deriving (Eq, Ord, Show)

instance H.Hashable Grid where
    hashWithSalt s (Grid x y z) = H.hashWithSalt s (x, y, z)

instance NFData Grid where
    rnf (Grid x y z) = rnf x `seq` rnf y `seq` rnf z

-- Starting grids are based on two airports that Blinco has been to.
startingGrids :: [Grid]
startingGrids = concat [ f a b  | a <- blinco, b <- blinco]
  where
    f a b = [ Grid (Just a) (Just b) Nothing
            , Grid (Just a) Nothing  (Just b)
            , Grid Nothing  (Just a) (Just b)
            ]

-- Extend any type of 2-row grid in all possible ways.
extendGrid :: HS.Set String -> Grid -> [Grid]
extendGrid iata (Grid (Just a) (Just b) Nothing)  = [ Grid (Just a) (Just b) (Just x) | x <- HS.toList iata ]
extendGrid iata (Grid (Just a) Nothing  (Just b)) = [ Grid (Just a) (Just x) (Just b) | x <- HS.toList iata ]
extendGrid iata (Grid Nothing  (Just a) (Just b)) = [ Grid (Just x) (Just a) (Just b) | x <- HS.toList iata ]
extendGrid _    g = error $ "Can't extend this grid: " ++ show g

-- The score of a grid is the number of row and column codes that Blinco has visited.
-- So the maximum score for a grid is 6.
score :: Grid -> Integer
score (Grid (Just a) (Just b) (Just c)) = sum [f x | x <- [a, b, c] ++ verticals [a, b, c]]
  where
    f x = if x `HS.member` blinco' then 1 else 0
score g = error $ "Can't compute score of incomplete grid: " ++ show g

-- Given a grid, extend it, check that it is valid, and keep only those
-- with score 5.
dostuff :: HS.Set String -> Grid -> [(Integer, Grid)]
dostuff iataFull x = map (\g -> (score g, g)) goodTriples
  where
    goodTriples = filter isValid $ extendGrid iataFull x

    -- A grid is valid if each row (reading left to right) and each
    -- column (reading top down) is an IATA code.
    isValid :: Grid -> Bool
    isValid (Grid (Just a) (Just b) (Just c)) = and [v `HS.member` iataFull && length (uniq $ [a, b, c] ++ verticals [a, b, c]) >= 5 | v <- verticals [a, b, c]]
    isValid g = error $ "Can't check validity of a partial grid: " ++ show g

uniq :: (Ord t, H.Hashable t) => [t] -> [t]
uniq = HS.toList . HS.fromList

-- Airports that Blinco has been to.
blinco :: [String]
blinco = [ "BNE", "SYD", "ADL", "PER", "CNB", "NTL", "UDG", "CTL"
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
           , "TPE", "SGN", "MNL"
           ]

-- As a hash set for efficient membership testing.
blinco' :: HS.Set String
blinco' = HS.fromList blinco

-- Extract the top-down column codes.
verticals :: [String] -> [String]
verticals [a, b, c] = [ [a !! 0, b !! 0, c !! 0]
                      , [a !! 1, b !! 1, c !! 1]
                      , [a !! 2, b !! 2, c !! 2]
                      ]
verticals _ = error "verticals: malformed 3x3 grid?"

main :: IO ()
main = do
    -- IATA list taken from http://aircargotracking.skysthelimitsd.com/air-freight-news/international-airport-codes/
    iataFull <- lines <$> readFile "iata.txt"

    let grids = uniq $ concat $ parMap rdeepseq (dostuff $ HS.fromList iataFull) startingGrids

    forM_ (filter (\(s, _) -> s >= 5) grids) $ \(_, Grid (Just x) (Just y) (Just z)) -> do putStrLn x
                                                                                           putStrLn y
                                                                                           putStrLn z
                                                                                           putStrLn ""
