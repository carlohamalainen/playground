{-# LANGUAGE RankNTypes #-}

module Main where

import Test.QuickCheck
import Zipper

quickCheckMany :: forall a. (Arbitrary a, Show a) => (a -> Bool) -> IO ()
quickCheckMany = quickCheckWith stdArgs { maxSuccess = 1000 }

main :: IO ()
main = do
    putStrLn "prop_finish_createZipper"
    quickCheckMany prop_finish_createZipper

    putStrLn "prop_inorder"
    quickCheckMany prop_inorder

    putStrLn "prop_maptree"
    quickCheckMany prop_maptree

    putStrLn "prop_maptree'"
    quickCheckMany prop_maptree'

    putStrLn "prop_zip_unzip"
    quickCheckMany prop_zip_unzip
