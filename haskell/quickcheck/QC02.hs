-- https://gist.githubusercontent.com/ijt/967505/raw/d370f2d84794cdf6181316e1b132a7633967596f/io_quickcheck_example.hs


-- Synopsis:
-- $ cabal install QuickCheck
-- $ runhaskell io_quickcheck_example.hs
--
-- Author: Issac Trotts <issac.trotts@gmail.com>

import System.Directory
import System.Environment
import System.Process
import System.Exit
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

main = do
    putStrLn "addThenClearMakesClear"
    quickCheck addThenClearMakesClear

    putStrLn "addNewIsIdempotent"
    quickCheck addNewIsIdempotent

-- Add some strings to /tmp/foo
add :: [String] -> IO ()
add strings = do
    -- Have to write then rename to work around lazy IO.
    oldStrings <- get
    writeFile "/tmp/foo2" $ unlines $ oldStrings ++ strings
    renameFile "/tmp/foo2" "/tmp/foo"

-- Add some strings to /tmp/foo that were not already there
addNew :: [String] -> IO ()
addNew strings = do
    oldStrings <- get
    add [s | s <- strings, s `notElem` oldStrings && s /= ""]

-- Get all the strings in /tmp/foo
get :: IO [String]
get = readProcessOrDie "cat" ["/tmp/foo"] "" >>= return . lines

-- Clear /tmp/foo
clear :: IO ()
clear = do
    writeFile "/tmp/foo2" ""
    renameFile "/tmp/foo2" "/tmp/foo"

readProcessOrDie :: String -> [String] -> String -> IO String
readProcessOrDie cmd args input = do
    (code, stdout, _) <- readProcessWithExitCode cmd args input
    case code of
        ExitFailure i -> error $ ("Command failed with status " ++ show i ++
                                  ": " ++ cmd ++ show args)
        ExitSuccess -> return stdout

addThenClearMakesClear :: [String] -> Property
addThenClearMakesClear strings = monadicIO $ do
    run $ add strings
    run $ clear
    contents <- run $ get
    assert $ contents == []

addNewIsIdempotent :: [String] -> Property
addNewIsIdempotent strings = (and $ map ('\n' `notElem`) strings) ==> monadicIO $ do
    run $ clear
    run $ addNew strings
    contents1 <- run $ get
    run $ addNew strings
    contents2 <- run $ get
    assert $ contents1 == contents2
