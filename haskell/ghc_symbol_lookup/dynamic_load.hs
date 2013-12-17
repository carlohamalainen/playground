-- With inspiration from:
-- http://parenz.wordpress.com/2013/08/17/ghc-api-interpreted-compiled-and-package-modules/

import Control.Applicative
import DynFlags
import GHC
import GHC.Paths
import GhcMonad            (liftIO) -- from ghc7.7 and up you can use the usual
                                    -- liftIO from Control.Monad.IO.Class
import Unsafe.Coerce

import Outputable
import System.Environment
import Control.Monad

main = do
    args <- getArgs

    -- FIXME brittle
    let haskellFile   = args !! 0   -- e.g. "Foo.hs"
        haskellModule = args !! 1   -- e.g. "Foo"
        haskellSymbol = args !! 2   -- e.g. "String", the thing we are looking for.
        imports       = drop 3 args -- space-separated imports to apply in our current test

    putStrLn $ "Haskell source file: " ++ haskellFile
    putStrLn $ "Haskell module (expected in source file): " ++ haskellModule
    putStrLn $ "Haskell symbol (should be used somewhere in the file): " ++ haskellSymbol
    putStrLn $ "Import list: " ++ (show imports)

    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                        , ghcLink   = LinkInMemory
                                        }
            setTargets =<< sequence [ guessTarget haskellFile Nothing ]
           
            -- Load each of the modules specified on the command line.
            forM_ imports $ \m -> load $ LoadUpTo (mkModuleName m)

            -- Set the context for each of the modules; note the use of IIDecl.
            setContext $ map (IIDecl . simpleImportDecl . mkModuleName) imports

            -- Attempt to type check the expression. This can cause the error
            --      dynamic_load.hs: panic! (the 'impossible' happened)
            -- so this program can't operate on (inits imports) to work out
            -- when the symbol in question appears in scope.
            act <- exprType haskellSymbol
            -- act <- typeKind False haskellSymbol
            liftIO $ putStrLn $ showSDoc tracingDynFlags (ppr act)
            liftIO $ putStrLn "SUCCESS"


