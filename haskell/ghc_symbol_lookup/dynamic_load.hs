-- Original version from:
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


main = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    }
        setTargets =<< sequence [ guessTarget "S3Checksums.hs" Nothing ]
        load LoadAllTargets
        -- Bringing the module into the context
        setContext [ IIModule . mkModuleName $ "S3Checksums"
                   , IIDecl
                     . simpleImportDecl
                     . mkModuleName $ "Outputable" ]

        setContext [ IIModule . mkModuleName $ "S3Checksums"
                   , IIDecl
                     . simpleImportDecl
                     . mkModuleName $ "System.Process" ]

        act' <- exprType "createProcess"
        liftIO $ putStrLn $ showSDoc tracingDynFlags (ppr act')

