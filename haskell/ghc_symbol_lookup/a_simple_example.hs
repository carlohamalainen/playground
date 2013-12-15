{-# LANGUAGE CPP #-}

-- http://www.haskell.org/haskellwiki/GHC/As_a_library#Getting_Started
-- 3 A Simple Example

import GHC
import GHC.Paths ( libdir )
import DynFlags
 
main = 
#if __GLASGOW_HASKELL__ > 704
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
#else
    defaultErrorHandler defaultLogAction $ do
#endif
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        target <- guessTarget "HelloWorld.hs" Nothing
        setTargets [target]
        load LoadAllTargets

