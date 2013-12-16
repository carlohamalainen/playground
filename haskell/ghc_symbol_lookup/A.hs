-- Initial version of this file is from http://www.haskell.org/haskellwiki/GHC/As_a_library
--A.hs
--invoke: ghci -package ghc A.hs
 
{-# LANGUAGE CPP #-}
import GHC
import Outputable
 
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths
 
import DynFlags
targetFile = "B.hs"

main = do
   res <- example
#if __GLASGOW_HASKELL__ > 704
   putStrLn $ showSDoc tracingDynFlags ( ppr res )
#else
   putStrLn $ showSDoc ( ppr res )
#endif
 
example = 
#if __GLASGOW_HASKELL__ > 704
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
#else
    defaultErrorHandler defaultLogAction $ do
#endif
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags
                            [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags'

        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets

        -- http://stackoverflow.com/questions/11571520/reify-a-module-into-a-record
        setContext [IIDecl (simpleImportDecl (mkModuleName "B"))]

        modSum <- getModSummary $ mkModuleName "B"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        l <- loadModule d
        n <- getNamesInScope
        c <- return $ coreModule d
 
        g <- getModuleGraph
        mapM showModule g

        -- Works, not sure if we need it.
        ctx <- getContext
        GHC.setContext ctx

        -- Get the "source imports"?
        let sourceImports = ms_srcimps modSum
        let textualImports = ms_textual_imps modSum
        rdrNames <- getRdrNamesInScope

        ghcNames <- GHC.getNamesInScope



        -- http://www.cse.unsw.edu.au/~pls/repos/yi/Yi/Boot.hs
        -- setContext = GHC.setContext session,
        -- setContextAfterLoad = setContextAfterLoadL session,
        -- getNamesInScope = GHC.getNamesInScope session,
        -- getRdrNamesInScope = GHC.getRdrNamesInScope session,
        -- nameToString = Outputable.showSDoc . Outputable.ppr,
        -- isLoaded = GHC.isLoaded session,
        -- mkModuleName = Module.mkModuleName,
        -- getModuleGraph = GHC.getModuleGraph session ma/g 





        -- return $ (sourceImports, n, parsedSource d,"/n-----/n",  typecheckedSource d)
        -- return (n, ghcNames, sourceImports, textualImports, ctx, rdrNames)
        return (n, rdrNames, sourceImports, textualImports)
