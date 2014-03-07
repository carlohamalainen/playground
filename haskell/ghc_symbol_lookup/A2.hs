{-# LANGUAGE Rank2Types #-}

--A.hs
--invoke: ghci -package ghc A.hs
 
{-# LANGUAGE CPP #-}
import GHC
import Outputable
 
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths

import Data.Typeable

import DynFlags

import Control.Applicative
import Control.Monad (void, when)
import CoreUtils
import Data.Function
import Data.Generics hiding (typeOf)
import Data.List
import Data.Maybe
import Data.Ord as O
import Data.Time.Clock
import Desugar
import GHC
import GHC.SYB.Utils
import HscTypes
import Outputable
import PprTyThing
import TcHsSyn (hsPatType)
import TcRnTypes


import qualified Language.Haskell.GhcMod.Gap as Gap



data Hole = Hole

targetFile = "B2.hs"

listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
listifySpans tcs lc = listifyStaged TypeChecker p tcs
  where
    p (L spn _) = isGoodSrcSpan spn && spn `spans` lc

listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))

main = do
   (es, tcs)  <- example

   -- print $ show bs

   putStrLn $ showSDoc tracingDynFlags ( ppr $ last es )
   print "derpahahaha"
 
example = 
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags
                            [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags'
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName "B2"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        l <- loadModule d
        n <- getNamesInScope
        c <- return $ coreModule d
 
        g <- getModuleGraph
        mapM showModule g     
        -- return $ t -- (typecheckedSource d)

        -- let lineNo = 22
        --     colNo  = 13

        let lineNo = 38
            colNo  = 10

        let tcm@TypecheckedModule{tm_typechecked_source = tcs} = t
        let bs = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
            es = listifySpans tcs (lineNo, colNo) :: [LHsExpr Id]
            ps = listifySpans tcs (lineNo, colNo) :: [LPat Id]

        hs_env <- getSession

        let modu = ms_mod $ pm_mod_summary $ tm_parsed_module tcm
            rn_env = tcg_rdr_env $ fst $ tm_internals_ tcm
            ty_env = tcg_type_env $ fst $ tm_internals_ tcm

        (_, mbe) <- Gap.liftIO $ deSugarExpr hs_env modu rn_env ty_env (head es)

        return (es, tcs)
{-
getType tcm e = do
    hs_env <- getSession
    (_, mbe) <- Gap.liftIO $ deSugarExpr hs_env modu rn_env ty_env e
    return $ (getLoc e, ) <$> CoreUtils.exprType <$> mbe
  where
    modu = ms_mod $ pm_mod_summary $ tm_parsed_module tcm
    rn_env = tcg_rdr_env $ fst $ tm_internals_ tcm
    ty_env = tcg_type_env $ fst $ tm_internals_ tcm
-}



