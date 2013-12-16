{-# LANGUAGE CPP #-}

module LookupTool where

import qualified GHC
import Outputable
import GHC.Paths ( libdir )
import DynFlags

import qualified SrcLoc

import System.FilePath.Posix

import Language.Haskell.Exts.Annotated
import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Distribution.HaskellSuite
import Distribution.Simple.Compiler

import Data.Maybe
import Data.List
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import Text.Printf
import Control.Applicative
import Control.Monad


data HaskellModule = MkHaskellModule { modName :: String
                                     , modIsImplicit :: Bool
                                     } deriving (Show)


lookupSymbol symbol fileName = undefined

main = do
    -- 1, get import list
    res <- getImports "B.hs" "B"
    print $ map conv res

    -- 2, look up exported things.
    pkgs <- (++) <$>
              getInstalledPackages (Proxy :: Proxy NamesDB) UserPackageDB <*>
              getInstalledPackages (Proxy :: Proxy NamesDB) GlobalPackageDB

    forM_ ["Prelude", "Data.Maybe"] $ \m -> do print m
                                               evalNamesModuleT (namesAndTypesInModule m) pkgs >>= print

conv :: SrcLoc.Located (GHC.ImportDecl GHC.RdrName) -> HaskellModule
conv idecl = MkHaskellModule name isImplicit
    where idecl' = SrcLoc.unLoc idecl
          name = showSDoc tracingDynFlags (ppr $ GHC.ideclName $ idecl')
          isImplicit = GHC.ideclImplicit idecl'
 
getImports :: FilePath -> String -> IO [SrcLoc.Located (GHC.ImportDecl GHC.RdrName)]
getImports targetFile moduleName = 
    GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags

        -- FIXME What if the source file does *not* have an
        -- implicit prelude import? We'll have to use a different
        -- definition of dflags'?

        let dflags' = foldl xopt_set dflags
                            [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        GHC.setSessionDynFlags dflags'

        target <- GHC.guessTarget targetFile Nothing
        GHC.setTargets [target]
        GHC.load GHC.LoadAllTargets

        -- http://stackoverflow.com/questions/11571520/reify-a-module-into-a-record
        GHC.setContext [GHC.IIDecl (GHC.simpleImportDecl (GHC.mkModuleName moduleName))]

        modSum <- GHC.getModSummary $ GHC.mkModuleName moduleName

        -- let sourceImports = ms_srcimps modSum
        let textualImports = GHC.ms_textual_imps modSum

        return textualImports



namesAndTypesInModule :: String -> ModuleT Symbols IO ([SymValueInfo OrigName], [SymTypeInfo OrigName])
namesAndTypesInModule moduleName = do
    Symbols values _types <- fromMaybe (error $ "Module " ++ moduleName ++ " not found") <$> getModuleInfo moduleName

    return $ (Set.toList values, Set.toList _types)

