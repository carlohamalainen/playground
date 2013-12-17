{-# LANGUAGE CPP #-}

-- FIXME we end up semi-compiling each module that we check - can we use
-- an interactive load instead?

module Main where

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

import System.Environment
import System.Exit


import qualified Distribution.Simple.Program
import qualified Distribution.Simple.Compiler
import qualified Distribution.Simple.PackageIndex
import qualified Distribution.Simple.GHC
import qualified Distribution.InstalledPackageInfo


data HaskellModule = MkHaskellModule { modName :: String
                                     , modIsImplicit :: Bool
                                     } deriving (Show)

matchTypeName :: String -> SymTypeInfo OrigName -> Bool
matchTypeName symbol symTypeInfo = symbol == nameS
    where GName modNameS nameS = origGName $ st_origName symTypeInfo

matchName :: String -> SymValueInfo OrigName -> Bool
matchName symbol symValueInfo = symbol == nameS
    where GName modNameS nameS = origGName $ sv_origName symValueInfo


-- moduleExportsThing :: String -> String -> 
moduleExportsThing symbol (MkHaskellModule moduleName _) = do
    pkgs <- (++) <$>
              getInstalledPackages (Proxy :: Proxy NamesDB) UserPackageDB <*>
              getInstalledPackages (Proxy :: Proxy NamesDB) GlobalPackageDB
    
    (names, types) <- evalNamesModuleT (namesAndTypesInModule moduleName) pkgs

    return $ (any (matchName symbol) names) || (any (matchTypeName symbol) types)

-- getGHCPackages :: IO [InstalledPackageInfo]
-- getGHCPackages = do
--   pc <- Distribution.Simple.Program.configureAllKnownPrograms minBound (Distribution.Simple.Program.addKnownPrograms [Distribution.Simple.Program.ghcProgram,Distribution.Simple.Program.ghcPkgProgram] Distribution.Simple.Program.emptyProgramConfiguration)
--   ix <- getInstalledPackages minBound [GlobalPackageDB, UserPackageDB] pc
--   return (Distribution.Simple.PackageIndex.allPackages ix)

go haskellFile haskellModule symbol = do
    -- Get list of imports from this file/module
    imports <- (map conv) <$> getImports haskellFile haskellModule

    pkgs <- (++) <$>
              getInstalledPackages (Proxy :: Proxy NamesDB) UserPackageDB <*>
              getInstalledPackages (Proxy :: Proxy NamesDB) GlobalPackageDB

    (names, types) <- evalNamesModuleT (namesAndTypesInModule haskellModule) pkgs

    matches <- zip imports <$> mapM (moduleExportsThing symbol) imports

    let finds = find snd matches

    case finds of Just (MkHaskellModule name implicit, _) -> putStrLn $ "Found \"" ++ symbol ++ "\" in " ++ name
                  Nothing                                 -> putStrLn $ "Error: could not find \"" ++ symbol ++ "\""

main = do
    args <- getArgs

    -- FIXME brittle
    let haskellFile   = args !! 0
        haskellModule = args !! 1
        symbol        = args !! 2

    go haskellFile haskellModule symbol


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

