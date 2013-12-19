{-# LANGUAGE CPP #-}

-- FIXME we end up semi-compiling each module that we check - can we use
-- an interactive load instead?

module Main where

import qualified GHC
import Outputable
import GHC.Paths ( libdir )
import DynFlags

import Name
import RdrName
import HscTypes

import qualified SrcLoc
import FastString

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
import System.IO
import System.Process

import Data.List

-- Read everything else available on a handle, and return the empty
-- string if we have hit EOF.
readRestOfHandle :: Handle -> IO String
readRestOfHandle h = do
    ineof <- hIsEOF h
    if ineof
        then return ""
        else hGetContents h

data Hole = Hole
hole = undefined

data HaskellModule = MkHaskellModule { modName          :: String
                                     , modQualifier     :: Maybe String
                                     , modIsImplicit    :: Bool
                                     , modHiding        :: [String]

                                     -- ideclSource :: Bool
                                     -- True = {--} import

                                     -- ideclSafe :: Bool
                                     -- True => safe import
                                     -- ideclQualified :: Bool
                                     -- True => qualified
                                     -- ideclImplicit :: Bool
                                     -- True => implicit import (of Prelude)
                                     -- ideclAs :: Maybe ModuleName
                                     -- as Module
                                     -- ideclHiding :: Maybe (Bool, [LIE name])
                                     -- (True => hiding, names)

                                     } deriving (Show)

-- Found the symbol, all is well, OR found it but now ambiguous due
-- to an earlier import, store the stderr result.
data CheckResult = CheckFoundSymbol | CheckFoundAmbiguous String

instance Show CheckResult where
    show (CheckFoundSymbol) = "CheckFoundSymbol"
    show (CheckFoundAmbiguous _) = "CheckFoundAmbiguous"

matchTypeName :: String -> SymTypeInfo OrigName -> Bool
matchTypeName symbol symTypeInfo = symbol == nameS
    where GName modNameS nameS = origGName $ st_origName symTypeInfo

matchName :: String -> SymValueInfo OrigName -> Bool
matchName symbol symValueInfo = symbol == nameS
    where GName modNameS nameS = origGName $ sv_origName symValueInfo


-- moduleExportsThing :: String -> String -> 
moduleExportsThing symbol (MkHaskellModule moduleName moduleQualifier moduleIsImplicit moduleHiding) = do
    pkgs <- (++) <$>
              getInstalledPackages (Proxy :: Proxy NamesDB) UserPackageDB <*>
              getInstalledPackages (Proxy :: Proxy NamesDB) GlobalPackageDB
    
    (names, types) <- evalNamesModuleT (namesAndTypesInModule moduleName) pkgs

    return $ (any (matchName symbol) names) || (any (matchTypeName symbol) types)

runCheckCmd :: String -> String -> String -> String -> [HaskellModule] -> IO CheckResult
runCheckCmd cmd haskellFile haskellModule symbol imports = do
    let args = [haskellFile, haskellModule, symbol] ++ (map modName imports)
    (_, Just hout, Just herr, _) <- createProcess (proc cmd args){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    stdOut <- readRestOfHandle hout
    stdErr <- readRestOfHandle herr

    return $ if isInfixOf "SUCCESS" stdOut && (not $ symbol `elem` (concatMap modHiding imports))
                then CheckFoundSymbol 
                else CheckFoundAmbiguous stdErr

runCheckKind :: String -> String -> String -> [HaskellModule] -> IO CheckResult
runCheckKind = runCheckCmd "CheckKind"

runCheckType :: String -> String -> String -> [HaskellModule] -> IO CheckResult
runCheckType = runCheckCmd "CheckType"

go haskellFile haskellModule symbol = do
    x <- findNamesTest haskellFile haskellModule
    -- print $ "parsed name: " ++ (showSDoc tracingDynFlags (ppr $ x))
    -- print $ "nameModule: " ++ (showSDoc tracingDynFlags (ppr $ GHC.nameModule x))
    -- print $ "nameSrcLoc: " ++ (showSDoc tracingDynFlags (ppr $ nameSrcLoc x))
    -- print "derp"
    print $ "=====> " ++ (showSDoc tracingDynFlags (ppr x))

    {-
    -- Get list of imports from this file/module
    imports <- inits <$> (map conv) <$> getImports haskellFile haskellModule

    typeChecks <- (zip (map (map modName) imports)) <$> mapM (runCheckType haskellFile haskellModule symbol) imports

    forM_ typeChecks print
    -}



    -- typeChecks <- (zip imports) <$> mapM (runCheckType haskellFile haskellModule symbol) imports
    -- kindChecks <- (zip imports) <$> mapM (runCheckKind haskellFile haskellModule symbol) imports

    -- let allChecks = typeChecks ++ kindChecks

    -- forM_ allChecks $ \a -> print a

    -- case find snd allChecks of (Just (imps, _)) -> print $ last imps
    --                            Nothing  -> print "error could not find it, derp"

main = do
    args <- getArgs

    -- FIXME brittle
    let haskellFile   = args !! 0
        haskellModule = args !! 1
        symbol        = args !! 2

    print haskellFile
    print haskellModule
    print symbol

    go haskellFile haskellModule symbol


conv :: SrcLoc.Located (GHC.ImportDecl GHC.RdrName) -> HaskellModule
conv idecl = MkHaskellModule name qualifier isImplicit hiding
    where idecl' = SrcLoc.unLoc idecl
          name = showSDoc tracingDynFlags (ppr $ GHC.ideclName $ idecl')
          isImplicit = GHC.ideclImplicit idecl'
          qualifier = unpackFS <$> GHC.ideclPkgQual idecl'
          hiding = removeBrackets $ blerp $ GHC.ideclHiding idecl'

          removeBrackets [] = []
          removeBrackets x = reverse . tail . reverse . tail $ x

bloop :: GHC.Located (GHC.IE GHC.RdrName) -> String
bloop loc = showSDoc tracingDynFlags (ppr names)
    where names = GHC.ieNames $ SrcLoc.unLoc loc

blerp Nothing = []
blerp (Just (False, _)) = [] -- FIXME really? Why allow this?
blerp (Just (True, hiding)) = map bloop hiding

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


-- findNamesTest :: FilePath -> String -> IO [SrcLoc.Located (GHC.ImportDecl GHC.RdrName)]
findNamesTest targetFile moduleName = 
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
 
        -- Set the context for each of the modules; note the use of IIDecl.
        GHC.setContext $ map (GHC.IIDecl . GHC.simpleImportDecl . GHC.mkModuleName) ["Prelude", moduleName]

        -- http://stackoverflow.com/questions/11571520/reify-a-module-into-a-record
        -- GHC.setContext [GHC.IIDecl (GHC.simpleImportDecl (GHC.mkModuleName moduleName))]

        modSum <- GHC.getModSummary $ GHC.mkModuleName moduleName

        -- let sourceImports = ms_srcimps modSum
        let textualImports = GHC.ms_textual_imps modSum

        h <- head <$> GHC.parseName "head"

        let h' = nameOccName h

        let env = emptyGlobalRdrEnv

        let env' = lookupGlobalRdrEnv env h'

        -- from the RewriteM thing
        -- let gre = HscTypes.mg_rdr_env Hole

        

        return $ GHC.nameModule h

        -- let xxx = lookupGRE_RdrName 

        -- return h










