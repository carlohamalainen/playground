<p> A few days ago
<a href="http://www.haskell.org/pipermail/haskell-cafe/2013-December/111778.html">I posted to Haskell Cafe</a>
asking if there was a way, given a 
Haskell file and a string like <code>head</code>, to find the correct path to the Haddock html documentation for that thing, be it 
a type, function, and so on.
In that thread, <a href="http://ro-che.info/">Roman Cheplyaka</a>
pointed out that I should be asking for where a thing is <i>imported
from</i>, as opposed to <i>defined</i>.  

<p> For example, <code>head</code> is defined in <code>GHC.List</code> which is in the <code>base</code> package, but it is exported
from <code>Prelude</code>. </p>

<p> After a few days of tinkering with the GHC API I came up with this: </p>

> {-# LANGUAGE CPP #-}
> {-# LANGUAGE Rank2Types #-}
>
> import Control.Applicative
> import Control.Monad
> import DynFlags
> import FastString
> import GHC
> import GHC.Paths (libdir)
> import HscTypes
> import Name
> import Outputable
> import RdrName
> import System.Environment
> import Data.Typeable
> import GHC.SYB.Utils
> import Data.Generics hiding (typeOf)
> import Data.Maybe
> import TcRnTypes
> import qualified CoreMonad
> import Desugar
> import Data.List
> import Control.Monad.Instances
>
> import qualified SrcLoc

<p> Suppose that we have a module <code>Muddle</code> in the file <code>Muddle.hs</code>: </p>

< -- Muddle.hs
<
< module Muddle where
<
< import Data.Maybe
<
< f :: a -> Maybe a
< f x = Just x
<
< s = "boo" :: String
<
< main = print "Hello, World!"

<p> We want the list of imports, which in this case
will be the implicit <code>Prelude</code> import along with
<code>Data.Maybe</code>. To do this we use <code>runGhc</code> to run
a session that loads the target, sets the context, and then extracts
the module summary: </p>

> getImports :: FilePath -> String -> IO [SrcLoc.Located (ImportDecl RdrName)]
> getImports targetFile targetModuleName =
>   defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
>       runGhc (Just libdir) $ do
>           -- Set the dynamic flags for the session.
>           dflags <- getSessionDynFlags
>           -- FIXME Do we need Opt_ImplicitPrelude? Maybe we should check
>           -- if the targetFile has an implicit prelude option set?
>           let dflags' = foldl xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
>           setSessionDynFlags dflags' { hscTarget = HscInterpreted, ghcLink = LinkInMemory }
>
>           -- Load the target file (e.g. "Muddle.hs").
>           target <- guessTarget targetFile Nothing
>           setTargets [target]
>           load LoadAllTargets
>
>           -- Set the context by loading the module, e.g. "Muddle" which is in "Muddle.hs".
>           setContext [(IIDecl . simpleImportDecl . mkModuleName) targetModuleName]
>
>           -- Extract the module summary and the *textual* imports.
>           modSum <- getModSummary $ mkModuleName targetModuleName
>
>           return $ ms_textual_imps modSum

<p> There is no <code>Show</code>
instance for <code>(GenLocated SrcSpan (ImportDecl RdrName))</code> so instead we must
use <code>showSDoc tracingDynFlags $ ppr</code> to get a string representation: </p>

<pre>
*Guts> imps <- getImports "Muddle.hs" "Muddle"
*Guts> showSDoc tracingDynFlags $ ppr imps
"[import (implicit) Prelude, import Data.Maybe]"
</pre>

<p> For convenience we'll convert a subset of the information in a
<code>SrcLoc.Located (ImportDecl RdrName)</code> to our own data structure.

> data HaskellModule = HaskellModule { modName          :: String
>                                    , modQualifier     :: Maybe String
>                                    , modIsImplicit    :: Bool
>                                    , modHiding        :: [String]
>                                    } deriving (Show)
>
> toHaskellModule :: SrcLoc.Located (GHC.ImportDecl GHC.RdrName) -> HaskellModule
> toHaskellModule idecl = HaskellModule name qualifier isImplicit hiding
>   where idecl'     = SrcLoc.unLoc idecl
>         name       = showSDoc tracingDynFlags (ppr $ GHC.ideclName $ idecl')
>         isImplicit = GHC.ideclImplicit idecl'
>         qualifier  = unpackFS <$> GHC.ideclPkgQual idecl'
>         hiding     = removeBrackets $ parseHiding $ GHC.ideclHiding idecl'
>
>         removeBrackets :: [a] -> [a]
>         removeBrackets [] = []
>         removeBrackets x = reverse . tail . reverse . tail $ x
>
>         grabNames :: GHC.Located (GHC.IE GHC.RdrName) -> String
>         grabNames loc = showSDoc tracingDynFlags (ppr names)
>           where names = GHC.ieNames $ SrcLoc.unLoc loc
> 
>         parseHiding :: Maybe (Bool, [Located (IE RdrName)]) -> [String]
>         parseHiding Nothing = []
>         parseHiding (Just (False, _)) = error "This should not happen???"
>         parseHiding (Just (True, h))  = map grabNames h

<p> Example of usage: </p>

<pre>
*Guts> imps <- getImports "Muddle.hs" "Muddle"
*Guts> map toHaskellModule imps
[ HaskellModule {modName = "Prelude",    modQualifier = Nothing, modIsImplicit = True,  modHiding = []}
, HaskellModule {modName = "Data.Maybe", modQualifier = Nothing, modIsImplicit = False, modHiding = []}
]
</pre>

<p> Next we need to be able to look up a string in the current context,
parse it to a <code>Name</code> (GHC's internal representation for
something) and then work out where it is defined and where it is
imported from. </p>

> lookupSymbol :: String -> String -> String -> [String] -> IO [(Name, [GlobalRdrElt])]
> lookupSymbol targetFile targetModuleName symbol importList =
>     defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
>       runGhc (Just libdir) $ do
>         dflags <- getSessionDynFlags
>         let dflags' = foldl xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
>         setSessionDynFlags dflags' { hscTarget = HscInterpreted, ghcLink = LinkInMemory }
>
>         target <- guessTarget targetFile Nothing
>         setTargets [target]
>         load LoadAllTargets
>
>         setContext $ map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList)
>
>         modSummary <- getModSummary $ mkModuleName targetModuleName :: Ghc ModSummary
>         p <- parseModule modSummary   :: Ghc ParsedModule
>         t <- typecheckModule p        :: Ghc TypecheckedModule
>         d <- desugarModule t          :: Ghc DesugaredModule
>
>         let guts = coreModule d            :: ModGuts
>             gre = HscTypes.mg_rdr_env guts :: GlobalRdrEnv
>
>         -- FIXME this can cause a GHC panic if the name is ambiguous
>         names <- parseName symbol
>         let occNames = map nameOccName names                        :: [OccName]
>             occNamesLookups = map (lookupGlobalRdrEnv gre) occNames :: [[GlobalRdrElt]]
>
>         return $ zip names occNamesLookups

<p> It took a while to work out that I could get to <code>ModGuts</code> value, which lets us find out
the <code>Provenance</code> data (see below). This tidbit from <a href="http://www.haskell.org/ghc/docs/7.6.3/html/libraries/ghc-7.6.3/src/RdrName.html">RdrName.lhs</a>
in the GHC source code was very helpful: </p>

< pprNameProvenance :: GlobalRdrElt -> SDoc
< -- ^ Print out the place where the name was imported
< pprNameProvenance (GRE {gre_name = name, gre_prov = LocalDef})
<   = ptext (sLit "defined at") <+> ppr (nameSrcLoc name)
< pprNameProvenance (GRE {gre_name = name, gre_prov = Imported whys})
<   = case whys of
<       (why:_) | opt_PprStyle_Debug -> vcat (map pp_why whys)
<               | otherwise          -> pp_why why
<       [] -> panic "pprNameProvenance"
<   where
<     pp_why why = sep [ppr why, ppr_defn_site why name]

<p> Finally we have the two functions of interest: </p>

> -- Module in which a name is *defined*.
> symbolDefinedIn :: Name -> Module
> symbolDefinedIn occname = nameModule occname
>
> -- List of possible modules which have resulted in 
> -- the name being in the current scope. Using a
> -- global reader we get the provenance data and then
> -- get the list of import specs.
> symbolImportedFrom :: GlobalRdrElt -> [ModuleName]
> symbolImportedFrom occNameLookup = map importSpecModule whys
>   where prov = gre_prov occNameLookup :: Provenance
>         Imported whys = prov
>         _ = whys :: [ImportSpec] -- dummy binding so we can see that 'whys' has this type, just for documentation

<p> A basic <code>main</code> so that we can use this as a command line utility: </p>

> main :: IO ()
> main = do
>   args <- getArgs
>
>   -- quick and dirty argument parsing, no error checking
>   let targetFile     = args !! 0
>       targetModule   = args !! 1
>       symbol         = args !! 2
>       lineNo         = (read $ args !! 3) :: Int
>       colNo          = (read $ args !! 4) :: Int
>
>   _importList <- (map (modName . toHaskellModule)) <$> getImports targetFile targetModule
>
>   qn <- qualifiedName targetFile targetModule lineNo colNo _importList
>   let possibleExtraImport = moduleOfQualifiedName <$> qn
>       qualifiedNamePart   = nameOfQualifiedName   <$> qn
>
>   -- when (symbol /= qualifiedNamePart) (error $ "derp: " ++ symbol ++ " /= " ++ qualifiedNamePart)
>
>   -- FIXME clunky, use catMaybes or whatever.
>   let importList = if isJust possibleExtraImport then _importList ++ [fromJust possibleExtraImport] else _importList
>
>   -- FIXME assert end of qn == symbol
>
>   -- x <- lookupSymbol targetFile targetModule symbol importList
>
>   let symbol' = if isJust qn then fromJust qn else symbol
>   x <- lookupSymbol targetFile targetModule symbol' importList
>
>   forM_ x $ \(name, lookUp) -> do putStrLn $ "file:          " ++ targetFile
>                                   putStrLn $ "module:        " ++ targetModule
>                                   putStrLn $ "symbol:        " ++ symbol'
>                                   putStrLn $ "imports:       " ++ (show importList)
>
>                                   let definedIn    = symbolDefinedIn name
>                                       importedFrom = map symbolImportedFrom lookUp
>
>                                   putStrLn $ "defined in:    " ++ (showSDoc tracingDynFlags (ppr $ definedIn))
>                                   putStrLn $ "imported from: " ++ (showSDoc tracingDynFlags (ppr $ importedFrom))

> -- FIXME total hack, should deconstruct the name properly.
> moduleOfQualifiedName :: String -> String
> moduleOfQualifiedName qn = concat $ intersperse "." $ reverse $ tail $ reverse $ separateBy '.' qn

> -- FIXME total hack, should deconstruct the name properly.
> nameOfQualifiedName :: String -> String
> nameOfQualifiedName qn = last $ separateBy '.' qn

> -- copied from http://stackoverflow.com/a/4978733
> separateBy :: Eq a => a -> [a] -> [[a]]
> separateBy chr = unfoldr sep' where
>   sep' [] = Nothing
>   sep' l  = Just . fmap (drop 1) . break (==chr) $ l

<pre>

$ ghc --make guts.lhs

$ ./guts Muddle.hs Muddle head
file:          Muddle.hs
module:        Muddle
symbol:        head
imports:       ["Prelude","Data.Maybe"]
defined in:    base:GHC.List
imported from: [[Prelude]]

$ ./guts Muddle.hs Muddle String
file:          Muddle.hs
module:        Muddle
symbol:        String
imports:       ["Prelude","Data.Maybe"]
defined in:    base:GHC.Base
imported from: [[Prelude]]

$ ./guts Muddle.hs Muddle Int
file:          Muddle.hs
module:        Muddle
symbol:        Int
imports:       ["Prelude","Data.Maybe"]
defined in:    ghc-prim:GHC.Types
imported from: [[Prelude]]

$ ./guts Muddle.hs Muddle fromJust
file:          Muddle.hs
module:        Muddle
symbol:        fromJust
imports:       ["Prelude","Data.Maybe"]
defined in:    base:Data.Maybe
imported from: [[Data.Maybe]]

$ ./guts Muddle.hs Muddle Just
file:          Muddle.hs
module:        Muddle
symbol:        Just
imports:       ["Prelude","Data.Maybe"]
defined in:    base:Data.Maybe
imported from: [[Data.Maybe, Prelude]]
</pre>


<p> In the first three cases we would point the user at
<a href="https://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html">base-4.6.0.1/docs/Prelude.html</a>
instead of <code>GHC.List</code>, <code>GHC.base</code>, or <code>GHC.Types</code>.
The fourth case, looking up <code>fromJust</code> is straightforward
- the function comes from <code>Data.Maybe</code> which is in the
<code>base</code> package. The final case, looking for the constructor <code>Just</code> shows that it comes from two packages.
Since the user explicitly imported <code>Data.Maybe</code> it would be preferable to point them
at <a href="http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Maybe.html">base-4.6.0.1/docs/Data-Maybe.html</a> instead
of <a href="https://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html">base-4.6.0.1/docs/Prelude.html</a>.


<p> There is one problem with this code, in that if the symbol is
ambiguous then it causes a panic. For example in one of my other
Haskell projects I can't look up <code>many</code>: </p>

<pre>
$ guts S3Checksums.hs S3Checksums many
guts: panic! (the 'impossible' happened)
  (GHC version 7.6.3 for x86_64-unknown-linux):
	Ambiguous occurrence `many'
It could refer to either `Text.Parsec.Prim.many',
                         imported from `Text.Parsec'
                         (and originally defined in `parsec-3.1.4:Text.Parsec.Prim')
                      or `Control.Applicative.many',
                         imported from `Control.Applicative'


Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
</pre>

<p> This isn't really a GHC bug, in my opinion. If we qualify the name that we are searching for then it works ok: </p>

<pre>
$ guts S3Checksums.hs S3Checksums Text.Parsec.many
file:          S3Checksums.hs
module:        S3Checksums
symbol:        Text.Parsec.many
imports:       ["Prelude","Utils","Text.Parsec.String","Text.Parsec","System.Process","System.IO"
               ,"System.FilePath.Posix","System.Directory","Data.Map","Data.Functor.Identity"
               ,"Control.Monad.State","Control.Monad","Control.Applicative"]
defined in:    parsec-3.1.4:Text.Parsec.Prim
imported from: [[Text.Parsec]]
</pre>


> -- listifySpans and listifyStaged are copied from ghcmod/Language/Haskell/GhcMod/Info.hs
> listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
> listifySpans tcs lc = listifyStaged TypeChecker p tcs
>   where
>     p (L spn _) = isGoodSrcSpan spn && spn `spans` lc
>
> listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
> listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))


> qualifiedName :: String -> String -> Int -> Int -> [String] -> IO (Maybe String)
> qualifiedName targetFile targetModuleName lineNo colNo importList =
>     defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
>       runGhc (Just libdir) $ do
>         dflags <- getSessionDynFlags
>         let dflags' = foldl xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
>         setSessionDynFlags dflags' { hscTarget = HscInterpreted, ghcLink = LinkInMemory }
>
>         target <- guessTarget targetFile Nothing
>         setTargets [target]
>         load LoadAllTargets
>
>         setContext $ map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList)
>
>         modSummary <- getModSummary $ mkModuleName targetModuleName :: Ghc ModSummary
>         p <- parseModule modSummary   :: Ghc ParsedModule
>         t <- typecheckModule p        :: Ghc TypecheckedModule
>
>         let TypecheckedModule{tm_typechecked_source = tcs} = t
>             -- bs = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
>             es = listifySpans tcs (lineNo, colNo) :: [LHsExpr Id]
>             -- ps = listifySpans tcs (lineNo, colNo) :: [LPat Id]
>         
>         -- Why is it correct to take the last thing from
>         -- this list? What is listifySpans doing?
>
>         if length es == 0 then return Nothing
>                           else return $ Just (showSDoc tracingDynFlags (ppr $ last es))
>         

