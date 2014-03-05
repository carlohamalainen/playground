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

<p> After a few days of tinkering with the GHC API I came up with this code. It's not perfect, but it might be useful
for someone wanting to see examples of how to use the dynamic compilation capabilities of the GHC API. </p>

> {-# LANGUAGE CPP #-}
> {-# LANGUAGE Rank2Types #-}
>
> import Control.Applicative
> import Control.Monad
> import Control.Monad.Instances()
> import Data.Function (on)
> import Data.Generics hiding (typeOf)
> import Data.List
> import Data.Maybe
> import Data.Typeable()
> import Desugar()
> import DynFlags
> import FastString
> import GHC
> import GHC.Paths (libdir)
> import GHC.SYB.Utils
> import HscTypes
> import Name
> import Outputable
> import RdrName
> import System.Environment
> import TcRnTypes()
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
< g :: IO ()
< g = do
<     let (Just _, _) = (Just 3, Just 4)
< 
<     return ()
< 
< s = "boo" :: String
< t = Just 100 :: Maybe Int
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
>                                    , modImportedAs    :: String
>                                    } deriving (Show)
>
> {-
> ideclPkgQual :: Maybe FastString
> Package qualifier.
>
> ideclSource :: Bool
> True = {--} import
>
> ideclSafe :: Bool
> True => safe import
>
> ideclQualified :: Bool
> True => qualified
>
> ideclImplicit :: Bool
> True => implicit import (of Prelude)
>
> ideclAs :: Maybe ModuleName
> as Module
>
> ideclHiding :: Maybe (Bool, [LIE name])
> (True => hiding, names)
>
> -}
>
> toHaskellModule :: SrcLoc.Located (GHC.ImportDecl GHC.RdrName) -> HaskellModule
> toHaskellModule idecl = HaskellModule name qualifier isImplicit hiding importedAs
>   where idecl'     = SrcLoc.unLoc idecl
>         name       = showSDoc tracingDynFlags (ppr $ GHC.ideclName $ idecl')
>         isImplicit = GHC.ideclImplicit idecl'
>         qualifier  = unpackFS <$> GHC.ideclPkgQual idecl'
>         hiding     = removeBrackets $ parseHiding $ GHC.ideclHiding idecl'
>         importedAs = showSDoc tracingDynFlags (ppr $ ideclAs idecl')
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
imported from. It turns out that we need the fully qualified name when we
do this lookup, otherwise we can induce a GHC panic like this: </p>

<pre>
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

<p> This isn't really a GHC bug, in my opinion. If we qualify the name that we are searching for then it works ok. </p>

> lookupSymbol :: String -> String -> String -> [String] -> IO [(Name, [GlobalRdrElt])]
> lookupSymbol targetFile targetModuleName qualifiedSymbol importList =
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
>         -- Bring in the target module and its imports.
>         setContext $ map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList)
>
>         -- Get the module summary, then parse it, type check it, and desugar it.
>         modSummary <- getModSummary $ mkModuleName targetModuleName :: Ghc ModSummary
>         p <- parseModule modSummary   :: Ghc ParsedModule
>         t <- typecheckModule p        :: Ghc TypecheckedModule
>         d <- desugarModule t          :: Ghc DesugaredModule
>
>         -- The "guts" has the global reader environment, which we need.
>         let guts = coreModule d            :: ModGuts
>             gre = HscTypes.mg_rdr_env guts :: GlobalRdrEnv
>
>         -- Beware that parseName expects an unambiguous symbol otherwise it causes a 
>         -- GHC panic. A fully qualified name should suffice. Is there a way to 
>         -- catch this exception?
>         names <- parseName qualifiedSymbol
>         let occNames = map nameOccName names                        :: [OccName]
>             occNamesLookups = map (lookupGlobalRdrEnv gre) occNames :: [[GlobalRdrElt]]
>
>         return $ zip names occNamesLookups

<p> It took a while to work out that I could get to <code>ModGuts</code> value, which lets us find out
the <code>Provenance</code> data (see below).
This tidbit from <a href="http://www.haskell.org/ghc/docs/7.6.3/html/libraries/ghc-7.6.3/src/RdrName.html">RdrName.lhs</a>
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


> -- http://stackoverflow.com/a/4978733
> 
> separateBy :: Eq a => a -> [a] -> [[a]]
> separateBy chr = unfoldr sep where
>   sep [] = Nothing
>   sep l  = Just . fmap (drop 1) . break (==chr) $ l


> postfixMatch originalSymbol qualifiedName = isPrefixOf (reverse endTerm) (reverse qualifiedName)
>   where endTerm = last $ separateBy '.' originalSymbol

<p> A basic <code>main</code> so that we can use this as a command line utility: </p>

> -- main :: IO ()
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
>   importList <- (map (modName . toHaskellModule)) <$> getImports targetFile targetModule
>   importListRaw <- getImports targetFile targetModule
>
>   forM_ importListRaw $ \x -> putStrLn $ "  " ++ (showSDoc tracingDynFlags (ppr $ x))
>   putStrLn ""
>
>   qnames <- qualifiedName targetFile targetModule lineNo colNo importList :: IO [String]
>
>   putStrLn "<qnames>"
>   forM_ qnames putStrLn
>   putStrLn "</qnames>"
>   putStrLn ""
>
>   let postMatches = filter (postfixMatch symbol) qnames :: [String]
> 
>       symbol' = if postMatches == [] then symbol else minimumBy (compare `on` length) postMatches -- Flaky!
>
>   putStrLn $ "symbol:  " ++ symbol
>   putStrLn $ "symbol': " ++ symbol'
>
>       
>   let maybeExtraModule = moduleOfQualifiedName symbol'
>
>       importList' = if symbol == symbol' then importList else importList ++ [fromJust maybeExtraModule]
>
>   putStrLn $ "try to match on: " ++ (show (symbol, qnames))
>   putStrLn $ "postMatches: " ++ (show postMatches)
>   putStrLn $ "importlist': " ++ (show importList')
>
>   x <- lookupSymbol targetFile targetModule symbol' importList'
>
>   forM_ x $ \(name, lookUp) -> do putStrLn $ "file:          " ++ targetFile
>                                   putStrLn $ "module:        " ++ targetModule
>                                   putStrLn $ "symbol:        " ++ symbol'
>                                   putStrLn $ "imports:       " ++ (show importList')
>
>                                   let definedIn    = symbolDefinedIn name
>                                       importedFrom = map symbolImportedFrom lookUp
>
>                                   putStrLn $ "defined in:    " ++ (showSDoc tracingDynFlags (ppr $ definedIn))
>                                   putStrLn $ "imported from: " ++ (showSDoc tracingDynFlags (ppr $ importedFrom))

> -- FIXME total hack, should deconstruct the name properly?
> moduleOfQualifiedName :: String -> Maybe String
> moduleOfQualifiedName qn = if moduleBits == []
>                                       then Nothing
>                                       else Just $ concat $ intersperse "." moduleBits
>   where moduleBits = reverse $ drop 1 $ reverse $ separateBy '.' qn -- FIXME is this ok?



<p> There is one problem with this code, in that if the symbol is
ambiguous then it causes a panic. For example in one of my other
Haskell projects I can't look up <code>many</code>: </p>

<pre>
$ guts S3Checksums.hs S3Checksums many

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


> -- qualifiedName :: String -> String -> Int -> Int -> [String] -> IO [String]
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
>             bs = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
>             es = listifySpans tcs (lineNo, colNo) :: [LHsExpr Id]
>             ps = listifySpans tcs (lineNo, colNo) :: [LPat Id]
>
>         let blahToString x = showSDoc tracingDynFlags $ ppr x
>             bsStrings = map blahToString bs
>             esStrings = map blahToString es
>             psStrings = map blahToString ps
>
>         return $ bsStrings ++ esStrings ++ psStrings
>







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


