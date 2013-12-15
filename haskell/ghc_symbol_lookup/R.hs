{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
import GHC
import GHC.Paths -- ghc-paths package
import Outputable
import GhcMonad

main :: IO ()
main = runGhc (Just libdir) $ goModule "Data.Map"

-- goModule :: GhcMonad m => String -> m ()
goModule modStr = do
  df <- getSessionDynFlags
  _ <- setSessionDynFlags df  
  -- ^ Don't know if this is the correct way, but it works for this purpose

  setContext [IIDecl (simpleImportDecl (mkModuleName modStr))]
  infos <- mapM getInfo =<< getNamesInScope 

  return infos
{-
  let ids = onlyIDs infos
  liftIO . putStrLn . showSDoc . render $ ids 

onlyIDs :: [Maybe (TyThing, Fixity, [Instance])] -> [Id]
onlyIDs infos = [ i | Just (AnId i, _, _) <- infos ] 

render :: [Id] -> SDoc
render ids = mkFields ids $$ text "------------" $$ mkInits ids 

mkFields :: [Id] -> SDoc
mkFields = vcat . map (\i ->
  text "," <+> pprUnqual i <+> text "::" <+> ppr (idType i))

mkInits :: [Id] -> SDoc
mkInits = vcat . map (\i ->
  text "," <+> pprUnqual i <+> text "=" <+> ppr i)


-- * Helpers

withUnqual :: SDoc -> SDoc
withUnqual  = withPprStyle (mkUserStyle neverQualify AllTheWay)

pprUnqual :: Outputable a => a -> SDoc
pprUnqual = withUnqual . ppr

-}
