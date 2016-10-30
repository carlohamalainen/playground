<p> I was looking at an old post of mine
on <a href="https://carlo-hamalainen.net/blog/2014/6/7/notes-on-free-monads">free monads</a>
and wondered what it would look like using <a href="https://hackage.haskell.org/package/operational">operational</a>. Here we go! </p>

<p> (Literate Haskell source for this blog post
is <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/note-on-operational">here</a>.) </p>

<p> First, some imports. We use <a href="https://wiki.haskell.org/GADTs_for_dummies">GADTs</a>
for our instruction type. </p>

> {-# LANGUAGE GADTs #-}
>
> module Note where
>
> import Control.Monad
> import Control.Monad.Operational
> import Test.QuickCheck
> import qualified Data.Map as M

<p> We want to encode a few instructions that operate on a data store. For
the moment we don't care about the implementation, just the underlying
actions that we can perform: create a value, list all values, retrieve
a particular value, and delete a value. </p>

<p> Suppose we have an index of type <code>i</code> and values
of type <code>v</code>. Then <code>DataStoreInstruction</code> is: </p>

> data DataStoreInstruction i v a where
>     Create   :: v -> DataStoreInstruction i v i
>     List     ::      DataStoreInstruction i v [v]
>     Retrieve :: i -> DataStoreInstruction i v (Maybe v)
>     Delete   :: i -> DataStoreInstruction i v ()

<p> Intuitively, <code>Create</code> takes a value of type <code>v</code>
and returns an instruction, which on interpretation gives a value
of type <code>i</code> (the last type parameter). </p>

<p><code>List</code> doesn't take any argument and produces a list of
type <code>v</code>, i.e. <code>[v]</code>. The only odd one is <code>Delete</code>
which doesn't return anything, so it has a <code>()</code> in the last type variable.</p>

<p> A sequence of <code>DataStoreInstruction</code>s is a program, which we get
using the <a href="https://hackage.haskell.org/package/operational-0.2.3.4/docs/Control-Monad-Operational.html#t:Program">Program</a> constructor: </p>

> type DataStoreProgram i v a = Program (DataStoreInstruction i v) a

<p> where the index has type <code>i</code>, the values
have type <code>v</code>, and the overall result of the program has type <code>a</code>. <p>

<p> To more easily construct programs, use <a href="https://hackage.haskell.org/package/operational-0.2.3.4/docs/Control-Monad-Operational.html#v:singleton">singleton</a>: </p>

> create :: v -> DataStoreProgram i v i
> create = singleton . Create
>
> list :: DataStoreProgram i v [v]
> list = singleton List
>
> retrieve :: i -> DataStoreProgram i v (Maybe v)
> retrieve = singleton . Retrieve
>
> delete :: i -> DataStoreProgram i v ()
> delete = singleton . Delete

<p> Now we can write programs in this DSL! All the usual monad things are
at our disposal: </p>

> -- Insert a few values and return a list
> -- of all values:
> doSomeThings :: DataStoreProgram Int Int [Int]
> doSomeThings = do
>     ix3 <- create 3
>     ix4 <- create 4
>     delete ix3
>     ix5 <- create 5
>     list

> -- Insert all the supplied values and
> -- return a list of indexes as well as a
> -- list of final values (which should be empty).
> insertValues :: [Int] -> DataStoreProgram Int Int ([Int], [Int])
> insertValues xs = do
>     ixs <- forM xs create
>     forM_ ixs delete -- delete everything
>     final <- list
>
>     return (ixs, final)

<p> The last step is to write an interpreter. We do this
using <a href="https://hackage.haskell.org/package/operational-0.2.3.4/docs/Control-Monad-Operational.html#v:view">view</a> and pattern matching on the
constructors of <code>DataStoreInstruction</code>. We
use <a href="https://hackage.haskell.org/package/operational-0.2.3.4/docs/Control-Monad-Operational.html#v::-62--62--61-">:>>=</a>
to break apart the program. As the documentation says, <code>(someInstruction :>>= k)</code>
means <code>someInstruction</code> and the remaining program is given by the function <code>k</code>. </p>

<p> Here we interpret the program using a <code>Map</code> as the underlying data store: </p>

> interpretMap :: Ord a => DataStoreProgram a a b -> (M.Map a a -> b)
> interpretMap = eval . view
>   where
>     eval (Return x)          m = x
>     eval (Create   v :>>= k) m = interpretMap (k v)              (M.insert v v m)
>     eval (List       :>>= k) m = interpretMap (k (M.elems m))    m
>     eval (Retrieve i :>>= k) m = interpretMap (k $ M.lookup i m) m
>     eval (Delete   i :>>= k) m = interpretMap (k ())             (M.delete i m)

<p> If we wanted to we could flatten a program out to a string: </p>

> interpretString :: (Show a, Show b) => DataStoreProgram a a b -> String
> interpretString = eval . view
>   where
>     eval (Return x)          = "Return "   ++ show x
>     eval (Create   v :>>= k) = "Create("   ++ show v ++ "); "  ++ interpretString (k v)
>     eval (List       :>>= k) = "List; "                        ++ interpretString (k [])
>     eval (Retrieve i :>>= k) = "Retrieve(" ++ show i ++ "); "  ++ interpretString (k $ Just i)
>     eval (Delete   i :>>= k) = "Delete("   ++ show i ++ "); "  ++ interpretString (k ())

<p> Maybe we have some super-important business need for storing Int/Int key value maps
with a Postgresql backend. We could target this by writing an interpreter that
uses <a href="https://hackage.haskell.org/package/postgresql-simple-0.5.2.1/docs/Database-PostgreSQL-Simple.html">postgresql-simple</a>: </p>

> data Connection = Connection -- cheating for this example
>
> interprestPostgresql :: DataStoreProgram Int Int b -> (Connection -> IO b)
> interprestPostgresql = eval . view
>   where
>     eval (Return x)          _ = return x
>     eval (Create   v :>>= k) c = interprestPostgresql (k v) (insertPsql c v)
>     eval (List       :>>= k) c = do allValues <- getAllPsql c
>                                     interprestPostgresql (k allValues) c
>     eval (Retrieve i :>>= k) c = do v <- lookupPsql c i
>                                     interprestPostgresql (k v) c
>     eval (Delete   i :>>= k) c = deletePsql c i >> interprestPostgresql (k ()) c
>
>     -- Exercises for the reader.
>     insertPsql c v = undefined
>     getAllPsql c   = undefined
>     lookupPsql c i = undefined
>     deletePsql c i = undefined

<p> Running the programs: </p>

<pre>
*Note> interpretMap doSomeThings M.empty
[4,5]

*Note> interpretString doSomeThings
"Create(3); Create(4); Delete(3); Create(5); List; Return []"

*Note> interpretMap (insertValues [1..10]) M.empty
([1,2,3,4,5,6,7,8,9,10],[])

*Note> interpretString (insertValues [1..10])
"Create(1); Create(2); Create(3); Create(4); Create(5); Create(6);
 Create(7); Create(8); Create(9); Create(10); Delete(1); Delete(2);
 Delete(3); Delete(4); Delete(5); Delete(6); Delete(7); Delete(8);
 Delete(9); Delete(10); List; Return ([1,2,3,4,5,6,7,8,9,10],[])"
</pre>

<h3>QuickCheck</h3>

<p> It's always good to write some tests: </p>

> prop_insert_then_delete :: [Int] -> Bool
> prop_insert_then_delete xs = null $ interpretMap (f xs) M.empty
>   where
>     f :: [Int] -> DataStoreProgram Int Int [Int]
>     f is = do
>         ixs <- forM is create
>         forM_ ixs delete
>         list

> prop_create :: Positive Int -> Bool
> prop_create (Positive n) = let ns = [1..n] in ns == interpretMap (f ns) M.empty
>   where
>     f = mapM create

<pre>
*Note> quickCheck prop_insert_then_delete
+++ OK, passed 100 tests.

*Note>
*Note>
*Note>
*Note> quickCheck prop_create
+++ OK, passed 100 tests.
</pre>

<p> Over time we find out that the interpreter that uses <code>Map</code> is too slow,
so we write a new one using a fancy data structure: </p>

> -- Uses fancy tuned data structure.
> interpretMapFast :: Ord a => DataStoreProgram a a b -> (M.Map a a -> b)
> interpretMapFast = undefined -- So fancy!

<p> Now we can compare implementations using QuickCheck. Nice! </p>

> prop_fast_inserts :: [Int] -> Bool
> prop_fast_inserts xs = (interpretMapFast xs' M.empty) == (interpretMap xs' M.empty)
>   where
>     xs' = f xs
>
>     f :: [Int] -> DataStoreProgram Int Int [Int]
>     f is = do
>         ixs <- forM is create
>         list

<h2> Use of operational in larger projects </h2>

<p> Here are a few samples of the operational package in action. For more,
see the <a href="http://packdeps.haskellers.com/reverse/operational">reverse dependencies</a> on Hackage. </p>

<h3> Hadron </h3>

<p> I first heard about operational from this talk at the New York Haskell
meetup: <a href="https://vimeo.com/90189610">Conquering Hadoop with Haskell and Ozgun Ataman</a>.

<p> Here is the <code>ConI</code> type from <a href="https://github.com/Soostone/hadron/blob/master/src/Hadron/Controller.hs">Hadron.Controller</a>: </p>

< data ConI a where
<     Connect :: forall i o. MapReduce i o
<             -> [Tap i] -> Tap o
<             -> Maybe String
<             -> ConI ()
<
<     MakeTap :: Protocol' a -> ConI (Tap a)
<
<     BinaryDirTap
<         :: FilePath
<         -> (FilePath -> Bool)
<         -> ConI (Tap (FilePath, B.ByteString))
<
<     ConIO :: IO a -> ConI a
<
<     OrchIO :: IO a -> ConI ()
<     -- Only the orchestrator performs action
<
<     NodeIO :: IO a -> ConI a
<     -- Only the nodes perform action
<
<     SetVal :: String -> B.ByteString -> ConI ()
<
<     GetVal :: String -> ConI (Maybe B.ByteString)
<
<     RunOnce :: Serialize a => IO a -> ConI a
<     -- Only run on orchestrator, then make available to all the
<     -- nodes via HDFS.

<p> There is the distinction between the single orchestrator node, which
runs <code>OrchIO</code> and can't run <code>NodeIO</code>, and worker nodes that can't run <code>OrchIO</code>
but can run <code>NodeIO</code>. In the <code>orchestrate</code>, trying to evaluate
a <code>NodeIO</code> results in an error: </p>

< orchestrate
<     :: (MonadMask m, MonadIO m, Applicative m)
<     => Controller a
<     -> RunContext
<     -> RerunStrategy
<     -> ContState
<     -> m (Either String a)
< orchestrate (Controller p) settings rr s = do
<     bracket
<       (liftIO $ openFile "hadron.log" AppendMode)
<       (liftIO . hClose)
<       (\_h -> do echoInfo ()  "Initiating orchestration..."
<                  evalStateT (runExceptT (go p)) s)
<     where
<       go = eval . O.view
<
<       eval (Return a) = return a
<       eval (i :>>= f) = eval' i >>= go . f
<
<       eval' :: (Functor m, MonadIO m) => ConI a -> ExceptT String (StateT ContState m) a
<       eval' (ConIO  f) = liftIO f
<       eval' (OrchIO f) = void $ liftIO f
<       eval' (NodeIO _) = return (error "NodeIO can't be used in the orchestrator decision path")

<p> Meanwhile, worker nodes ignore an <code>OrchIO</code>
and <a href="https://github.com/Soostone/hadron/blob/8f78faecad49ac0cc35484a70c8b186ae916920b/src/Hadron/Controller.hs#L1022-L1038">lift the NodeIO action</a>. </p>


<h3> Chart </h3>

<p> The <a href="http://hackage.haskell.org/package/Chart-1.8/docs/Graphics-Rendering-Chart-Backend-Impl.html">Graphics.Rendering.Chart.Backend.Impl</a> module
defines the the backend instructions: </p>

< data ChartBackendInstr a where
<   StrokePath :: Path -> ChartBackendInstr ()
<   FillPath   :: Path -> ChartBackendInstr ()
<   GetTextSize :: String -> ChartBackendInstr TextSize
<   DrawText    :: Point -> String -> ChartBackendInstr ()
<   GetAlignments :: ChartBackendInstr AlignmentFns
<   WithTransform  :: Matrix ->  Program ChartBackendInstr a -> ChartBackendInstr a
<   WithFontStyle  :: FontStyle -> Program ChartBackendInstr a -> ChartBackendInstr a
<   WithFillStyle  :: FillStyle -> Program ChartBackendInstr a -> ChartBackendInstr a
<   WithLineStyle  :: LineStyle -> Program ChartBackendInstr a -> ChartBackendInstr a
<   WithClipRegion :: Rect -> Program ChartBackendInstr a -> ChartBackendInstr a
<
< type BackendProgram a = Program ChartBackendInstr a

<p> Then the <a href="https://hackage.haskell.org/package/Chart-cairo-1.8/docs/Graphics-Rendering-Chart-Backend-Cairo.html">Graphics.Rendering.Chart.Backend.Cairo</a>
module provides a way to run a <code>BackendProgram</code> for the cairo graphics engine: </p>

< runBackend' :: CEnv -> BackendProgram a -> C.Render a
< runBackend' env m = eval env (view m)
<   where
<     eval :: CEnv -> ProgramView ChartBackendInstr a -> C.Render a
<     eval env (Return v)= return v
<     eval env (StrokePath p :>>= f) = cStrokePath env p >>= step env f
<     eval env (FillPath p :>>= f) = cFillPath env p >>= step env f
<     eval env (GetTextSize s :>>= f) = cTextSize s >>= step env f
<     eval env (DrawText p s :>>= f) = cDrawText env p s >>= step env f
<     eval env (GetAlignments :>>= f) = return (ceAlignmentFns env) >>= step env f
<     eval env (WithTransform m p :>>= f) = cWithTransform env m p >>= step env f
<     eval env (WithFontStyle font p :>>= f) = cWithFontStyle env font p >>= step env f
<     eval env (WithFillStyle fs p :>>= f) = cWithFillStyle env fs p >>= step env f
<     eval env (WithLineStyle ls p :>>= f) = cWithLineStyle env ls p >>= step env f
<     eval env (WithClipRegion r p :>>= f) = cWithClipRegion env r p >>= step env f

<p> Meanwhile, <a href="https://hackage.haskell.org/package/Chart-diagrams-1.8/docs/Graphics-Rendering-Chart-Backend-Diagrams.html">Graphics.Rendering.Chart.Backend.Diagrams</a>
does the same but for <a href="https://hackage.haskell.org/package/diagrams-lib">diagrams</a>: </p>

< runBackend' tr m = eval tr $ view $ m
<   where
<     eval :: (D.Renderable (D.Path V2 (N b)) b, D.Renderable t b, D.TypeableFloat (N b))
<          => TextRender b t -> ProgramView ChartBackendInstr a
<          -> DState (N b) (D.QDiagram b V2 (N b) Any, a)
<     eval tr (Return v) = return (mempty, v)
<     eval tr (StrokePath p   :>>= f) = dStrokePath  p   <># step tr f
<     eval tr (FillPath   p   :>>= f) = dFillPath    p   <># step tr f
<     eval tr@TextRenderSvg    (DrawText   p s :>>= f) = dDrawTextSvg    p s <># step tr f
<     eval tr@TextRenderNative (DrawText   p s :>>= f) = dDrawTextNative p s <># step tr f
<     eval tr (GetTextSize  s :>>= f) = dTextSize      s <>= step tr f
<     eval tr (GetAlignments  :>>= f) = dAlignmentFns    <>= step tr f
<     eval tr (WithTransform m p :>>= f)  = dWithTransform  tr m  p <>= step tr f
<     eval tr (WithFontStyle fs p :>>= f) = dWithFontStyle  tr fs p <>= step tr f
<     eval tr (WithFillStyle fs p :>>= f) = dWithFillStyle  tr fs p <>= step tr f
<     eval tr (WithLineStyle ls p :>>= f) = dWithLineStyle  tr ls p <>= step tr f
<     eval tr (WithClipRegion r p :>>= f) = dWithClipRegion tr r  p <>= step tr f



<h3> redis-io </h3>

<p> The <a href="https://hackage.haskell.org/package/redis-resp-0.4.0/docs/Data-Redis-Command.html">Data.Redis.Command</a> module
defines heaps of commands: </p>

< data Command :: * -> * where
<     -- Connection
<     Ping   :: Resp -> Command ()
<     Echo   :: FromByteString a => Resp -> Command a
<     Auth   :: Resp -> Command ()
<     Quit   :: Resp -> Command ()
<     Select :: Resp -> Command ()
<
<     -- Many more here.
<
< type Redis  = ProgramT Command

<p> and <code>Database.Redis.IO.Client</code>, an internal module of <a href="https://hackage.haskell.org/package/redis-io">redis-io</a>, defines how
to interpret the redis commands: </p>

< eval f conn red = run conn [] red
<   where
<     run :: Connection -> [IO ()] -> Redis IO a -> IO (a, [IO ()])
<     run h ii c = do
<         r <- viewT c
<         case r of
<             Return a -> return (a, ii)
<
<             -- Connection
<             Ping   x :>>= k -> f h x (matchStr "PING" "PONG") >>= \(a, i) -> run h (i:ii) $ k a
<             Echo   x :>>= k -> f h x (readBulk "ECHO")        >>= \(a, i) -> run h (i:ii) $ k a
<             Auth   x :>>= k -> f h x (matchStr "AUTH" "OK")   >>= \(a, i) -> run h (i:ii) $ k a
<             Quit   x :>>= k -> f h x (matchStr "QUIT" "OK")   >>= \(a, i) -> run h (i:ii) $ k a
<             Select x :>>= k -> f h x (matchStr "SELECT" "OK") >>= \(a, i) -> run h (i:ii) $ k a
<
<             -- Many more here, snipped


<h3>language-puppet</h3>

<p> The internal module <code>Puppet.Interpreter.Types</code> of <a href="https://hackage.haskell.org/package/language-puppet">language-puppet</a>
defines an interpreter instruction type: </p>

< data InterpreterInstr a where
<     GetNativeTypes      :: InterpreterInstr (Container NativeTypeMethods)
<     GetStatement        :: TopLevelType -> Text -> InterpreterInstr Statement
<     ComputeTemplate     :: Either Text T.Text -> InterpreterState -> InterpreterInstr T.Text
<     ExternalFunction    :: Text -> [PValue] -> InterpreterInstr PValue
<     GetNodeName         :: InterpreterInstr Text
<     HieraQuery          :: Container Text -> T.Text -> HieraQueryType -> InterpreterInstr (Maybe PValue)
<     GetCurrentCallStack :: InterpreterInstr [String]
<     IsIgnoredModule     :: Text -> InterpreterInstr Bool
<     IsExternalModule    :: Text -> InterpreterInstr Bool
<     IsStrict            :: InterpreterInstr Bool
<     PuppetPaths         :: InterpreterInstr PuppetDirPaths
<     -- Many more here, snipped.


<p> Then <code>Puppet.Interpreter.IO</code> provides: </p>

< -- The internal (not exposed) eval function
< eval :: Monad m
<      => InterpreterReader m
<      -> InterpreterState
<      -> ProgramViewT InterpreterInstr (State InterpreterState) a
<      -> m (Either PrettyError a, InterpreterState, InterpreterWriter)
< eval _ s (Return x) = return (Right x, s, mempty)
< eval r s (a :>>= k) =
<     let runInstr = interpretMonad r s . k -- run one instruction
<         thpe = interpretMonad r s . throwPosError . getError
<         pdb = r^.readerPdbApi
<         strFail iof errf = iof >>= \case
<             Left rr -> thpe (errf (string rr))
<             Right x -> runInstr x
<         canFail iof = iof >>= \case
<             S.Left err -> thpe err
<             S.Right x -> runInstr x
<         canFailX iof = runExceptT iof >>= \case
<             Left err -> thpe err
<             Right x -> runInstr x
<         logStuff x c = (_3 %~ (x <>)) <$> c
<     in  case a of
<             IsStrict                     -> runInstr (r ^. readerIsStrict)
<             ExternalFunction fname args  -> case r ^. readerExternalFunc . at fname of
<                                                 Just fn -> interpretMonad r s ( fn args >>= k)
<                                                 Nothing -> thpe (PrettyError ("Unknown function: " <> ttext fname))
<             GetStatement topleveltype toplevelname
<                                          -> canFail ((r ^. readerGetStatement) topleveltype toplevelname)
<             ComputeTemplate fn stt       -> canFail ((r ^. readerGetTemplate) fn stt r)
<             WriterTell t                 -> logStuff t (runInstr ())
<             WriterPass _                 -> thpe "WriterPass"
<             WriterListen _               -> thpe "WriterListen"
<             PuppetPaths                  -> runInstr (r ^. readerPuppetPaths)
<             GetNativeTypes               -> runInstr (r ^. readerNativeTypes)
<             ErrorThrow d                 -> return (Left d, s, mempty)
<             ErrorCatch _ _               -> thpe "ErrorCatch"
<             GetNodeName                  -> runInstr (r ^. readerNodename)
<             -- More cases here, snipped.

<p> Since <code>InterpreterInstr</code> is a normal data type, it's possible to
write an instance of <code>Pretty</code> so that warnings or error messages
look nicer: </p>

< -- Puppet.Interpreter.PrettyPrinter
<
< instance Pretty (InterpreterInstr a) where
<
<     ...
<
<     pretty (ExternalFunction fn args)  = pf (ttext fn) (map pretty args)
<     pretty GetNodeName                 = pf "GetNodeName" []
<     pretty (HieraQuery _ q _)          = pf "HieraQuery" [ttext q]
<     pretty GetCurrentCallStack         = pf "GetCurrentCallStack" []
<     pretty (ErrorThrow rr)             = pf "ErrorThrow" [getError rr]
<     pretty (ErrorCatch _ _)            = pf "ErrorCatch" []
<     pretty (WriterTell t)              = pf "WriterTell" (map (pretty . view _2) t)
<     pretty (WriterPass _)              = pf "WriterPass" []

<h2> References </h2>

<p> <ul>
<li> <a href="http://apfelmus.nfshost.com/articles/operational-monad.html">The Operational Monad Tutorial</a> </li>

<li> <a href="https://hackage.haskell.org/package/operational">Control.Monad.Operational</a> </li>

<li> <a href="http://packdeps.haskellers.com/reverse/operational">operational's reverse dependencies</a> </li>

<li> <a href="https://vimeo.com/90189610">Conquering Hadoop with Haskell and Ozgun Ataman</a> </li>


 </ul></p>


