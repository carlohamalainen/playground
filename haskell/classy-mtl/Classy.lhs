
<p> This post has a minimal stand-alone example
of the classy lenses and prisms from <a href="http://twitter.com/GeorgeTalksCode">George Wilson's</a> <a href="http://talks.bfpg.org/talks/2015-06-09.next_level_mtl.html">talk</a> about mtl. The source code for George's talk is here: <a href="https://github.com/gwils/next-level-mtl-with-classy-optics">https://github.com/gwils/next-level-mtl-with-classy-optics</a>. </p>

<p> Literate Haskell source for this post is here: <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/classy-mtl">https://github.com/carlohamalainen/playground/tree/master/haskell/classy-mtl</a>. </p>

<p> First, some imports: </p>

> {-# LANGUAGE OverloadedStrings    #-}
> {-# LANGUAGE TemplateHaskell      #-}
>
> module Classy where
>
> import Control.Lens
> import Control.Monad.Except
> import Control.Monad.Reader
> import Data.Text

<h2> Toy program - uses the network and a database </h2>

<p> The case study in George's talk was a program that has to interact with a database
and the network. We have a type for the database connection info: </p>

> type DbConnection = Text
> type DbSchema     = Text
>
> data DbConfig = DbConfig
>     { _dbConn :: DbConnection
>     , _schema :: DbSchema
>     }

<p> For the network we have a port and some kind of SSL setting: </p>

> type Port = Integer
> type Ssl  = Text
>
> data NetworkConfig = NetworkConfig
>     { _port     :: Port
>     , _ssl      :: Ssl
>     }

<p> At the top level, our application has a database and a network configuration: </p>

> data AppConfig = AppConfig
>     { _appDbConfig   :: DbConfig
>     , _appNetConfig  :: NetworkConfig
>     }

<p> Types for errors that we see when dealing with the database and the network: </p>

> data DbError = QueryError Text | InvalidConnection
>
> data NetworkError = Timeout Int | ServerOnFire
>
> data AppError = AppDbError  { dbError  :: DbError      }
>               | AppNetError { netError :: NetworkError }

<h2> Classy lenses and prisms </h2>

<p> Use Template Haskell to make all of the classy lenses and prisms. Documentation for <code>makeClassy</code>
and <code>makeClassyPrisms</code> is in <a href="https://hackage.haskell.org/package/lens-4.11/docs/Control-Lens-TH.html">Control.Lens.TH</a>. </p>

> makeClassy ''DbConfig
> makeClassy ''NetworkConfig
> makeClassy ''AppConfig
>
> makeClassyPrisms ''DbError
> makeClassyPrisms ''NetworkError
> makeClassyPrisms ''AppError

<p> We get the following typeclasses: </p>

<ul>

<li> <code>HasDbConfig</code> </li>
<li> <code>HasNetworkConfig</code> </li>
<li> <code>HasAppConfig</code> </li>

<li> <code>AsNetworkError</code> </li>
<li> <code>AsDbError</code> </li>
<li> <code>AsAppError</code> </li>

</ul>

<p> For example, here is the generated class <code>HasDbConfig</code>: </p>

<pre>
*Classy> :i HasDbConfig
class HasDbConfig c_a6IY where
  dbConfig :: Functor f => (DbConfig -> f DbConfig) -> c0 -> f c0
  dbConn   :: Functor f => (DbConnection -> f DbConnection) -> c0 -> f c0
  schema   :: Functor f => (DbSchema -> f DbSchema) -> c0 -> f c0
instance HasDbConfig DbConfig -- Defined at Classy.lhs:58:3
</pre>

<p> If we write <code>HasDbConfig r</code> in the class constraints of a type signature then
we can use the lenses <code>dbConfig</code>, <code>dbConn</code>, and <code>schema</code> to get the entire config, connection string, and schema,
from something of type <code>r</code>. </p>

<p> In contrast, the constraint <code>AsNetworkError r</code> means that we can
use the prisms <code>_NetworkError</code>, <code>_Timeout</code>, and <code>_ServerOnFire</code>
on a value of type <code>r</code> to get at the network error details. </p>

<pre>
*Classy> :i AsNetworkError
class AsNetworkError r_a759 where
  _NetworkError ::
    (Choice p, Control.Applicative.Applicative f) =>
    p NetworkError (f NetworkError) -> p r0 (f r0)

  _Timeout ::
    (Choice p, Control.Applicative.Applicative f) =>
    p Int (f Int) -> p r0 (f r0)

  _ServerOnFire ::
    (Choice p, Control.Applicative.Applicative f) =>
    p () (f ()) -> p r0 (f r0)
  	-- Defined at Classy.lhs:63:3

instance AsNetworkError NetworkError -- Defined at Classy.lhs:63:3
</pre>

<h2> Using the class constraints </h2>

<p> The first function is <code>loadFromDb</code> which uses a reader environment for database
configuration, can throw a database error, and do IO actions. </p>

> loadFromDb :: ( MonadError e m,
>                 MonadReader r m,
>                 AsDbError e,
>                 HasDbConfig r,
>                 MonadIO m) => m Text
> loadFromDb = do
>
>   -- Due to "MonadReader r m" and "HasDbConfig r"
>   -- we can ask for the database config:
>   rdr <- ask
>   let dbconf  = rdr ^. dbConfig :: DbConfig
>
>   -- We can ask for the connection string directly:
>   let connstr  = rdr ^. dbConn :: DbConnection
>
>   -- We have "AsDbError e", so we can throw a DB error:
>   throwError $ (_InvalidConnection #) ()
>   throwError $ (_QueryError #) "Bad SQL!"
>
>   return "foo"

<p> Another function, <code>sendOverNet</code> uses a reader environment with a network config,
throws network errors, and does IO actions. </p>

> sendOverNet :: ( MonadError e m,
>                  MonadReader r m,
>                  AsNetworkError e,
>                  AsAppError e,
>                  HasNetworkConfig r,
>                  MonadIO m) => Text -> m ()
> sendOverNet mydata = do
>
>   -- We have "MonadReader r m" and "HasNetworkConfig r"
>   -- so we can ask about the network config:
>   rdr <- ask
>   let netconf = rdr ^. networkConfig  :: NetworkConfig
>       p       = rdr ^. port           :: Port
>       s       = rdr ^. ssl            :: Ssl
>
>   liftIO $ putStrLn $ "Pretending to connect to the network..."
>
>   -- We have "AsNetworkError e" so we can throw a network error:
>   throwError $ (_NetworkError #) (Timeout 100)
>
>   -- We have "AsAppError e" so we can throw an application-level error:
>   throwError $ (_AppNetError #) (Timeout 100)
>
>   return ()

<p> If we load from the database and also send over the network then we get extra class constraints: </p>

> loadAndSend :: ( AsAppError e,
>                  AsNetworkError e,
>                  AsDbError e,
>                  HasNetworkConfig r,
>                  HasDbConfig r,
>                  MonadReader r m,
>                  MonadError e m,
>                  MonadIO m) => m ()
> loadAndSend = do
>   liftIO $ putStrLn "Loading from the database..."
>   t <- loadFromDb
>
>   liftIO $ putStrLn "Sending to the network..."
>   sendOverNet t

<h2> Things that won't compile </h2>

<p> We can't throw the database error <code>InvalidConnection</code>
without the right class constraint: </p>

< nope1 :: (MonadError e m, AsNetworkError e) => m ()
< nope1 = throwError $ (_InvalidConnection #) ()

<font color="red">
<pre>
Could not deduce (AsDbError e)
arising from a use of ‘_InvalidConnection’
</pre> </font>

<p> We can't throw an application error if we are only allowed to
throw network errors, even though this specific application error is
a network error: </p>

< nope2 :: (MonadError e m, AsNetworkError e) => m ()
< nope2 = throwError $ (_AppNetError #) (Timeout 100)

<font color="red">
<pre>
Could not deduce (AsAppError e)
arising from a use of ‘_AppNetError’
</pre> </font>

<p> We can't get the network config from a value of type <code>r</code> if we only have
the constraint about having the database config: </p>

< nope3 :: (MonadReader r m, HasDbConfig r) => m ()
< nope3 = do
<   rdr <- ask
<   let netconf = rdr ^. networkConfig
<
<   return ()

<font color="red">
<pre>
Could not deduce (HasNetworkConfig r)
arising from a use of ‘networkConfig’
</pre> </font>


<h2>What is the #?</h2>

The <code>#</code> is an infix alias for <a href="https://hackage.haskell.org/package/lens-4.11/docs/Control-Lens-Review.html#v:review">review</a>. More details are in <a href="https://hackage.haskell.org/package/lens-4.11/docs/Control-Lens-Review.html">Control.Lens.Review</a>. </p>


<pre>
*Classy> :t review _InvalidConnection ()
review _InvalidConnection () :: AsDbError e => e

*Classy> :t throwError $ review _InvalidConnection ()
throwError $ review _InvalidConnection () :: (AsDbError e, MonadError e m) => m a
</pre>

<h2>What is the monad transformer stack?</h2>

<p> We didn't specify it! The functions <code>loadFromDb</code>
and <code>sendOverNet</code> have the general monad <code>m</code>
in their type signatures, not a specific transformer stack like <code>ReaderT AppConfig (ExceptT AppError IO) a</code>. </p>

<h2>What else?</h2>

<p> <a href="http://twitter.com/benkolera">Ben Kolera</a> did a
talk at <a href="http://bfpg.org">BFPG</a> about <a href="http://talks.bfpg.org/talks/2015-02-24.monad_transformers.html">stacking monad transformers</a>.
He later modified the code from his talk to use the
classy lens/prism approach. You can see the code <a href="https://github.com/benkolera/talk-stacking-your-monads/tree/master/code">before</a>
and <a href="https://github.com/benkolera/talk-stacking-your-monads/tree/master/code-classy">after</a>, and also see a <a href="https://github.com/benkolera/talk-stacking-your-monads/blob/master/classy.diff">diff</a>. As far as I could see
there is <a href="https://github.com/benkolera/talk-stacking-your-monads/blob/master/code-classy/src/Csv.hs#L60">one spot</a> in the code where an error is thrown, which motivated me to create the stand-alone example in this post with the body for <code>loadFromDb</code> and <code>sendOverNet</code> sketched out. </p>






