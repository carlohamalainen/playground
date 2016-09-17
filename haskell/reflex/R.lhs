<p> When I post a series of photos to a personal blog I find myself editing HTML in Vim and switching back and forth to
a browser to see if I have written comments in the right places and ordered the photos correctly. I could use
a HTML editor to do this, but why not try FRP with Haskell? :) Apparently I sort of use
FRP <a href="https://www.reddit.com/r/haskell/comments/41icy5/haskell_developer_roles_at_standard_chartered/cz37u9f">at work</a>
so trying out <a href="https://github.com/reflex-frp/reflex-platform">Reflex</a> wasn't too much of a leap. </p>

<p> This post is adapted from the <a href="https://github.com/reflex-frp/reflex-examples/tree/master/BasicTodo">todo list</a> and <a href="https://github.com/reflex-frp/reflex-examples/tree/master/drag-and-drop">drag 'n' drop</a> examples in <a href="https://github.com/reflex-frp/reflex-examples">this Reflex repo</a>. </p>

<p> Let's start with the types. My basic blob of data is an image (a URL), a comment about the image, and
a flag to indicate if the image should appear in the final output: </p>

< data Image = Image
<     { imageFileName :: T.Text   -- ^ e.g. "file:///tmp/cat.jpg"
<     , imageVisible  :: Bool     -- ^ Output in HTML render?
<     , imageRemark   :: T.Text   -- ^ Comment that goes before the image.
<     }
<     deriving (Eq, Ord, Show)

<p> The images have to be rendered in a particular order, so we'll use a <code>Map</code> </p>

< Map Int a

<p> where the integer keys provide the ordering and <code>a</code> is some type. </p>

<p> To toggle visibility in the final rendering, we flip <code>imageVisible</code>: </p>

< toggleVisibility :: Int -> Map Int Image -> Map Int Image
< toggleVisibility k m = M.adjust f k m
<   where
<     f (Image x b c) = Image x (not b) c

<p> We can set the description for an image: </p>

< setDesc :: (Int, T.Text) -> Map Int Image -> Map Int Image
< setDesc (k, c) m = M.adjust f k m
<   where
<     f (Image x b _) = Image x b c

<p> We can move the <code>k</code>th image up: </p>

< moveUp :: Int -> Map Int Image -> Map Int Image
< moveUp 0 m = m
< moveUp k m
<   = let xs = M.elems m in
<     M.fromList $ zip [0..] $ take (k-1) xs ++ [xs !! k, xs !! (k-1)] ++ drop (k+1) xs
<     -- ^^^ Assumes contiguous keys!

<p> and down: </p>

< moveDown :: Int -> Map Int Image -> Map Int Image
< moveDown k m
<   | k == fst (M.findMax m) = m
<   | otherwise = let xs = M.elems m in
<       M.fromList $ zip [0..] $ take k xs ++ [xs !! (k+1), xs !! k] ++ drop (k+2) xs

<p> It's not efficient to completely rebuild the map by converting
it to a list and back again, but this'll do for now. </p>

<p> In terms of the user interface there are a few events to
consider: </p>

<p> <ul>
<li> user toggles visibility of the <code>k</code>th image; </li>
<li> user moves the <code>k</code>th image up; </li>
<li> user moves the <code>k</code>th image down; and </li>
<li> user changes the comment text for the <code>k</code>th image. </li>
</ul> </p>

<p> We'll put these four events into our own type. The first three are of
type <code>Event t Int</code> where the <code>Int</code> is the key for the
image in question. The last one has type <code>Event t (Int, T.Text)</code>
since we need the key and the text that was entered into the textbox.
In Reflex, the event type is <a href="https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Class.html#t:Event">Event</a>. </p>

< data ImageEvent t = ImageEvent
<     { evToggle  :: Event t Int
<     , evUp      :: Event t Int
<     , evDown    :: Event t Int
<     , evKey     :: Event t (Int, T.Text)
<     }

<p> Next, <code>imageW</code> creates an unnumbered list of images, consisting of
a text field indicating if the image will be visible; a text box for writing a comment;
buttons to toggle visibility and move the image up and down; and finally the image itself. </p>

< imageW
<     :: forall m t. (MonadWidget t m)
<     => Dynamic t (Map Int Image)
<     -> m (Dynamic t (Map Int (ImageEvent t)))
< imageW xs = elClass "ul" "list" $ listWithKey xs $ \k x -> elClass "li" "element" $ do
<     dynText $ fmap (T.pack . show . imageVisible) x
<     el "br" $ return ()
<
<     let xEvent = imageRemark <$> uniqDyn x
<
<     ti <- textInput $ textBoxAttrs & setValue .~ (updated xEvent)
<
<     tEvent <- updated <$> return (zipDynWith (,) (constDyn k) (_textInput_value ti))
<
<     el "br" $ return ()
<
<     (visibleEvent, moveUpEvent, moveDownEvent) <- elClass "div" "my buttons" $ do
<                                                     visibleEvent  <- (fmap $ const k) <$> button "visible"
<                                                     moveUpEvent   <- (fmap $ const k) <$> button "up"
<                                                     moveDownEvent <- (fmap $ const k) <$> button "down"
<                                                     return (visibleEvent, moveUpEvent, moveDownEvent)
<
<     elClass "p" "the image" $ elDynAttr "img" (fmap f x) (return ())
<
<     return $ ImageEvent visibleEvent moveUpEvent moveDownEvent tEvent
<
<   where
<
<     f :: Image -> Map T.Text T.Text
<     f i = M.fromList
<             [ ("src",   imageFileName i)
<             , ("width", "500")
<             ]
<
<     textBoxAttrs :: TextInputConfig t
<     textBoxAttrs = def { _textInputConfig_attributes = constDyn $ M.fromList [("size", "100")] }

<p> To process the dynamic map
we use <a href="https://hackage.haskell.org/package/reflex-dom-0.3/docs/Reflex-Dom-Widget-Basic.html#v:listWithKey">listWithKey</a>:


< listWithKey
<   :: forall t k v m a. (Ord k, MonadWidget t m)
<   => Dynamic t (Map k v)
<   -> (k -> Dynamic t v -> m a)
<   -> m (Dynamic t (Map k a))

<p> Specialised to our usage, the type is: </p>

< listWithKey
<   :: forall t m. (MonadWidget t m)
<   => Dynamic t (Map Int Image)
<   -> (Int -> Dynamic t Image -> m (ImageEvent t))
<   -> m (Dynamic t (Map Int (ImageEvent t)))

<p> It's like mapping over the elements of the dynamic input: </p>

< listWithKey xs $ \k x -> ...

<p> We use <a href="https://hackage.haskell.org/package/reflex-dom-0.3/docs/Reflex-Dom-Widget-Basic.html#v:elClass">elClass</a> to
produce the elements on the page. For example the text attribute showing if the image is visible or not can be rendered
using <a href="https://hackage.haskell.org/package/reflex-dom-0.3/docs/Reflex-Dom-Widget-Basic.html#v:dynText">dynText</a>: </p>

<     dynText $ fmap (T.pack . show . imageVisible) x

<p> We have an <code>fmap</code> since <code>x :: Dynamic t Image</code> and <a href="https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Dynamic.html#t:Dynamic">Dynamic</a>
has a <code>Functor</code> instance. </p>

<p> The image list and all the events are wrapped up in <code>imageListW</code>. Here's the main part: </p>

< imageListW
<     :: forall t m. MonadWidget t m
<     => Dynamic t T.Text
<     -> m ()
< imageListW dynDrop = do
<     let eventDrop = fmap const $ updated $ fmap parseDrop dynDrop :: Event t (MM Image -> MM Image)
<
<     rec xs <- foldDyn ($) emptyMap $ mergeWith (.)
<             [ eventDrop
<             , switch . current $ toggles
<             , switch . current $ ups
<             , switch . current $ downs
<             , switch . current $ keys
<             ]
<
<         bs <- imageW xs
<
<         let toggles :: Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))
<             ups     :: Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))
<             downs   :: Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))
<             keys    :: Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))
<
<             toggles = (mergeWith (.) . map (fmap $ toggleVisibility) . map evToggle . M.elems) <$> bs
<             ups     = (mergeWith (.) . map (fmap $ moveUp)           . map evUp     . M.elems) <$> bs
<             downs   = (mergeWith (.) . map (fmap $ moveDown)         . map evDown   . M.elems) <$> bs
<             keys    = (mergeWith (.) . map (fmap $ setDesc)          . map evKey    . M.elems) <$> bs
<
<     ta <- textArea $ (def :: TextAreaConfig t)
<                         { _textAreaConfig_setValue   = (T.concat . map rawHTML . M.elems) <$> updated xs
<                         , _textAreaConfig_attributes = taAttrs
<                         }
<
<     return ()

<p> Notice that <code>toggles</code> is used before it is defined! This is made possible by
using the <a href="https://wiki.haskell.org/MonadFix">recursive do</a> extension which provides
the ability to do <i>value</i> recursion. </p>

<p> The key bit is the use
of <a href="https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Class.html#v:mergeWith">mergeWith</a>
that combines all of the events. </p>

< mergeWith :: Reflex t => (a -> a -> a) -> [Event t a] -> Event t a

<p> Here, <code>mergeWidth (.)</code> will left-fold simultaneous events. </p>

<     rec xs <- foldDyn ($) emptyMap $ mergeWith (.)
<             [ eventDrop
<             , switch . current $ toggles
<             , switch . current $ ups
<             , switch . current $ downs
<             , switch . current $ keys
<             ]

<p> The <code>toggles</code> has type </p>

< Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))

<p> so we use <a href="https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Class.html#v:switch">switch</a>
and <a href="https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Dynamic.html#v:current">current</a>
to get to an <code>Event</code> type: </p>

<pre>
ghci> :t switch
switch :: Reflex t => Behavior t (Event t a) -> Event t a

ghci> :t current
current :: Reflex t => Dynamic t a -> Behavior t a

ghci> :t switch . current
switch . current :: Reflex t => Dynamic t (Event t a) -> Event t a
</pre>

<p> This merge is also where we bring in the drag 'n' drop event via <code>eventDrop</code> which is how we get
a list of images into the dynamic map. </p>

<p><b>Try it out</b></p>

<p> To try it out without setting up Reflex, grab <a href="https://github.com/carlohamalainen/playground/raw/master/haskell/reflex/gallery_editor.zip">gallery_editor.zip</a>, unzip it, and open <code>gallery_editor/gallery.jsexe/index.html</code> in your browser. Drag some images onto the top area of the page using your file manager. Tested on Ubuntu 16. </p>

<p> Or, grab the source from <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/reflex">Github</a>. </p>

<p><img src="https://raw.githubusercontent.com/carlohamalainen/playground/master/haskell/reflex/dropped01.png" width=600> </p>
<p><img src="https://raw.githubusercontent.com/carlohamalainen/playground/master/haskell/reflex/dropped02.png" width=600> </p>


