{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import Reflex
import Reflex.Dom
import System.FilePath
import System.Directory

import qualified Data.Map as M
import qualified Data.Text as T

import qualified GHCJS.DOM.DataTransfer as DOM
import qualified GHCJS.DOM.Element      as DOM
import qualified GHCJS.DOM.EventM       as DOM
import qualified GHCJS.DOM.MouseEvent   as DOM

type MM a = M.Map Int a

data Image = Image
    { imageFileName :: T.Text   -- ^ e.g. "file:///tmp/cat.jpg"
    , imageVisible  :: Bool     -- ^ Output in HTML render?
    , imageRemark   :: T.Text   -- ^ Comment that goes before the image.
    }
    deriving (Eq, Ord, Show)

data ImageEvent t = ImageEvent
    { evToggle  :: Event t Int
    , evUp      :: Event t Int
    , evDown    :: Event t Int
    , evKey     :: Event t (Int, T.Text)
    }

imageW
    :: forall m t. (MonadWidget t m)
    => Dynamic t (MM Image)
    -> m (Dynamic t (MM (ImageEvent t)))
imageW xs = elClass "ul" "list" $ listWithKey xs $ \k x -> elClass "li" "element" $ do
    dynText $ fmap (T.pack . show . imageVisible) x
    el "br" $ return ()

    let xEvent = imageRemark <$> uniqDyn x

    ti <- textInput $ textBoxAttrs & setValue .~ (updated xEvent)

    tEvent <- updated <$> return (zipDynWith (,) (constDyn k) (_textInput_value ti))

    el "br" $ return ()

    (visibleEvent, moveUpEvent, moveDownEvent) <- elClass "div" "my buttons" $ do
                                                    visibleEvent  <- (fmap $ const k) <$> button "visible"
                                                    moveUpEvent   <- (fmap $ const k) <$> button "up"
                                                    moveDownEvent <- (fmap $ const k) <$> button "down"
                                                    return (visibleEvent, moveUpEvent, moveDownEvent)

    elClass "p" "the image" $ elDynAttr "img" (fmap f x) (return ())

    return $ ImageEvent visibleEvent moveUpEvent moveDownEvent tEvent

  where

    f :: Image -> M.Map T.Text T.Text
    f i = M.fromList
            [ ("src",   imageFileName i)
            , ("width", "500")
            ]

    textBoxAttrs :: TextInputConfig t
    textBoxAttrs = def { _textInputConfig_attributes = constDyn $ M.fromList [("size", "100")] }

toggleVisibility :: Int -> MM Image -> MM Image
toggleVisibility k m = M.adjust f k m
  where
    f (Image x b c) = Image x (not b) c

setDesc :: (Int, T.Text) -> MM Image -> MM Image
setDesc (k, c) m = M.adjust f k m
  where
    f (Image x b _) = Image x b c

moveUp :: Int -> MM Image -> MM Image
moveUp 0 m = m
moveUp k m
    = let xs = M.elems m in
        M.fromList $ zip [0..] $ take (k-1) xs ++ [xs !! k, xs !! (k-1)] ++ drop (k+1) xs
        -- ^^^ Assumes contiguous keys!

moveDown :: Int -> MM Image -> MM Image
moveDown k m
    | k == fst (M.findMax m) = m
    | otherwise = let xs = M.elems m in
        M.fromList $ zip [0..] $ take k xs ++ [xs !! (k+1), xs !! k] ++ drop (k+2) xs

imageListW
    :: forall t m. MonadWidget t m
    => Dynamic t T.Text
    -> m ()
imageListW dynDrop = do
    let eventDrop = fmap const $ updated $ fmap parseDrop dynDrop :: Event t (MM Image -> MM Image)

    rec xs <- foldDyn ($) emptyMap $ mergeWith (.)
            [ eventDrop
            , switch . current $ toggles
            , switch . current $ ups
            , switch . current $ downs
            , switch . current $ keys
            ]

        bs <- imageW xs

        let toggles :: Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))
            ups     :: Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))
            downs   :: Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))
            keys    :: Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))

            toggles = (mergeWith (.) . map (fmap $ toggleVisibility) . map evToggle . M.elems) <$> bs
            ups     = (mergeWith (.) . map (fmap $ moveUp)           . map evUp     . M.elems) <$> bs
            downs   = (mergeWith (.) . map (fmap $ moveDown)         . map evDown   . M.elems) <$> bs
            keys    = (mergeWith (.) . map (fmap $ setDesc)          . map evKey    . M.elems) <$> bs

    ta <- textArea $ (def :: TextAreaConfig t)
                        { _textAreaConfig_setValue   = (T.concat . map rawHTML . M.elems) <$> updated xs
                        , _textAreaConfig_attributes = taAttrs
                        }

    return ()

  where

    taAttrs :: Dynamic t (M.Map T.Text T.Text)
    taAttrs = constDyn ("style" =: "width:600px; height:300px; border: 1px solid red;")

    emptyMap :: MM Image
    emptyMap = M.fromList $ zip [0..] []

    parseDrop :: T.Text -> MM Image
    parseDrop t = M.fromList $ zip [0..] $ map (\x -> Image x True "") $ T.words t

    rawHTML :: Image -> T.Text
    rawHTML (Image x True "") = "<p><img src=" `ta` x `ta` "></p>\n\n"
    rawHTML (Image x True  c) = "<p>" `ta` c `ta` "</p>\n" `ta` "<p><img src=" `ta` x `ta` "></p>\n\n"
    rawHTML (Image _ False _) = ""

    ta :: T.Text -> T.Text -> T.Text
    ta = T.append

main :: IO ()
main = do
    mainWidget $ el "div" $ do
      rec
        let ddEvent :: (DOM.DataTransfer -> DOM.EventM e DOM.MouseEvent a) ->
                       DOM.EventM e DOM.MouseEvent a
            ddEvent op = do
              dt <- fromMaybe (error "no DT?") <$> (DOM.getDataTransfer =<< DOM.event)
              op dt
            ddEvent_ :: DOM.EventM e DOM.MouseEvent () -> DOM.EventM e DOM.MouseEvent ()
            ddEvent_ op = ddEvent (const op)

        dragEnterEvent <- wrapDomEvent (_element_raw dropsite) (`DOM.on` DOM.dragEnter) (ddEvent_ DOM.preventDefault)
        dragLeaveEvent <- wrapDomEvent (_element_raw dropsite) (`DOM.on` DOM.dragLeave) (ddEvent_ $ return ())

        dropsite <- fst <$> (elDynAttr' "div" dropsiteAttrs $ dynText dropText)
        inDrop <- holdDyn False (leftmost [True <$ dragEnterEvent, False <$ dragLeaveEvent, False <$ dropEvent])

        dropsiteAttrs <- return . ffor inDrop $ \case
          True  -> "style" =: "border:1em solid blue;padding:2em;margin:2em;background-color:green;"
          False -> "style" =: "border:1em solid blue;padding:2em;margin:2em;"
        dragOverEvent <- wrapDomEvent (_element_raw dropsite) (`DOM.on` DOM.dragOver) (ddEvent_ DOM.preventDefault)
        performEvent_ $ return () <$ dragOverEvent

        dropEvent <- wrapDomEvent (_element_raw dropsite) (`DOM.on` DOM.drop) $ ddEvent $ \dt -> do
          DOM.preventDefault
          DOM.getData dt ("text/plain" :: String)
        dropText <- holdDyn "Drop here" $ fmap (const "Dropped ") dropEvent

        dynDrop <- uniqDyn <$> holdDyn ("" :: T.Text) dropEvent

        imageListW dynDrop

      return ()
