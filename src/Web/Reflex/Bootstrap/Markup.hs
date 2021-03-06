module Web.Reflex.Bootstrap.Markup(
    elClassDyn
  , ul
  , ulClass
  , ulClassDyn
  , li
  , liClass
  , liClassDyn
  , spanClass
  , spanClassDyn
  , header
  , handleDanger
  , danger
  , info
  , centered
  , strutWidgetY
  , container
  , containerFluid
  , panel
  , row
  , md1
  , md2
  , md3
  , md4
  , md5
  , md6
  , md7
  , md8
  , md9
  , md10
  , md11
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , icon
  , well
  , href
  , hrefClass
  , primaryButton
  ) where

import Data.Monoid
import Data.Text (Text)
import Reflex
import Reflex.Dom

-- | Like 'elClass' but with dynamic class
elClassDyn :: MonadWidget t m => Text -> Dynamic t Text -> m a -> m a
elClassDyn elmnt classD = elDynAttr elmnt $ (\c -> [("class", c)]) <$> classD

-- | Helper for simple ul element
ul :: MonadWidget t m => m a -> m a
ul = el "ul"

-- | Helper for ul element with class
ulClass :: MonadWidget t m => Text -> m a -> m a
ulClass = elClass "ul"

-- | Helper for ul element with class
ulClassDyn :: MonadWidget t m => Dynamic t Text -> m a -> m a
ulClassDyn = elClassDyn "ul"

-- | Helper for simple li element
li :: MonadWidget t m => m a -> m a
li = el "li"

-- | Helper for li element with class
liClass :: MonadWidget t m => Text -> m a -> m a
liClass = elClass "li"

-- | Helper for li element with dyn class
liClassDyn :: MonadWidget t m => Dynamic t Text -> m a -> m a
liClassDyn = elClassDyn "li"

-- | Helper for span element with class
spanClass :: MonadWidget t m => Text -> m a -> m a
spanClass = elClass  "span"

-- | Helper for span element with class
spanClassDyn :: MonadWidget t m => Dynamic t Text -> m a -> m a
spanClassDyn = elClassDyn "span"

-- | Helper to display centered header
header :: MonadWidget t m => Text -> m ()
header = elAttr "h1" [("style", "text-align: center;")] . text

-- | Display 'Left' occurences in danger well
handleDanger :: MonadWidget t m => Event t (Either Text a) -> m (Event t a)
handleDanger ea = do
  _ <- widgetHold (return ()) $ ffor ea $ \case
    Left e -> danger e
    Right _ -> return ()
  return $ fforMaybe ea $ \case
    Right a -> Just a
    _ -> Nothing

-- | Helper to dislpay text in red well
danger :: MonadWidget t m => Text -> m ()
danger = elClass "div" "alert alert-danger" . text

-- | Helper to dislpay text in blue well
info :: MonadWidget t m => Text -> m ()
info = elClass "div" "alert alert-info" . text

-- | Create wrapper div that is centered
centered :: MonadWidget t m => m a -> m a
centered w = elAttr "div" [("style", "text-align: center;")] $
  elAttr "div" [("style", "display: inline-block")] w

-- | Invisible div that fills vertical space
strutWidgetY :: MonadWidget t m => Text -- ^ Size parameter, ex "10px"
  -> m ()
strutWidgetY size = elAttr "div" [("style", "margin-top: " <> size <> ";")] $ return ()

container, containerFluid :: MonadWidget t m => m a -> m a
container = elClass "div" "container"
containerFluid = elClass "div" "container-fluid"

panel :: MonadWidget t m => m a -> m a
panel = elClass "div" "panel"

row :: MonadWidget t m => m a -> m a
row = elClass "div" "row"

md1 :: MonadWidget t m => m a -> m a
md1 = elClass "div" "col-md-1"

md2 :: MonadWidget t m => m a -> m a
md2 = elClass "div" "col-md-2"

md3 :: MonadWidget t m => m a -> m a
md3 = elClass "div" "col-md-3"

md4 :: MonadWidget t m => m a -> m a
md4 = elClass "div" "col-md-4"

md5 :: MonadWidget t m => m a -> m a
md5 = elClass "div" "col-md-5"

md6 :: MonadWidget t m => m a -> m a
md6 = elClass "div" "col-md-6"

md7 :: MonadWidget t m => m a -> m a
md7 = elClass "div" "col-md-7"

md8 :: MonadWidget t m => m a -> m a
md8 = elClass "div" "col-md-8"

md9 :: MonadWidget t m => m a -> m a
md9 = elClass "div" "col-md-9"

md10 :: MonadWidget t m => m a -> m a
md10 = elClass "div" "col-md-10"

md11 :: MonadWidget t m => m a -> m a
md11 = elClass "div" "col-md-11"

-- | Embedd icon
icon :: MonadWidget t m => Text -> m ()
icon name = elClass "i" "material-icons" $ text name

-- | Wrap in corresponding h tag
h1, h2, h3, h4, h5, h6 :: MonadWidget t m => m a -> m a
h1 = el "h1"
h2 = el "h2"
h3 = el "h3"
h4 = el "h4"
h5 = el "h5"
h6 = el "h6"

-- | Bootstrap well panel
well :: MonadWidget t m => m a -> m a
well = elClass "div" "well"

-- | Create clickable link with subcontent
href :: MonadWidget t m => m a -> m (Event t ())
href ma = do
  (l,_) <- elAttr' "a" [("href", "#"), ("onclick", "return false;")] ma
  return $ domEvent Click l

-- | Create clickable link with subcontent
hrefClass :: MonadWidget t m => Text -> m a -> m (Event t ())
hrefClass cl ma = do
  (l,_) <- elAttr' "a" [("class", cl), ("href", "#"), ("onclick", "return false;")] ma
  return $ domEvent Click l

-- | The most common bootstrap style for button
primaryButton :: MonadWidget t m => Dynamic t Text -> m (Event t ())
primaryButton sd = do
  (e, _) <- elAttr' "a" [("class", "btn btn-raised btn-primary"), ("href", "javascript:void(0)")] $ dynText sd
  return $ domEvent Click e

-- | The most common bootstrap style for button
primarySecondary :: MonadWidget t m => Dynamic t Text -> m (Event t ())
primarySecondary sd = do
  (e, _) <- elAttr' "a" [("class", "btn btn-raised btn-secondary"), ("href", "javascript:void(0)")] $ dynText sd
  return $ domEvent Click e
