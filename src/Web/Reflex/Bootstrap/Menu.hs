module Web.Reflex.Bootstrap.Menu(
    menuWidget
  , menuWidget'
  , menuWidgetDyn
  , menuWidgetDyn'
  , rawMenuWidgetDyn
  , rawMenuWidget
  , PageWidget
  , MenuWidget(..)
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Proxy
import Data.Text
import Reflex.Dom

import qualified Data.Map.Strict as M

import Web.Reflex.Bootstrap.Markup
import Web.Reflex.Bootstrap.Router
import Web.Reflex.Bootstrap.Utils

-- | Describe your menu items, how to render them
class (MonadWidget t m, Ord (MenuItem a), Eq (MenuItem a)) => MenuWidget t m a | a -> m, m -> t where
  -- | Your type to describe menu item (usally a Enum)
  type MenuItem a :: *

  -- | Which test to display on menu brand label
  menuBrand :: Proxy a -> m (Dynamic t Text, Event t (MenuItem a))

  -- | Render menu item label on button
  menuItemLabel :: Proxy a -> MenuItem a -> m (Dynamic t Text)

  -- | Which label to use for logout button
  menuLogoutLabel :: Proxy a -> m (Dynamic t Text)

  -- | Additional classes that should be added to the generated navbar
  menuBarClasses :: Proxy a -> m (Dynamic t Text)
  menuBarClasses _ = pure $ pure "navbar-expand-lg navbar-dark bg-dark justify-content-between"

  -- | Render additional widget on the right side next to logout
  menuAdditionalRightWidget :: Proxy a -> m ()
  menuAdditionalRightWidget _ = pure ()

  -- | Whether to allow clicking on active menu item
  menuActiveClickable :: Proxy a -> m (Dynamic t Bool)
  menuActiveClickable _ = pure $ pure False

  -- | Classes that are added to active menu item
  menuActiveClasses :: Proxy a -> m (Dynamic t Text)
  menuActiveClasses _ = pure $ pure "nav-item active"

  -- | Classes that are added to non active menu item
  menuItemClasses :: Proxy a -> m (Dynamic t Text)
  menuItemClasses _ = pure $ pure "nav-item"

-- | Widget that build whole page and returns event when we want to change page.
-- Input events is fired, when user is going to change page by clicking menu. You
-- can use the event to cleanup important resources.
type PageWidget t m a = Event t () -> m (Event t (MenuItem a))

-- | Create menu widget that switches over pages of the app, allows dynamically
-- change menu items.
menuWidgetDyn :: forall t m a . MenuWidget t m a
  => Proxy a
  -> MenuItem a -- ^ Name of startup page
  -> Dynamic t (Map (MenuItem a) (PageWidget t m a)) -- ^ Mappings from page name to widget, return event allows to jump to another page
  -> m (Event t ()) -- ^ Event when user wants to logout
menuWidgetDyn prox initialItem itemsD = do
  de <- widgetHoldDyn $ ffor itemsD $ menuWidget prox initialItem
  pure $ switch . current $ de

-- | Create menu widget that switches over pages of the app
menuWidget :: forall t m a . MenuWidget t m a
  => Proxy a
  -> MenuItem a -- ^ Name of startup page
  -> Map (MenuItem a) (PageWidget t m a) -- ^ Mappings from page name to widget, return event allows to jump to another page
  -> m (Event t ()) -- ^ Event when user wants to logout
menuWidget prox initialItem items = fmap (switch . current) . route $ mdo
  r <- menuBar ne initialItem
  ne <- case M.lookup initialItem items of
    Nothing -> pure never
    Just m -> m . void . unRoute . snd $ r
  pure r
  where
  menuBar :: Event t (MenuItem a) -> MenuItem a -> m (Event t (), Route t m (Event t ()))
  menuBar nextE currItem = do
    classesD <- fmap ("navbar " <>) <$> menuBarClasses prox
    elDynAttr "div" ((\classes -> [("class", classes)]) <$> classesD) $ do
      -- Brand
      (brandTextD, brandE) <- menuBrand prox
      dyn $ ffor brandTextD $ \brandText -> elAttr "a" [
          ("class", "navbar-brand")
        , ("href", "#")
        ] $ text brandText
      -- Mobile menu toggler
      elAttr "button" [
          ("class", "navbar-toggler")
        , ("type", "button")
        , ("data-toggle", "collapse")
        , ("data-target", "#navbarSupportedContent")
        , ("aria-controls", "navbarSupportedContent")
        , ("aria-expanded", "false")
        , ("aria-label", "Toggle navigation")
        ] $ spanClass "navbar-toggler-icon" $ pure ()
      -- Content
      elAttr "div" [("class", "collapse navbar-collapse"), ("id", "navbarSupportedContent")] $ mdo
        routes <- ulClass "navbar-nav mr-auto" $ mdo
          activeClassesD <- menuActiveClasses prox
          inactiveClassesD <- menuItemClasses prox
          forM (M.toList items) $ \(name, m) -> do
            let mkItem lblD = do
                  e <- widgetHoldDyn $ ffor lblD $ hrefClass "nav-link" . text
                  pure $ Route . ffor (switch . current $ e) $ const $ mdo
                    r <- menuBar e name
                    e <- m . void . unRoute . snd $ r
                    pure r
                mkActiveItem lblD = do
                  _ <- dyn $ ffor lblD $ flip linkClass "nav-link"
                  pure $ Route never
            if name == currItem
              then liClassDyn activeClassesD $ do
                itemLabelD <- menuItemLabel prox name
                clickableD <- menuActiveClickable prox
                fmap switchRoute $ widgetHoldDyn $ ffor clickableD $ \clickable -> if clickable
                  then mkItem itemLabelD
                  else mkActiveItem itemLabelD
              else liClassDyn inactiveClassesD $ do
                itemLabelD <- menuItemLabel prox name
                mkItem itemLabelD
        menuAdditionalRightWidget prox
        logoutLabelD <- menuLogoutLabel prox
        logoutE <- widgetHoldDyn $ ffor logoutLabelD $ hrefClass "nav-link" . text
        -- Route for event when current widget want to change page
        let nextE' = leftmost [nextE, brandE]
        let manualRoute = Route . fforMaybe nextE' $ \name -> case M.lookup name items of
              Nothing -> Nothing
              Just m -> Just $ mdo
                r <- menuBar e name
                e <- m . void . unRoute . snd $ r
                pure r
        pure (switchPromptlyDyn logoutE, manualRoute <> mconcat routes)

-- | Create menu widget that switches over pages of the app, allows dynamically
-- change menu items.
rawMenuWidgetDyn :: forall t m a . MenuWidget t m a
  => Proxy a
  -> MenuItem a -- ^ Name of startup page
  -> Dynamic t [MenuItem a] -- ^ List of menu items to display to user
  -> m (Event t (), Event t (MenuItem a)) -- ^ Event when user wants to logout and event when user want to go to another page
rawMenuWidgetDyn prox initialItem itemsD = do
  de <- widgetHoldDyn $ ffor itemsD $ rawMenuWidget prox initialItem
  let logoutE = switch . current $ fst <$> de
      changeE = switch . current $ snd <$> de
  pure (logoutE, changeE)

-- | Create menu widget that switches over pages of the app, allows dynamically
-- change menu items.
rawMenuWidget :: forall t m a . MenuWidget t m a
  => Proxy a
  -> MenuItem a -- ^ Name of startup page
  -> [MenuItem a] -- ^ List of menu items to display to user
  -> m (Event t (), Event t (MenuItem a)) -- ^ Event when user wants to logout and event when user want to go to another page
rawMenuWidget prox initialItem items = menuBar initialItem
  where
  menuBar :: MenuItem a -> m (Event t (), Event t (MenuItem a))
  menuBar currItem = do
    classesD <- fmap ("navbar " <>) <$> menuBarClasses prox
    elDynAttr "div" ((\classes -> [("class", classes)]) <$> classesD) $ do
      -- Brand
      (brandTextD, brandE) <- menuBrand prox
      elAttr "a" [
          ("class", "navbar-brand")
        , ("href", "#")
        ] $ dynText brandTextD
      -- Mobile menu toggler
      elAttr "button" [
          ("class", "navbar-toggler")
        , ("type", "button")
        , ("data-toggle", "collapse")
        , ("data-target", "#navbarSupportedContent")
        , ("aria-controls", "navbarSupportedContent")
        , ("aria-expanded", "false")
        , ("aria-label", "Toggle navigation")
        ] $ spanClass "navbar-toggler-icon" $ pure ()
      -- Content
      elAttr "div" [("class", "collapse navbar-collapse"), ("id", "navbarSupportedContent")] $ mdo
        es <- ulClass "navbar-nav mr-auto" $ mdo
          forM items $ \name -> do
            activeClassesD <- menuActiveClasses prox
            inactiveClassesD <- menuItemClasses prox
            let mkItem lblD = do
                  e <- hrefClass "nav-link" $ dynText lblD
                  pure $ name <$ e
                mkActiveItem lblD = do
                  _ <- dyn $ ffor lblD $ flip linkClass "nav-link"
                  pure never
            if name == currItem
              then liClassDyn activeClassesD $ do
                itemLabelD <- menuItemLabel prox name
                clickableD <- menuActiveClickable prox
                fmap (switch . current) $ widgetHoldDyn $ ffor clickableD $ \clickable -> if clickable
                  then mkItem itemLabelD
                  else mkActiveItem itemLabelD
              else liClassDyn inactiveClassesD $ do
                itemLabelD <- menuItemLabel prox name
                mkItem itemLabelD
        menuAdditionalRightWidget prox
        logoutLabelD <- menuLogoutLabel prox
        logoutE <- hrefClass "nav-link" . dynText $ logoutLabelD
        pure (logoutE, leftmost $ brandE : es)


-- | Create menu widget that switches over pages of the app
menuWidgetDyn' :: forall t m a . MenuWidget t m a
  => Proxy a
  -> PageWidget t m a -- ^ Startup widget
  -> Dynamic t (Map (MenuItem a) (PageWidget t m a)) -- ^ Mappings from page name to widget, return event allows to jump to another page
  -> m (Event t ()) -- ^ Event when user wants to logout
menuWidgetDyn' prox initWidget itemsD = do
  de <- widgetHoldDyn $ ffor itemsD $ menuWidget' prox initWidget
  pure $ switch . current $ de

-- | Create menu widget that switches over pages of the app
menuWidget' :: forall t m a . MenuWidget t m a
  => Proxy a
  -> PageWidget t m a -- ^ Startup widget
  -> Map (MenuItem a) (PageWidget t m a) -- ^ Mappings from page name to widget, return event allows to jump to another page
  -> m (Event t ()) -- ^ Event when user wants to logout
menuWidget' prox initWidget items = fmap (switch . current) . route $ mdo
  r <- menuBar iw Nothing
  iw <- initWidget . void . unRoute . snd $ r
  pure r
  where
  menuBar :: Event t (MenuItem a) -> Maybe (MenuItem a) -> m (Event t (), Route t m (Event t ()))
  menuBar nextE mcurrItem = do
    classesD <- fmap ("navbar " <>) <$> menuBarClasses prox
    elDynAttr "div" ((\classes -> [("class", classes)]) <$> classesD) $ do
      -- Brand
      (brandTextD, brandE) <- menuBrand prox
      dyn $ ffor brandTextD $ \brandText -> elAttr "a" [
          ("class", "navbar-brand")
        , ("href", "#")
        ] $ text brandText
      -- Mobile menu toggler
      elAttr "button" [
          ("class", "navbar-toggler")
        , ("type", "button")
        , ("data-toggle", "collapse")
        , ("data-target", "#navbarSupportedContent")
        , ("aria-controls", "navbarSupportedContent")
        , ("aria-expanded", "false")
        , ("aria-label", "Toggle navigation")
        ] $ spanClass "navbar-toggler-icon" $ pure ()
      -- Content
      elAttr "div" [("class", "collapse navbar-collapse"), ("id", "navbarSupportedContent")] $ mdo
        routes <- ulClass "navbar-nav mr-auto" $ mdo
          forM (M.toList items) $ \(name, m) -> do
            activeClassesD <- menuActiveClasses prox
            inactiveClassesD <- menuItemClasses prox
            let mkItem lblD = do
                  e <- widgetHoldDyn $ ffor lblD $ hrefClass "nav-link" . text
                  pure $ Route . ffor (switchPromptlyDyn e) $ const $ mdo
                    r <- menuBar e (Just name)
                    e <- m . void . unRoute . snd $ r
                    pure r
                mkActiveItem lblD = do
                  _ <- dyn $ ffor lblD $ flip linkClass "nav-link"
                  pure $ Route never
            if Just name == mcurrItem
              then liClassDyn activeClassesD $ do
                itemLabelD <- menuItemLabel prox name
                clickableD <- menuActiveClickable prox
                fmap switchRoute $ widgetHoldDyn $ ffor clickableD $ \clickable -> if clickable
                  then mkItem itemLabelD
                  else mkActiveItem itemLabelD
              else liClassDyn inactiveClassesD $ do
                itemLabelD <- menuItemLabel prox name
                mkItem itemLabelD
        menuAdditionalRightWidget prox
        logoutLabelD <- menuLogoutLabel prox
        logoutE <- widgetHoldDyn $ ffor logoutLabelD $ hrefClass "nav-link" . text
        -- Route for event when current widget want to change page
        let nextE' = leftmost [nextE, brandE]
        let manualRoute = Route . fforMaybe nextE' $ \name -> case M.lookup name items of
              Nothing -> Nothing
              Just m -> Just $ mdo
                r <- menuBar e (Just name)
                e <- m . void . unRoute . snd $ r
                pure r
        pure (switchPromptlyDyn logoutE, manualRoute <> mconcat routes)
