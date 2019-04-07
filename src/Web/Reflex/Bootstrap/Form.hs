{-|
Module      : Web.Reflex.Bootstrap.Form
Description : Rudimentary support for bootstrap forms
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable

Example of usage:
@
authForm :: MonadWidget t m => Dynamic t (Login, Password)
authForm = horizontalForm $ do
  loginInput <- formGroupText "Login" def
  passInput <- formGroupText "Password" def { _textInputConfig_inputType = "password" }
  return $ (,) <$> loginInput <*> passInput
@
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Web.Reflex.Bootstrap.Form(
    FormType(..)
  , MonadForm(..)
  , FormT(..)
  , runFormT
  , horizontalForm
  , horizontalFormEnter
  , noForm
  , inlineForm
  , inlineFormEnter
  , formGroupStatic
  , formGroupText
  , formGroupInt
  , formGroupJson
  , formGroupLabel
  , formGroupSelect
  , textInputDyn
  , editInputDyn
  , submitButton
  ) where

import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Bifunctor
import Data.Coerce
import Data.Default
import Data.Functor
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Proxy
import Data.Text
import GHC.Generics
import Reflex
import Reflex.Dom
import Reflex.Dom.Core
import Reflex.Host.Class
import Text.Read (readMaybe, readEither)

import Web.Reflex.Bootstrap.Markup
import Web.Reflex.Bootstrap.Upload.Input
import Web.Reflex.Bootstrap.Utils

-- | Bootstrap has two types of forms
data FormType = FormHorizontal | FormInline | FormNone
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Helps tracking properties of form
class MonadWidget t m => MonadForm t m | m -> t where
  -- | Which type of form is it
  getFormType :: m FormType

-- | Default implementation of 'MonadForm'
newtype FormT t m a = FormT { unFormT :: ReaderT FormType m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t,
            MonadSample t, MonadAsyncException, MonadException, HasDocument,
            NotReady t)

-- | Execute 'FormT' context
runFormT :: FormType -> FormT t m a -> m a
runFormT ft m = runReaderT (unFormT m) ft

instance MonadWidget t m => MonadForm t (FormT t m) where
  getFormType = FormT ask
  {-# INLINE getFormType #-}

--instance MonadWidget t m => MonadWidget t (FormT m) where

instance MonadTrans (FormT t) where
  lift = FormT . lift

instance MonadTransControl (FormT t) where
  type StT (FormT t) a = a
  liftWith = defaultLiftWith FormT unFormT
  restoreT = defaultRestoreT FormT

instance MonadReader r m => MonadReader r (FormT t m) where
  ask = lift ask
  local f (FormT a) = FormT $ mapReaderT (local f) a

instance MonadState s m => MonadState s (FormT t m) where
  get = lift get
  put s = lift $ put s

instance MonadDynamicWriter t w m => MonadDynamicWriter t w (FormT t m) where
  tellDyn = lift . tellDyn

instance EventWriter t w m => EventWriter t w (FormT t m) where
  tellEvent = lift . tellEvent

instance HasJSContext m => HasJSContext (FormT t m) where
  type JSContextPhantom (FormT t m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (FormT t m) where
  runWithReplace a0 a' = FormT $ runWithReplace (coerce a0) (coerceEvent a')
  traverseIntMapWithKeyWithAdjust f dm0 dm' = FormT $ traverseIntMapWithKeyWithAdjust
      (\k v -> unFormT (f k v)) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjust f dm0 dm' = FormT $ traverseDMapWithKeyWithAdjust
    (\k v -> unFormT (f k v)) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = FormT $ traverseDMapWithKeyWithAdjustWithMove
    (\k v -> unFormT (f k v)) (coerce dm0) (coerceEvent dm')

instance MonadRef m => MonadRef (FormT t m) where
  type Ref (FormT t m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (FormT t m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (FormT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance PerformEvent t m => PerformEvent t (FormT t m) where
  type Performable (FormT t m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

deriving instance TriggerEvent t m => TriggerEvent t (FormT t m)

instance PostBuild t m => PostBuild t (FormT t m) where
  getPostBuild = lift getPostBuild

instance (DomBuilder t m, MonadHold t m, MonadFix m) => DomBuilder t (FormT t m) where
  type DomBuilderSpace (FormT t m) = DomBuilderSpace m

-- | Render input elements without form
noForm :: MonadWidget t m => FormT t m a -> m a
noForm ma = runFormT FormNone ma

-- | Wrapper for bootstrap horizontal form
horizontalForm :: MonadWidget t m => FormT t m a -> m a
horizontalForm ma = body $ runFormT FormHorizontal ma
  where
    body = elAttr "form" [
        ("class", "form-horizontal")
      , ("accept-charset", "UTF-8")
      , ("onSubmit", "return false;")
      ]

-- | Wrapper for bootstrap horizontal form
inlineForm :: MonadWidget t m => FormT t m a -> m a
inlineForm ma = body $ runFormT FormInline ma
  where
    body = elAttr "form" [
        ("class", "form-inline")
      , ("accept-charset", "UTF-8")
      , ("onSubmit", "return false;")
      ]

-- | Wrapper for bootstrap horizontal form, event is fired on submit.
horizontalFormEnter :: forall t m a . MonadWidget t m => FormT t m a -> m (a, Event t ())
horizontalFormEnter ma = body $ runFormT FormHorizontal ma
  where
    body w = do
      (element, a) <- elAttr' "form" [
          ("class", "form-horizontal")
        , ("accept-charset", "UTF-8")
        , ("onSubmit", "return false;")
        ] w
      -- handle enter
      let enterCode = 13
          enterE = ffilter (== enterCode) $ domEvent Keydown element :: Event t Word
      pure (a, void enterE)

-- | Wrapper for bootstrap horizontal form, event is fired on submit.
inlineFormEnter :: forall t m a . MonadWidget t m => FormT t m a -> m (a, Event t ())
inlineFormEnter ma = body $ runFormT FormInline ma
  where
    body w = do
      (element, a) <- elAttr' "form" [
          ("class", "form-inline")
        , ("accept-charset", "UTF-8")
        , ("onSubmit", "return false;")
        ] w
      -- handle enter
      let enterCode = 13
          enterE = ffilter (== enterCode) $ domEvent Keydown element :: Event t Word
      pure (a, void enterE)

-- | Helper to create bootstrap text input with label
formGroupLabel :: forall t m a . MonadForm t m
  => Dynamic t Text -- ^ Label
  -> (Text -> m a) -- ^ Widget that should assign given id to field
  -> m a
formGroupLabel labelTextD w = formGroup $ do
  ft <- getFormType
  i <- genId
  initialLabel <- sample . current $ labelTextD
  let labelClass = case ft of
        FormHorizontal -> "col-md-12 control-label"
        FormInline -> "bmd-label-floating"
        FormNone -> ""
  mkLabel [ ("for", elemId i)
          , ("class", labelClass)] $ dynText labelTextD
  let wrapper = case ft of
        FormHorizontal -> elClass "div" "col-md-12"
        FormInline -> id
        FormNone -> id
  wrapper $ w $ elemId i
  where
    formGroup w = do
      ft <- getFormType
      let cz = case ft of
            FormHorizontal -> "form-group"
            FormInline -> "form-group bmd-form-group"
            FormNone -> ""
      elClass "div" cz w
    mkLabel = elAttr "label"
    elemId = ("form-group-" <>) . showt

-- | Helper to create bootstrap text input with label
formGroupJson :: forall t m a . (FromJSON a, MonadForm t m)
  => Dynamic t Text -- ^ Label
  -> UploadFileConfig t -- ^ Input field config
  -> m (Event t (Either Text (FullUploadFile a)))
formGroupJson labelTextD cfg = formGroupLabel labelTextD $ \fid -> do
  rec
    _ <- textInput def {
        _textInputConfig_attributes = ffor fileNameD $ \name -> [
            ("class", "form-control")
          , ("placeholder", name)
          , ("readonly", "")]
      }
    mfileE <- uploadJsonFileInput cfg {
        uploadFileInputAttrs = (mappend [
            ("class", "form-control")
          , ("id", fid)]
          ) <$> uploadFileInputAttrs cfg
      }
    let fileNameE = fforMaybe mfileE $ \case
          Right FullUploadFile{..} -> Just uploadFullFileName
          _ -> Nothing
    fileNameD <- holdDyn "Browse..." fileNameE
  return mfileE

-- | Helper to create bootstrap text input with label
formGroupText :: MonadForm t m => Dynamic t Text -> TextInputConfig t -> m (TextInput t)
formGroupText labelTextD cfg = formGroupLabel labelTextD $ \fid -> textInput cfg {
  _textInputConfig_attributes = constDyn [
      ("class", "form-control")
    , ("id", fid)
    , ("type", "text")
    ]
  }

-- | Helper to create bootstrap static text field with label
formGroupStatic :: MonadForm t m => Dynamic t Text -> Dynamic t Text -> m (TextInput t)
formGroupStatic labelTextD valD = formGroupLabel labelTextD $ \fid -> do
  v <- sample . current $ valD
  textInput TextInputConfig {
      _textInputConfig_setValue = updated valD
    , _textInputConfig_initialValue = v
    , _textInputConfig_inputType = "text"
    , _textInputConfig_attributes = constDyn [
          ("class", "form-control")
        , ("id", fid)
        , ("type", "text")
        , ("readonly", "")
        ]
    }

-- | Helper to create bootstrap select input with label.
--
-- Create a dropdown box The first argument gives the initial value of the dropdown; if it is not present in the map of options provided, it will be added with an empty string as its text
formGroupSelect :: (MonadForm t m, Ord k, Show k, Read k) => Dynamic t Text -> k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Dropdown t k)
formGroupSelect labelTextD initKey vals cfg = formGroupLabel labelTextD $ \fid -> dropdown initKey vals cfg {
    _dropdownConfig_attributes = do
      atrs <- _dropdownConfig_attributes cfg
      pure $ atrs <> [
          ("class", "form-control")
        , ("id", fid)
        ]
  }

-- | Configuration for 'formGroupInt' widget
data IntInputConfig t = IntInputConfig {
  intInputInitVal :: Int
, intInputSetVal  :: Event t Int
, intInputAttrs   :: Dynamic t (Map Text Text)
}

instance Reflex t => Default (IntInputConfig t) where
  def = IntInputConfig {
      intInputInitVal = 0
    , intInputSetVal = never
    , intInputAttrs = pure mempty
    }

-- | Helper to create bootsrap integer input with label
formGroupInt :: MonadForm t m => Dynamic t Text -> IntInputConfig t -> m (Dynamic t Int)
formGroupInt labelTextD IntInputConfig{..} = formGroupLabel labelTextD $ \fid -> do
  tinput <- textInput TextInputConfig {
      _textInputConfig_inputType    = "number"
    , _textInputConfig_initialValue = showt intInputInitVal
    , _textInputConfig_setValue     = showt <$> intInputSetVal
    , _textInputConfig_attributes   = mappend [
          ("class", "form-control")
        , ("id", fid)
        , ("type", "number")] <$> intInputAttrs
    }
  holdDyn intInputInitVal $ fforMaybe (updated $ value tinput) $ readMaybe . unpack

-- | Field that transforms dynamic without label
textInputDyn :: MonadWidget t m => Dynamic t Text -> m (Dynamic t Text)
textInputDyn dv = do
  dv0 <- sample . current $ dv
  ti <- textInput TextInputConfig {
      _textInputConfig_inputType = "text"
    , _textInputConfig_initialValue = dv0
    , _textInputConfig_setValue = updated dv
    , _textInputConfig_attributes = pure [("class", "form-control")]
    }
  pure $ _textInput_value ti

-- | Field to edit values that are showable and readable from string
editInputDyn :: (Read a, Show a, MonadWidget t m) => Dynamic t a -> m (Dynamic t a)
editInputDyn dv = do
  dv0 <- sample . current $ dv
  tinput <- textInput TextInputConfig {
      _textInputConfig_inputType    = "text"
    , _textInputConfig_initialValue = showt dv0
    , _textInputConfig_setValue     = showt <$> updated dv
    , _textInputConfig_attributes   = pure [("class", "form-control")]
    }
  let newValE = fmap (first pack . readEither . unpack) . updated . value $ tinput
  filteredValE <- handleDanger newValE
  holdDyn dv0 filteredValE

-- | Helper to make form submit button
submitButton :: forall t m . MonadForm t m => Dynamic t Text -> m (Event t ())
submitButton sd = do
  ft <- getFormType
  let wrapper = case ft of
        FormInline -> spanClass "form-group bmd-form-group"
        FormHorizontal -> id
        FormNone -> id
  (e, _) <- wrapper $ elAttr' "button" [("type", "button"), ("class", "btn btn-primary")] $ dynText sd
  return $ domEvent Click e
