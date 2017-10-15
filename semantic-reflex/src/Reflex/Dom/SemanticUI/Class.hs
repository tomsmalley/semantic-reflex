{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DuplicateRecordFields                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-name-shadowing #-}

module Reflex.Dom.SemanticUI.Class where

import Control.Lens hiding (element)
import Control.Monad ((<=<), void)
import Control.Monad.Reader (MonadReader, ask, runReaderT)

import Data.Default (Default(..))
import Data.Functor.Misc (WrapArg(..))
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>), First(..))

import GHC.TypeLits
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.HTMLInputElement as Input
import Language.Javascript.JSaddle (liftJSM)

import Reflex.Dom.Core hiding (fromJSString, divClass, Checkbox, CheckboxConfig, Input, setValue)

import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Lenses

import Reflex.Dom.SemanticUI.Button
import Reflex.Dom.SemanticUI.Checkbox
import Reflex.Dom.SemanticUI.Container
import Reflex.Dom.SemanticUI.Divider
import Reflex.Dom.SemanticUI.Header
import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Image
import Reflex.Dom.SemanticUI.Input
import Reflex.Dom.SemanticUI.Label
import Reflex.Dom.SemanticUI.Menu
import Reflex.Dom.SemanticUI.Message
import Reflex.Dom.SemanticUI.Segment
import Reflex.Dom.SemanticUI.Transition

import Reflex.Dom.SemanticUI.Paragraph

--------------------------------------------------------------------------------
-- | The 'UI' class encapsulates how components 'a' are rendered in a certain
-- context 'r'.
class UI t m (r :: k) a where
  type Return t m a
  ui' :: MonadWidget t m => a -> Restrict r m (El t, Return t m a)

-- | This instance is here to provide a more helpful and clear error message
-- when other instances are not selected
instance {-# OVERLAPPABLE #-} TypeError
      ( 'Text "Cannot use the component:"
  ':$$: 'ShowType a
  ':$$: 'Text "In the restricted context of:"
  ':$$: 'ShowType r
      ) => UI t m r a where
  ui' = error "impossible"

ui :: forall r t m a. (MonadWidget t m, UI t m r a)
   => a -> Restrict r m (Return t m a)
ui = fmap snd . ui'

ui_ :: forall r t m a. (MonadWidget t m, UI t m r a)
    => a -> Restrict r m ()
ui_ = void . ui

--------------------------------------------------------------------------------
-- Button instances

-- Buttons

instance (m' ~ m, t' ~ t) => UI t' m' None (Buttons t m a) where
  type Return t' m' (Buttons t m a) = a

  ui' (Buttons config@ButtonsConfig {..} buttons) = do
    (e, results) <- reRestrict $ elWithAnim' "div" attrs $ reRestrict buttons
    return (e, results)
    where
      attrs = _config <> def
        { _classes = buttonsConfigClasses config
        }

instance (m' ~ m, t' ~ t) => UI t' m' Buttons (Button t m)
  where ui' = unRestrict . ui'
instance (m' ~ m, t' ~ t) => UI t' m' Buttons (DivButton t m)
  where ui' = unRestrict . ui'
instance (m' ~ m, t' ~ t) => UI t' m' Buttons (LabeledButton t m)
  where ui' = unRestrict . ui'
instance t ~ t' => UI t' m Buttons (Conditional t) where
  type Return t' m (Conditional t) = ()
  ui' (Conditional ConditionalConfig {..})
    = reRestrict $ elWithAnim' "div" config blank
    where
      config = def
        & elConfigClasses |~ "or"
        & elConfigAttributes .~ fmap (maybe mempty ("data-text" =:)) _dataText

-- Button

instance (m' ~ m, t' ~ t) => UI t' m' None (Button t m) where
  type Return t' m' (Button t m) = Event t ()

  ui' (Button config@ButtonConfig {..} content) = do
    (e, _) <- reRestrict $ elWithAnim' "button" elConfig $ case _animated of
      Just (AnimatedButton _ hiddenContent) -> do
        reRestrict $ divClass "visible content" $ reRestrict content
        reRestrict $ divClass "hidden content" $ reRestrict hiddenContent
      Nothing -> reRestrict content
    return (e, domEvent Click e)
    where
      elConfig = _config <> def
        { _classes = buttonConfigClasses config
        }

instance t' ~ t => UI t' m Button (Icon t)
  where ui' = unRestrict . ui'
instance (m' ~ m, t' ~ t) => UI t' m' Button (Icons t m a)
  where ui' = unRestrict . ui'

-- DivButton

instance (m ~ m', t' ~ t) => UI t' m' None (DivButton t m) where
  type Return t' m' (DivButton t m) = Event t ()

  ui' (DivButton config@ButtonConfig {..} content) = do
    (e, _) <- reRestrict $ elWithAnim' "div" elConfig $ case _animated of
      Just (AnimatedButton _ hiddenContent) -> do
        reRestrict $ divClass "visible content" $ reRestrict content
        reRestrict $ divClass "hidden content" $ reRestrict hiddenContent
      Nothing -> reRestrict content
    return (e, domEvent Click e)
    where
      elConfig = _config <> def
        { _classes = buttonConfigClasses config
        }

instance t' ~ t => UI t' m DivButton (Icon t)
  where ui' = unRestrict . ui'
instance (m' ~ m, t' ~ t) => UI t' m' DivButton (Icons t m a)
  where ui' = unRestrict . ui'

-- LabeledButton

instance (m ~ m', t' ~ t) => UI t' m' None (LabeledButton t m) where
  -- TODO: return events for children?
  type Return t' m' (LabeledButton t m) = Event t ()

  ui' (LabeledButton config@LabeledButtonConfig{..} content) = do
    (e, _) <- reRestrict $ elWithAnim' "div" elConfig $ reRestrict content
    return (e, domEvent Click e)
    where
      elConfig = _config <> def
        { _classes = labeledButtonConfigClasses config }

instance (m' ~ m, t' ~ t) => UI t' m' LabeledButton (Button t m)
  where ui' = unRestrict . ui'
instance (m' ~ m, t' ~ t) => UI t' m' LabeledButton (DivButton t m)
  where ui' = unRestrict . ui'
instance (m ~ m', t ~ t') => UI t' m' LabeledButton (Label t m a)
  where ui' = unRestrict . ui'

--------------------------------------------------------------------------------
-- Checkbox instances

instance UI t m None (Checkbox t) where
  type Return t m (Checkbox t) = CheckboxResult t
  ui' (Checkbox label config@CheckboxConfig {..}) = do

    let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
          & initialAttributes .~ constAttrs
          & elementConfig_eventSpec %~ addEventSpecFlags
              (Proxy @(DomBuilderSpace m)) Click (const stopPropagation)

    (divEl, inputEl) <- unRestrict $ elWithAnim' "div" divAttrs $ do
      (inputEl, _) <- Restrict $ element "input" cfg blank
      el "label" `mapRestrict` activeText label
      return inputEl

    let e = DOM.uncheckedCastTo DOM.HTMLInputElement $ _element_raw inputEl

    case _disabled of
      Static d -> Input.setDisabled e d
      Dynamic d -> do
        -- Set initial value
        Input.setDisabled e <=< sample $ current d
        -- Set future values
        performEvent_ $ ffor (updated d) $ Input.setDisabled e

    -- Set initial value
    Input.setChecked e _initialValue
    -- Set future value, filtering uniques
    setEvent <- performEvent $ ffor _setValue $ \newValue -> do
      oldValue <- liftJSM $ Input.getChecked e
      Input.setChecked e newValue
      return $ justWhen (newValue /= oldValue) newValue

    -- Set initial indeterminate
    Input.setIndeterminate e _initialIndeterminate
    -- Set future indeterminate
    performEvent_ $ ffor _setIndeterminate $ Input.setIndeterminate e

    rec

      -- Events from the UI
      let trigger = domEvent Change inputEl <> domEvent Click divEl
      uiEvent <- fmap (fmapMaybe id) $ performEvent $ ffor trigger $ \_ -> do

        -- If the checkbox is disabled we do nothing
        disabled <- Input.getDisabled e
        if disabled
        then return Nothing
        else do

          -- This seems to be the only way to get the values from *before* the
          -- user interacted with the checkbox.
          oldValue <- sample $ current value
          oldIndeterminate <- sample $ current indeterminate

          -- If the checkbox was indeterminate we always set checked to true,
          -- otherwise we toggle the value.
          -- This matches the Semantic UI visual behaviour.
          let newValue = if oldIndeterminate then True else not oldValue
          -- Always clear the indeterminate state
              newIndeterminate = False

          Input.setChecked e newValue
          Input.setIndeterminate e newIndeterminate

          return $ Just (newValue, newIndeterminate)

      indeterminate <- holdDyn _initialIndeterminate $ leftmost
        [ _setIndeterminate
        , snd <$> uiEvent
        ]

      value <- holdUniqDyn <=< holdDyn _initialValue $ leftmost
        [ fmapMaybe id setEvent
        , fst <$> uiEvent
        ]

    let initialFocus = False -- FIXME: Is this correct?
    focus <- holdDyn initialFocus $ leftmost
      [ False <$ select (_element_events inputEl) (WrapArg Blur)
      , True <$ select (_element_events inputEl) (WrapArg Focus)
      ]

    return $ (divEl, CheckboxResult
      { _value = value
      , _change = fst <$> uiEvent
      , _indeterminate = indeterminate
      , _focus = focus
      })

    where
      divAttrs = _config <> def
        { _classes = checkboxConfigClasses config
        }
      constAttrs = "type" =: "checkbox" <> "class" =: "hidden"

--------------------------------------------------------------------------------
-- Container instances

instance (t ~ t', m ~ m') => UI t' m' None (Container t m a) where
  type Return t' m' (Container t m a) = a
  ui' (Container config@ContainerConfig {..} contents)
    = reRestrict $ elWithAnim' "i" attrs contents
    where
      attrs = _config <> def
        { _classes = containerConfigClasses config
        }

--------------------------------------------------------------------------------
-- Divider instances

instance t ~ t' => UI t' m None (Divider t) where
  type Return t' m (Divider t) = ()

  ui' (Divider config@DividerConfig {..}) = do
    reRestrict $ elWithAnim' "div" elConfig blank
    where
      elConfig = _config <> def
        { _classes = dividerConfigClasses config
        }

instance (m ~ m', t ~ t') => UI t' m' None (ContentDivider t m a) where
  type Return t' m' (ContentDivider t m a) = a

  ui' (ContentDivider config@DividerConfig {..} content) = do
    reRestrict $ elWithAnim' "div" elConfig $ reRestrict content
    where
      elConfig = _config <> def
        { _classes = addClass "horizontal" <$> dividerConfigClasses config
        }

instance (m ~ m', t ~ t') => UI t' m' ContentDivider (PageHeader t m a) where
  ui' = unRestrict . ui'
instance (m ~ m', t ~ t') => UI t' m' ContentDivider (Header t m a) where
  ui' = unRestrict . ui'

--------------------------------------------------------------------------------
-- Flag instances

instance t ~ t' => UI t' m None (Flag t) where
  type Return t' m (Flag t) = ()
  ui' (Flag flagActive FlagConfig {..})
    = reRestrict $ elWithAnim' "i" config blank
    where
      config = _config
        & elConfigClasses .~ (flip addClass "flag" <$> flagActive)

instance t ~ t' => UI t' m Inline (Flag t) where ui' = unRestrict . ui'

--------------------------------------------------------------------------------
-- Header instances

instance (m ~ m', t ~ t') => UI t' m' None (PageHeader t m a) where
  type Return t' m' (PageHeader t m a) = a
  ui' (PageHeader size config@HeaderConfig {..} widget) = do
    reRestrict $ elWithAnim' (headerSizeEl size) elConfig $ case _image of
      Nothing -> case _icon of
        Nothing -> reRestrict widget
        Just i -> ui_ i >> divClass "content" (reRestrict widget)
      Just i -> ui_ i >> divClass "content" (reRestrict widget)
    where
      elConfig = _config <> def { _classes = headerConfigClasses config }

instance (m ~ m', t ~ t') => UI t' m' None (Header t m a) where
  type Return t' m' (Header t m a) = a
  ui' (Header config@HeaderConfig {..} widget) = do
    reRestrict $ elWithAnim' "div" elConfig $ case _image of
      Nothing -> case _icon of
        Nothing -> reRestrict widget
        Just i -> ui_ i >> divClass "content" (reRestrict widget)
      Just i -> ui_ i >> divClass "content" (reRestrict widget)
    where
      msize = (fmap . fmap) headerSizeText _size
      addSize = maybe id addClass <$> msize
      elConfig = _config <> def
        { _classes = addSize <*> headerConfigClasses config
        }

instance (t ~ t', m ~ m') => UI t' m' Header (Anchor t m a)
  where ui' = unRestrict . ui'
instance (m ~ m', t ~ t') => UI t' m' Header (Label t m a)
  where ui' = unRestrict . ui'
instance (m' ~ m, t' ~ t) => UI t' m' Header (Icons t m a)
  where ui' = unRestrict . ui'
instance t' ~ t => UI t' m Header (Icon t)
  where ui' = unRestrict . ui'
instance t ~ t' => UI t' m Header (Image t)
  where ui' = unRestrict . ui'

instance m ~ m' => UI t m' Header (SubHeader m a) where
  type Return t m' (SubHeader m a) = a
  ui' (SubHeader content) = do
    reRestrict $ elWithAnim' "div" elConfig $ reRestrict content
    where
      elConfig = def { _classes = "sub header" }

--------------------------------------------------------------------------------
-- Icon instances

instance t' ~ t => UI t' m None (Icon t) where
  type Return t' m (Icon t) = ()

  ui' (Icon activeIcon config@IconConfig {..})
    = reRestrict $ elWithAnim' "i" elConfig blank
    where
      elConfig = _config <> def
        { _classes = addClass <$> activeIcon <*> iconConfigClasses config
        , _attrs = maybe mempty ("title" =:) <$> _title
        }

instance (m' ~ m, t' ~ t) => UI t' m' None (Icons t m a) where
  type Return t' m' (Icons t m a) = a

  ui' (Icons config@IconsConfig {..} icons)
    = reRestrict $ elWithAnim' "i" elConfig $ reRestrict icons
      where
        elConfig = _config <> def
          { _classes = iconsConfigClasss config
          }

instance t' ~ t => UI t' m Icons (Icon t) where ui' = unRestrict . ui'

--------------------------------------------------------------------------------
-- Input instances

instance t' ~ t => UI t' m None (Input t) where
  type Return t' m (Input t) = InputResult t

  ui' (Input config@InputConfig {..}) = do
    (divEl, inputResult) <- reRestrict $ elActiveAttr' "div" divAttrs $ do
      TextInput {..} <- Restrict $ textInput def
        { _textInputConfig_attributes = inputAttrs }
--      runRenderWhen @None ui' _icon
      return _textInput_value
    return (divEl, InputResult inputResult)
    where
      divAttrs = classAttr <$> inputConfigClasses config
      inputAttrs = active pure id $ mkInputAttrs <$> _placeholder
      mkInputAttrs mp = "type" =: "text" <> maybe mempty ("placeholder" =:) mp

--------------------------------------------------------------------------------
-- Label instances

instance t ~ t' => UI t' m Label (Detail t) where
  type Return t' m (Detail t) = ()
  ui' (Detail txt) = do
    reRestrict $ elWithAnim' "div" elConfig $ activeText txt
      where elConfig = def { _classes = "detail" }

instance (m ~ m', t ~ t') => UI t' m' None (Label t m a) where
  type Return t' m' (Label t m a) = a

  ui' (Label config@LabelConfig {..} content) = do
    reRestrict $ elWithAnim' elType elConfig $ reRestrict content
    where
      elConfig = _config <> def
        { _classes = labelConfigClasses config
        }
      elType = if _link then "a" else "div"

instance t' ~ t => UI t' m Label (Icon t)
  where ui' = unRestrict . ui'
instance (m' ~ m, t' ~ t) => UI t' m' Label (Icons t m a) where
  ui' = unRestrict . ui'

--------------------------------------------------------------------------------
-- Image instances

instance t ~ t' => UI t' m None (Image t) where
  type Return t' m (Image t) = ()
  ui' (Image src config@ImageConfig {..})
    = elWithAnim' "img" elConfig blank
    where
      elConfig = _config <> def
        { _classes = imageConfigClasses config
        , _attrs = mkAttrs <$> src <*> _title
        }
      mkAttrs s t = "src" =: s <> maybe mempty ("title" =:) t

-- | Force images to appear inline in inline context
instance (m ~ m', t ~ t') => UI t' m' Image (Label t m a)
  where ui' = unRestrict . ui'

-- | Images in labels *must* be some form of spaced if they are not an 'Avatar',
-- or they will cause a line break. Default to spacing both sides allowing user
-- override to 'LeftSpaced' or 'RightSpaced'.
instance t ~ t' => UI t' m Label (Image t) where
  ui' (Image url conf@ImageConfig{..}) = unRestrict $ ui' $ Image url conf'
    where conf' = conf { _spaced = mkSpaced <$> _shape <*> _spaced }
          mkSpaced mShape mSpaced = if mShape == Just Avatar then mSpaced
                                    else Just $ fromMaybe Spaced mSpaced

instance (m ~ m', t ~ t') => UI t' m' None (ContentImage t m a) where
  type Return t' m' (ContentImage t m a) = a
  ui' (ContentImage src config@ImageConfig {..} content)
    = elWithAnim' "div" elConfig $ do
      a <- reRestrict content
      void $ elWithAnim "img" imgConfig blank
      return a
    where
      elConfig = _config <> def
        { _classes = imageConfigClasses config }
      imgConfig = def { _attrs = mkAttrs <$> src <*> _title }
      mkAttrs s t = "src" =: s <> maybe mempty ("title" =:) t

instance (m ~ m', t ~ t') => UI t' m' ContentImage (Label t m a)
  where ui' = unRestrict . ui'

--------------------------------------------------------------------------------
-- Menu instances

instance ( t ~ t', m ~ m', Ord v
          , MonadReader (Demux t (Maybe v)) m, EventWriter t (First v) m)
  => UI t' m' Menu (MenuItem t m v) where
  type Return t' m' (MenuItem t m v) = ()
  ui' (MenuItem value config@MenuItemConfig{..} widget) = do
    selected <- ask
    let isSelected = Dynamic $ demuxed selected $ Just value

    (e, _) <- reRestrict $ elWithAnim' "div" (elConfig isSelected) widget
    Restrict $ tellEvent $ (First value) <$ domEvent Click e
    return (e, ())
      where
--        (_, _) = itemElAttrs config { _link = reLink _link }
--        reLink NoLink = StyleLink
--        reLink a = a
        elConfig isSelected = _config <> def
          { _classes = addClassMaybe <$> boolClass "active" isSelected
                                     <*> menuItemConfigClasses config
          }

instance (Ord v, t ~ t', m ~ m') => UI t' m' None (Menu t m v a) where
  type Return t' m' (Menu t m v a) = (Dynamic t (Maybe v), a)
  ui' (Menu config@MenuConfig{..} items) = reRestrict $ elWithAnim' "div" elConfig $ do
    rec
      (b, evt) <- Restrict $ runEventWriterT $ runReaderT (runRestricted items) (demux current)
      current <- holdDyn _initialValue $ leftmost [Just . getFirst <$> evt, _setValue]
    return (current, b)
    where
      elConfig = _config <> def
        { _classes = menuConfigClasses config }

--------------------------------------------------------------------------------
-- Message instances

instance (t ~ t', m ~ m') => UI t' m' None (Message t m a) where
  type Return t' m' (Message t m a) = a
  ui' (Message config@MessageConfig{..} content) = do

    let contentDismissIcon = do

          evt <- case _dismissable of
            Nothing -> return never
            Just t -> fmap ((t <$) . domEvent Click . fst) $
              unRestrict $ ui' $ Icon "close" def
          result <- content
          return (result, evt)

    rec

      let elConfig = (_config <>) $ def
            & elConfigClasses .~ messageConfigClasses config
            & elConfigTransition . event ?~ closeEvent

      (divEl, (result, closeEvent)) <-
        reRestrict $ elWithAnim' "div" elConfig $ case _icon of
          Just i -> ui_ i >> divClass "content" (reRestrict contentDismissIcon)
          Nothing -> reRestrict contentDismissIcon

    return (divEl, result)

instance (m ~ m', t ~ t') => UI t' m' Message (Header t m a) where
  ui' (Header config widget)
    = unRestrict $ ui' $ Header (config & component .~ True) widget

-- TODO FIXME For removal:

instance m ~ m' => UI t m' None (Paragraph m a) where
  type Return t m' (Paragraph m a) = a
  ui' (Paragraph contents)
    = elWithAnim' "p" def $ reRestrict contents

instance (t ~ t', m ~ m') => UI t' m' None (Anchor t m a) where
  type Return t' m' (Anchor t m a) = AnchorResult t a
  ui' (Anchor contents AnchorConfig{..}) = do
    (e, a) <- reRestrict $ elWithAnim' "a" elConfig $ reRestrict contents
    return (e, AnchorResult (domEvent Click e) a)
      where
        elConfig = _config
          & elConfigAttributes %~ (\a -> (maybe id (M.insert "href") <$> _href) <*> a)
instance (t ~ t', m ~ m') => UI t' m' Inline (Anchor t m a) where
  ui' = unRestrict . ui'
instance (t ~ t', m ~ m') => UI t' m' Message (Anchor t m a) where
  ui' = unRestrict . ui'

instance t' ~ t => UI t' m Inline (Icon t)
  where ui' = unRestrict . ui'
instance (m' ~ m, t' ~ t) => UI t' m' Inline (Icons t m a)
  where ui' = unRestrict . ui'
instance t ~ t' => UI t' m Inline (Image t)
  where ui' (Image url conf)
          = unRestrict $ ui' $ Image url $ conf & inline |~ True
instance (m ~ m', t ~ t') => UI t' m' Inline (Label t m a)
  where ui' = unRestrict . ui'

--------------------------------------------------------------------------------
-- Segment instances

instance (t ~ t', m ~ m') => UI t' m' None (Segment t m a) where
  type Return t' m' (Segment t m a) = a
  ui' (Segment config@SegmentConfig{..} content) = do

    reRestrict $ elWithAnim' "div" elConfig content

    where
      elConfig = _config <> def { _classes = segmentConfigClasses config }
