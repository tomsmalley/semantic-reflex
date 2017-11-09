{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-name-shadowing #-}

module Reflex.Dom.SemanticUI.Class where

import Control.Lens hiding (element, List)
import Control.Monad ((<=<), void)
import Control.Monad.Reader

import Data.Default (Default(..))
import Data.Foldable (traverse_)
import Data.Functor.Misc (WrapArg(..))
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>), First(..))
import Data.Text (Text)

import GHC.TypeLits
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.HTMLFormElement as Form
import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.DOMRect as DOMRect
import qualified GHCJS.DOM.DOMTokenList as DOMTokenList
import Language.Javascript.JSaddle (liftJSM)

import Reflex.Dom.Core hiding (fromJSString, divClass, Checkbox, CheckboxConfig, Input, setValue, Dropdown, DropdownConfig, HasValue(value), link, selectElement)

import Reflex.Dom.Active
import Data.Selectable
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Lenses

import Reflex.Dom.SemanticUI.Button
import Reflex.Dom.SemanticUI.Checkbox
import Reflex.Dom.SemanticUI.Container
import Reflex.Dom.SemanticUI.Dimmer
import Reflex.Dom.SemanticUI.Divider
import Reflex.Dom.SemanticUI.Dropdown
import Reflex.Dom.SemanticUI.Field
import Reflex.Dom.SemanticUI.Form
import Reflex.Dom.SemanticUI.Header
import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Image
import Reflex.Dom.SemanticUI.Input
import Reflex.Dom.SemanticUI.Label
import Reflex.Dom.SemanticUI.List
import Reflex.Dom.SemanticUI.Menu
import Reflex.Dom.SemanticUI.Message
import Reflex.Dom.SemanticUI.Segment
import Reflex.Dom.SemanticUI.Sticky
import Reflex.Dom.SemanticUI.Transition

import Reflex.Dom.SemanticUI.Paragraph

import Reflex.Dom.SemanticUI.UI

{-
--------------------------------------------------------------------------------
-- | The 'UI' class encapsulates how components 'a' are rendered in a certain
-- context 'r'.
class UI t m r a where
  type Return r a
  ui' :: (Restriction m ~ r, MonadWidget t m) => a -> m (El t, Return r a)
-}

-- | This instance is here to provide a more helpful and clear error message
-- when other instances are not selected
instance {-# OVERLAPPABLE #-} TypeError
      ( 'Text "Cannot use the component:"
  ':$$: 'ShowType a
  ':$$: 'Text "In the restricted context of:"
  ':$$: 'ShowType r
      ) => UI t m r a where
  ui' = error "impossible"

{-
ui :: forall r t m a. (MonadWidget t m, UI t m r a)
   => a -> Component r m (Return t m r a)
ui = fmap snd . ui'

ui_ :: forall r t m a. (MonadWidget t m, UI t m r a)
    => a -> Component r m ()
ui_ = void . ui
-}

ui :: forall t m r a. (Restriction m ~ r, MonadWidget t m, UI t m r a) => a -> m (Return t m r a)
ui = fmap snd . ui'

ui_ :: forall t m r a. (Restriction m ~ r, MonadWidget t m, UI t m r a) => a -> m ()
ui_ = void . ui

--------------------------------------------------------------------------------
-- Button instances

-- Buttons

instance (m' ~ m, t' ~ t) => UI t' m' (Ctx None) (Buttons t m a) where
  type Return t' m' (Ctx None) (Buttons t m a) = a

  ui' (Buttons config@ButtonsConfig {..} buttons) = do
    (e, results) <- semEl "div" attrs buttons
    return (e, results)
    where
      attrs = _config <> def
        { _classes = buttonsConfigClasses config
        }

instance (m' ~ m, t' ~ t) => UI t' m' (Ctx Buttons) (Button t m) where
  type Return t' m' (Ctx Buttons) (Button t m) = Event t ()
  ui' = ui'
instance (m' ~ m, t' ~ t) => UI t' m' (Ctx Buttons) (LabeledButton t m) where
  type Return t' m' (Ctx Buttons) (LabeledButton t m) = Event t ()
  ui' = ui'
instance t ~ t' => UI t' m (Ctx Buttons) (Conditional t) where
  type Return t' m (Ctx Buttons) (Conditional t) = ()
  ui' (Conditional ConditionalConfig {..})
    = semEl "div" config blank
    where
      config = def
        & elConfigClasses |~ "or"
        & elConfigAttributes .~ fmap (maybe mempty ("data-text" =:)) _dataText

-- Button

instance (m' ~ m, t' ~ t) => UI t' m' None (Button t m) where
  type Return t' m' None (Button t m) = Event t ()

  ui' (Button config@ButtonConfig {..} content) = do
    (e, _) <- semEl (toTagText _tag) elConfig
      $ case _animated of
        Just (AnimatedButton _ hiddenContent) -> do
          divClass "visible content" content
          divClass "hidden content" hiddenContent
        Nothing -> reComponent content
    return (e, domEvent Click e)
    where
      elConfig = _config <> def
        { _classes = buttonConfigClasses config
        }

instance t' ~ t => UI t' m (Ctx Buttons) (Icon t) where
  type Return t' m (Ctx Buttons) (Icon t) = ()
  ui' = unComponent . ui'
instance (m' ~ m, t' ~ t) => UI t' m' (Ctx Buttons) (Icons t m a) where
  type Return t' m' (Ctx Buttons) (Icons t m a) = a
  ui' = unComponent . ui'

-- LabeledButton

instance (m ~ m', t' ~ t) => UI t' m' None (LabeledButton t m) where
  -- TODO: return events for children?
  type Return t' m' None (LabeledButton t m) = Event t ()

  ui' (LabeledButton config@LabeledButtonConfig{..} content) = do
    (e, _) <- reComponent $ elWithAnim' "div" elConfig $ reComponent content
    return (e, domEvent Click e)
    where
      elConfig = _config <> def
        { _classes = labeledButtonConfigClasses config }

instance (m' ~ m, t' ~ t) => UI t' m' (Ctx LabeledButton) (Button t m) where
  type Return t' m' (Ctx LabeledButton) (Button t m) = Event t ()
  ui' = unComponent . ui'
instance (m ~ m', t ~ t') => UI t' m' (Ctx LabeledButton) (Label t m a) where
  type Return t' m' (Ctx LabeledButton) (Label t m a) = a
  ui' = unComponent . ui'

--------------------------------------------------------------------------------
-- Checkbox instances

instance UI t m None (Checkbox t) where
  type Return t m None (Checkbox t) = CheckboxResult t
  ui' (Checkbox label config@CheckboxConfig {..}) = do

    let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
          & initialAttributes .~ constAttrs
          & elementConfig_eventSpec %~ addEventSpecFlags
              (Proxy @(DomBuilderSpace m)) Click (const stopPropagation)

    (divEl, inputEl) <- unComponent $ elWithAnim' "div" divAttrs $ do
      (inputEl, _) <- Component $ element "input" cfg blank
      el "label" `mapComponent` activeText label
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
          let newValue = oldIndeterminate || not oldValue
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

    return (divEl, CheckboxResult
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
  type Return t' m' None (Container t m a) = a
  ui' (Container config@ContainerConfig {..} contents)
    = reComponent $ elWithAnim' "i" attrs contents
    where
      attrs = _config <> def
        { _classes = containerConfigClasses config
        }

--------------------------------------------------------------------------------
-- Dimmer instances

addBodyClasses :: [Text] -> DOM.JSM ()
addBodyClasses classes = catchJS $ do
  Just doc <- DOM.currentDocument
  Just body <- Document.getBody doc
  domTokenList <- Element.getClassList body
  DOMTokenList.add domTokenList classes

instance (m ~ m', t ~ t') => UI t' m' None (Dimmer t m a) where
  type Return t' m' None (Dimmer t m a) = a

  ui' (Dimmer config@DimmerConfig {..} content) = do
--    when _page $ liftJSM $ addBodyClasses ["dimmable"]

    let f Nothing d = flipDirection d
        f (Just d) _ = d

    rec

      let click = ffilter id $ tagActive _closeOnClick $ domEvent Click e

      dDir <- holdUniqDyn <=< foldDyn f (_dimmed ^. initial) $ leftmost
            [ fromMaybe never $ _dimmed ^. event
            , Just Out <$ click ]

      (e, a) <- reComponent $ elWithAnim' "div" (elConfig $ updated dDir) $
        reComponent content

    return (e, a)

    where

      elConfig evt = _config <> def
        { _classes = dimmerConfigClasses config
        , _transition = Just $ transConfig evt
        }

      transConfig evt = def
        & initialDirection .~ _dimmed ^. initial
        & event .~ fmap mkTrans evt

      mkTrans d = Transition Fade $ def
        & cancelling .~ True
        & direction ?~ d

--------------------------------------------------------------------------------
-- Divider instances

instance Dividable None where
  type Divide t m None a = DividerConfig t -> Either a (Component Divider m a)
                        -> Component None m a
  divider config@DividerConfig {..} eContent
    = reComponent $ elWithAnim "div" elConfig $ case eContent of
      Left a -> return a
      Right content -> reComponent content
      where
        elConfig = _config <> def
          { _classes = addContentDivider <$> dividerConfigClasses config }
        addContentDivider = case eContent of
          Left _ -> id
          Right _ -> addClass "horizontal"




instance t ~ t' => UI t' m None (Divider t) where
  type Return t' m None (Divider t) = ()

  ui' (Divider config@DividerConfig {..})
    = reComponent $ elWithAnim' "div" elConfig blank
    where
      elConfig = _config <> def
        { _classes = dividerConfigClasses config
        }

instance (m ~ m', t ~ t') => UI t' m' None (ContentDivider t m a) where
  type Return t' m' None (ContentDivider t m a) = a

  ui' (ContentDivider config@DividerConfig {..} content)
    = reComponent $ elWithAnim' "div" elConfig $ reComponent content
    where
      elConfig = _config <> def
        { _classes = addClass "horizontal" <$> dividerConfigClasses config
        }

instance (m ~ m', t ~ t') => UI t' m' (Ctx ContentDivider) (PageHeader t m a) where
  type Return t' m' (Ctx ContentDivider) (PageHeader t m a) = a
  ui' = unComponent . ui'
instance (m ~ m', t ~ t') => UI t' m' (Ctx ContentDivider) (Header t m a) where
  type Return t' m' (Ctx ContentDivider) (Header t m a) = a
  ui' = unComponent . ui'

--------------------------------------------------------------------------------
-- Dropdown instances

instance (Ord (f a), Ord a, m ~ m', Foldable f
         , MonadReader (Dynamic t (f a)) m, EventWriter t (First a) m)
  => UI t m' (Ctx SelectionDropdown) (DropdownItem m a) where
  type Return t m' (Ctx SelectionDropdown) (DropdownItem m a) = Event t (a, Component DropdownItem m ())
  ui' (DropdownItem value config@DropdownItemConfig {..}) = do

    undefined
--    isSelected <- Component $ asks $ Dynamic . fmap (elem value)
--    (e, _) <- reComponent $ elWithAnim' "div" (elConfig isSelected) $
--      reComponent _render
--    return (e, (value, _render) <$ domEvent Click e)
--      where
--        elConfig active = def
--          & classes .~ "item" <> ((\b -> if b then "active" else "") <$> active)

instance (Ord (f a), Ord a, t ~ t', m ~ m', Selectable f)
  => UI t' m' None (SelectionDropdown f t m a) where
  type Return t' m' None (SelectionDropdown f t m a) = Dynamic t (f a)
  ui' (SelectionDropdown config@DropdownConfig {..} preItems items) = do

    undefined
{-
    let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
          & initialAttributes .~ constAttrs
        constAttrs = "type" =: "hidden"

        elConfig = _config <> def
          { _classes = dropdownConfigClasses config
          , _attrs = Static $ "tabindex" =: "0" }

    rec
      isOpen <- holdDyn False $ leftmost
        [ True <$ gate (not <$> current isOpen) (domEvent Click divEl)
        , False <$ domEvent Blur divEl
        ]

      let menuConfig = mkMenuConfig (_value ^. initial)
            & component .~ True
            & value . event .~ _value ^. event
            & transition ?~ (def & initialDirection .~ Out
                                 & forceVisible .~ True
                                 & event .~ evt)

          evt = ffor (updated isOpen) $ \case
                  True -> mkTransition (Just In)
                  False -> mkTransition (Just Out)

          mkTransition d = Transition SlideDown $ def
            & cancelling .~ True & duration .~ 0.2 & direction .~ d

      (divEl, result) <- unComponent $ elWithAnim' "div" elConfig $ do
        Component $ element "input" cfg blank
        unComponent $ ui $ Icon "dropdown" def
        unComponent $ elWithAnim' "div" menuConfig $ do
          preItems
          evts <- traverse ui items
          leftmost evts

    return (divEl, fst result)
-}



instance (Ord (f a), Ord a, t ~ t', m ~ m', Selectable f)
  => UI t' m' None (MenuDropdown f t m a) where
  type Return t' m' None (MenuDropdown f t m a) = Dynamic t (f a)
  ui' (MenuDropdown config@DropdownConfig {..} items) = do

    let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
          & initialAttributes .~ constAttrs
        constAttrs = "type" =: "hidden"

        elConfig = _config <> def
          { _classes = dropdownConfigClasses config
          , _attrs = Static $ "tabindex" =: "0" }

    rec
      isOpen <- holdDyn False $ leftmost
        [ True <$ gate (not <$> current isOpen) (domEvent Click divEl)
        , False <$ domEvent Blur divEl
        ]

      let menuConfig = mkMenuConfig (_value ^. initial)
            & component .~ True
            & value . event .~ _value ^. event
            & transition ?~ (def & initialDirection .~ Out
                                 & forceVisible .~ True
                                 & event .~ evt)

          evt = ffor (updated isOpen) $ \case
                  True -> mkTransition (Just In)
                  False -> mkTransition (Just Out)

          mkTransition d = Transition SlideDown $ def
            & cancelling .~ True & duration .~ 0.2 & direction .~ d

      (divEl, result) <- unComponent $ elWithAnim' "div" elConfig $ do
        Component $ element "input" cfg blank
        unComponent $ ui $ Icon "dropdown" def
        unComponent $ ui $ Menu menuConfig items

    return (divEl, fst result)

--------------------------------------------------------------------------------
-- Menu instances

instance ( t ~ t', m ~ m', Ord a, Foldable f, Eq (f a)
         , MonadReader (Dynamic t (f a)) m, EventWriter t (First a) m)
  => UI t' m' (Ctx Menu) (MenuItem t m a) where
  type Return t' m' (Ctx Menu) (MenuItem t m a) = ()
  ui' (MenuItem value config@MenuItemConfig{..} widget) = do
    selected <- ask
    --let isSelected = Dynamic $ demuxed selected $ pure value
    let isSelected = Dynamic $ elem value <$> selected

    (e, _) <- reComponent $ elWithAnim' "div" (elConfig isSelected) widget
    Component $ tellEvent $ First value <$ domEvent Click e
    return (e, ())
      where
--        (_, _) = itemElAttrs config { _link = reLink _link }
--        reLink NoLink = StyleLink
--        reLink a = a
        config' = config & link .~ StyleLink
        elConfig isSelected = _config <> def
          { _classes = addClassMaybe <$> boolClass "active" isSelected
                                     <*> menuItemConfigClasses config'
          }

instance (t ~ t', m ~ m') => UI t' m' (Ctx Menu) (MenuItem' t m b) where
  type Return t' m' (Ctx Menu) (MenuItem' t m b) = b
  ui' (MenuItem' config@MenuItemConfig{..} widget)
    = reComponent $ elWithAnim' "div" elConfig widget
      where
        elConfig = _config <> def
          { _classes = menuItemConfigClasses $ config & link .~ NoLink }

instance (m ~ m', t ~ t') => UI t' m' Inline (Input t m a) where
  type Return t' m' Inline (Input t m a) = a -- InputResult t a
  ui' = unComponent . ui'

instance (Selectable f, Ord (f a), Ord a, t ~ t', m ~ m')
  => UI t' m' None (Menu f t m a b) where
  type Return t' m' None (Menu f t m a b) = (Dynamic t (f a), b)
  ui' (Menu config@MenuConfig{..} items)
    = reComponent $ elWithAnim' "div" elConfig $ do
    rec
      (b, evt) <- Component $
        runEventWriterT $ runReaderT (runComponent items) current

      current <- foldDyn mkCurrent (_value ^. initial) $ case _value ^. event of
        Just setValue -> leftmost [Left . getFirst <$> evt, Right <$> setValue]
        Nothing -> Left . getFirst <$> evt

    return (current, b)
    where
      elConfig = _config <> def
        { _classes = menuConfigClasses config }
      mkCurrent :: Either a (f a) -> f a -> f a
      mkCurrent (Left x) acc = selectElement x acc
      mkCurrent (Right acc) _ = acc


instance ( Ord a, m ~ m', t ~ t'
         , MonadReader (Dynamic t (f a)) m, EventWriter t (First a) m)
  => UI t' m' (Ctx Menu) (Menu f t m a b) where
  type Return t' m' (Ctx Menu) (Menu f t m a b) = b
  ui' (Menu config@MenuConfig{..} items) = do
    selected <- ask
    (el, (b, evt)) <- reComponent $ elWithAnim' "div" elConfig $
      Component $ runEventWriterT $ runReaderT (runComponent items) selected

    Component $ tellEvent evt
    return (el, b)

    where elConfig = _config <> def { _classes = menuConfigClasses $ config
                                               & component .~ True }

{-
    unComponent $ ui' $ Menu (conf & component .~ True) $ do
      tellEvent
      result <- items
-}

instance t ~ t' => UI t' m (Ctx Menu) (Divider t) where
  type Return t' m (Ctx Menu) (Divider t) = ()
  ui' = unComponent . ui'
instance (m ~ m', t ~ t') => UI t' m' (Ctx Menu) (Header t m a) where
  type Return t' m' (Ctx Menu) (Header t m a) = a
  ui' (Header conf content) = unComponent $ ui' $ Header conf' content
    where conf' = conf & component .~ True & item .~ True

--------------------------------------------------------------------------------
-- Flag instances

instance t ~ t' => UI t' m None (Flag t) where
  type Return t' m None (Flag t) = ()
  ui' (Flag flagActive FlagConfig {..})
    = reComponent $ elWithAnim' "i" config blank
    where
      config = _config
        & elConfigClasses .~ (flip addClass "flag" <$> flagActive)

instance t ~ t' => UI t' m Inline (Flag t) where
  type Return t' m Inline (Flag t) = ()
  ui' = unComponent . ui'

--------------------------------------------------------------------------------
-- Form / field instances

instance (m ~ m', t ~ t') => UI t' m' None (Form t m a) where
  type Return t' m' None (Form t m a) = a

  ui' (Form config@FormConfig {..} form) = reComponent $ do
    (formEl, formResult) <- elWithAnim' "form" elConfig $ reComponent form

    let e = DOM.uncheckedCastTo DOM.HTMLFormElement $ _element_raw formEl

    liftJSM $ EventM.on e GlobalEventHandlers.submit $ do
      consoleLog ("default prevented" :: Text)
      EventM.preventDefault

    return (formEl, formResult)
    where
      elConfig = _config <> def
        { _classes = formConfigClasses config
        }

instance (m ~ m', t ~ t') => UI t' m' (Ctx Form) (Buttons t m a) where
  type Return t' m' (Ctx Form) (Buttons t m a) = a
  ui' = unComponent . ui'
instance (m ~ m', t ~ t') => UI t' m' (Ctx Form) (Button t m) where
  type Return t' m' (Ctx Form) (Button t m) = Event t ()
  ui' = unComponent . ui'


instance (m ~ m', t ~ t') => UI t' m' (Ctx Form) (Field t m a) where
  type Return t' m' (Ctx Form) (Field t m a) = a

  ui' (Field config@FieldConfig {..} field)
    = reComponent $ elWithAnim' "div" elConfig $ reComponent field
    where
      elConfig = _config <> def
        { _classes = fieldConfigClasses config
        }

instance (m ~ m', t ~ t') => UI t' m' (Ctx Field) (Input t m a) where
  type Return t' m' (Ctx Field) (Input t m a) = a
  ui' = unComponent . ui'

--------------------------------------------------------------------------------
-- Header instances

instance HasHeader t m None where
  type HeaderType t m None a
        = HeaderConfig t -> Component Header m a -> Component None m a
  header config@HeaderConfig {..} widget
    = reComponent $ elWithAnim "div" elConfig $ case _image of
      Nothing -> case _icon of
        Nothing -> reComponent widget
        Just i -> ui_ i >> divClass "content" (reComponent widget)
      Just i -> ui_ i >> divClass "content" (reComponent widget)
    where
      msize = (fmap . fmap) headerSizeText _size
      addSize = maybe id addClass <$> msize
      elConfig = _config <> def
        { _classes = addSize <*> headerConfigClasses config
        }




instance (m ~ m', t ~ t') => UI t' m' None (PageHeader t m a) where
  type Return t' m' None (PageHeader t m a) = a
  ui' (PageHeader size config@HeaderConfig {..} widget)
    = reComponent $ elWithAnim' (headerSizeEl size) elConfig $ case _image of
      Nothing -> case _icon of
        Nothing -> reComponent widget
        Just i -> ui_ i >> divClass "content" (reComponent widget)
      Just i -> ui_ i >> divClass "content" (reComponent widget)
    where
      elConfig = _config <> def { _classes = headerConfigClasses config }

instance (m ~ m', t ~ t') => UI t' m' None (Header t m a) where
  type Return t' m' None (Header t m a) = a
  ui' (Header config@HeaderConfig {..} widget)
    = reComponent $ elWithAnim' "div" elConfig $ case _image of
      Nothing -> case _icon of
        Nothing -> reComponent widget
        Just i -> ui_ i >> divClass "content" (reComponent widget)
      Just i -> ui_ i >> divClass "content" (reComponent widget)
    where
      msize = (fmap . fmap) headerSizeText _size
      addSize = maybe id addClass <$> msize
      elConfig = _config <> def
        { _classes = addSize <*> headerConfigClasses config
        }

instance (t ~ t', m ~ m') => UI t' m' (Ctx Header) (Anchor t m a) where
  type Return t' m' (Ctx Header) (Anchor t m a) = AnchorResult t a
  ui' = unComponent . ui'
instance (m ~ m', t ~ t') => UI t' m' (Ctx Header) (Label t m a) where
  type Return t' m' (Ctx Header) (Label t m a) = a
  ui' = unComponent . ui'
instance (m' ~ m, t' ~ t) => UI t' m' (Ctx Header) (Icons t m a) where
  type Return t' m' (Ctx Header) (Icons t m a) = a
  ui' = unComponent . ui'
instance t' ~ t => UI t' m (Ctx Header) (Icon t) where
  type Return t' m (Ctx Header) (Icon t) = ()
  ui' = unComponent . ui'
instance t ~ t' => UI t' m (Ctx Header) (Image t) where
  type Return t' m (Ctx Header) (Image t) = ()
  ui' = unComponent . ui'

instance (t ~ t', m ~ m') => UI t' m' (Ctx Header) (SubHeader t m a) where
  type Return t' m' (Ctx Header) (SubHeader t m a) = a
  ui' (SubHeader content)
    = reComponent $ elWithAnim' "div" elConfig $ reComponent content
    where
      elConfig = def { _classes = "sub header" }

--------------------------------------------------------------------------------
-- Icon instances

instance t' ~ t => UI t' m None (Icon t) where
  type Return t' m None (Icon t) = ()

  ui' (Icon activeIcon config@IconConfig {..})
    = reComponent $ elWithAnim' "i" elConfig blank
    where
      elConfig = _config <> def
        { _classes = addClass <$> activeIcon <*> iconConfigClasses config
        , _attrs = maybe mempty ("title" =:) <$> _title
        }

instance (m' ~ m, t' ~ t) => UI t' m' None (Icons t m a) where
  type Return t' m' None (Icons t m a) = a

  ui' (Icons config@IconsConfig {..} icons)
    = reComponent $ elWithAnim' "i" elConfig $ reComponent icons
      where
        elConfig = _config <> def
          { _classes = iconsConfigClasss config
          }

instance t' ~ t => UI t' m (Ctx Icons) (Icon t) where
  type Return t' m (Ctx Icons) (Icon t) = ()
  ui' = unComponent . ui'

--------------------------------------------------------------------------------
-- Input instances

instance (m ~ m', t' ~ t) => UI t' m' None (Input t m a) where
  type Return t' m' None (Input t m a) = a

  ui' (Input config@InputConfig {..} contents)
    = reComponent $ elWithAnim' "div" elConfig $ reComponent contents
    where
      elConfig = _config <> def { _classes = inputConfigClasses config }

instance t' ~ t => UI t' m (Ctx Input) (Icon t) where
  type Return t' m (Ctx Input) (Icon t) = ()
  ui' = unComponent . ui'

instance (m ~ m', t ~ t') => UI t' m' (Ctx Input) (Label t m a) where
  type Return t' m' (Ctx Input) (Label t m a) = a
  ui' = unComponent . ui'

instance (Ord (f a), Ord a, Selectable f, m ~ m', t ~ t')
  => UI t' m' (Ctx Input) (MenuDropdown f t m a) where
  type Return t' m' (Ctx Input) (MenuDropdown f t m a) = Dynamic t (f a)
  ui' = unComponent . ui'

instance (m' ~ m, t' ~ t) => UI t' m' (Ctx Input) (Button t m) where
  type Return t' m' (Ctx Input) (Button t m) = Event t ()
  ui' = unComponent . ui'

--------------------------------------------------------------------------------
-- Label instances

instance t ~ t' => UI t' m (Ctx Label) (Detail t) where
  type Return t' m (Ctx Label) (Detail t) = ()
  ui' (Detail txt)
    = reComponent $ elWithAnim' "div" elConfig $ activeText txt
      where elConfig = def { _classes = "detail" }

instance (m ~ m', t ~ t') => UI t' m' None (Label t m a) where
  type Return t' m' None (Label t m a) = a

  ui' (Label config@LabelConfig {..} content)
    = reComponent $ elWithAnim' elType elConfig $ reComponent content
    where
      elConfig = _config <> def
        { _classes = labelConfigClasses config
        }
      elType = if _link then "a" else "div"

instance t' ~ t => UI t' m (Ctx Label) (Icon t) where
  type Return t' m (Ctx Label) (Icon t) = ()
  ui' = unComponent . ui'
instance (m' ~ m, t' ~ t) => UI t' m' (Ctx Label) (Icons t m a) where
  type Return t' m' (Ctx Label) (Icons t m a) = a
  ui' = unComponent . ui'

--------------------------------------------------------------------------------
-- List instances

instance (m ~ m', t ~ t') => UI t' m' None (List t m a) where
  type Return t' m' None (List t m a) = a
  ui' (List config@ListConfig {..} widget)
    = reComponent $ elWithAnim' "div" elConfig $ reComponent widget
    where
      elConfig = _config <> def
        { _classes = listConfigClasses config
        }

instance (m ~ m', t ~ t') => UI t' m' (Ctx List) (ListItem t m a) where
  type Return t' m' (Ctx List) (ListItem t m a) = a
  ui' (ListItem config@ListItemConfig {..} widget)
    = reComponent $ elWithAnim' (listItemElement _as) elConfig $ case _image of
      Nothing -> case _icon of
        Nothing -> reComponent widget
        Just i -> ui_ i >> divClass "content" (reComponent widget)
      Just i -> ui_ i >> divClass "content" (reComponent widget)
    where
      elConfig = _config <> def
        { _classes = listItemConfigClasses config
        }

instance (t ~ t', m ~ m') => UI t' m' (Ctx ListItem) (List t m a) where
  type Return t' m' (Ctx ListItem) (List t m a) = a
  ui' = unComponent . ui'

instance (t ~ t', m ~ m') => UI t' m' (Ctx ListItem) (ListHeader t m a) where
  type Return t' m' (Ctx ListItem) (ListHeader t m a) = a
  ui' (ListHeader content)
    = reComponent $ divClass' "header" $ reComponent content

instance (t ~ t', m ~ m') => UI t' m' (Ctx ListItem) (ListDescription t m a) where
  type Return t' m' (Ctx ListItem) (ListDescription t m a) = a
  ui' (ListDescription content)
    = reComponent $ divClass' "description" $ reComponent content

instance (t ~ t', m ~ m') => UI t' m' (Ctx ListItem) (Anchor t m a) where
  type Return t' m' (Ctx ListItem) (Anchor t m a) = AnchorResult t a
  ui' = unComponent . ui'

--------------------------------------------------------------------------------
-- Image instances

instance t ~ t' => UI t' m None (Image t) where
  type Return t' m None (Image t) = ()
  ui' (Image src config@ImageConfig {..})
    = elWithAnim' "img" elConfig blank
    where
      elConfig = _config <> def
        { _classes = imageConfigClasses config
        , _attrs = mkAttrs <$> src <*> _title
        }
      mkAttrs s t = "src" =: s <> maybe mempty ("title" =:) t

-- | Images in labels *must* be some form of spaced if they are not an 'Avatar',
-- or they will cause a line break. Default to spacing both sides allowing user
-- override to 'LeftSpaced' or 'RightSpaced'.
instance t ~ t' => UI t' m (Ctx Label) (Image t) where
  type Return t' m (Ctx Label) (Image t) = ()
  ui' (Image url conf@ImageConfig{..}) = unComponent $ ui' $ Image url conf'
    where conf' = conf { _spaced = mkSpaced <$> _shape <*> _spaced }
          mkSpaced mShape mSpaced = if mShape == Just Avatar then mSpaced
                                    else Just $ fromMaybe Spaced mSpaced

instance (m ~ m', t ~ t') => UI t' m' None (ContentImage t m a) where
  type Return t' m' None (ContentImage t m a) = a
  ui' (ContentImage src config@ImageConfig {..} content)
    = elWithAnim' "div" elConfig $ do
      a <- reComponent content
      void $ elWithAnim "img" imgConfig blank
      return a
    where
      elConfig = _config <> def
        { _classes = imageConfigClasses config }
      imgConfig = def { _attrs = mkAttrs <$> src <*> _title
                      , _classes = imageConfigClasses config }
      mkAttrs s t = "src" =: s <> maybe mempty ("title" =:) t

instance (m ~ m', t ~ t') => UI t' m' (Ctx Image) (Dimmer t m a) where
  type Return t' m' (Ctx Image) (Dimmer t m a) = a
  ui' = unComponent . ui'
instance (m ~ m', t ~ t') => UI t' m' (Ctx Image) (Label t m a) where
  type Return t' m' (Ctx Image) (Label t m a) = a
  ui' = unComponent . ui'

--------------------------------------------------------------------------------
-- Message instances

instance (t ~ t', m ~ m') => UI t' m' None (Message t m a) where
  type Return t' m' None (Message t m a) = a
  ui' (Message config@MessageConfig{..} content) = do

    let elConfig f = (_config <>) $ def
          & elConfigClasses .~ messageConfigClasses config & f

    case _dismissable of
      Nothing -> reComponent $ elWithAnim' "div" (elConfig id) $ case _icon of
        Just i -> ui_ i >> divClass "content" (reComponent content)
        Nothing -> reComponent content

      Just t -> do

        let dismissContent = do
              (e, _) <- ui' $ Icon "close" def
              result <- reComponent content
              return (result, (t <$) $ domEvent Click e)

        rec

          let elConfig' = elConfig $ elConfigTransition
                ?~ (def & event .~ closeEvent)

          (divEl, (result, closeEvent)) <-
            reComponent $ elWithAnim' "div" elConfig' $ case _icon of
              Just i -> ui_ i >> divClass "content" dismissContent
              Nothing -> dismissContent

        return (divEl, result)

instance (m ~ m', t ~ t') => UI t' m' (Ctx Message) (Header t m a) where
  type Return t' m' (Ctx Message) (Header t m a) = a
  ui' (Header config widget)
    = unComponent $ ui' $ Header (config & component .~ True) widget

-- TODO FIXME For removal:

instance (t ~ t', m ~ m') => UI t' m' None (Anchor t m a) where
  type Return t' m' None (Anchor t m a) = AnchorResult t a
  ui' (Anchor contents AnchorConfig{..}) = do
    (e, a) <- reComponent $ elWithAnim' "a" elConfig $ reComponent contents
    return (e, AnchorResult (domEvent Click e) a)
      where
        elConfig = _config
          & elConfigAttributes %~ (\a -> (maybe id (M.insert "href") <$> _href) <*> a)
instance (t ~ t', m ~ m') => UI t' m' Inline (Anchor t m a) where
  type Return t' m' Inline (Anchor t m a) = AnchorResult t a
  ui' = unComponent . ui'
instance (t ~ t', m ~ m') => UI t' m' (Ctx Message) (Anchor t m a) where
  type Return t' m' (Ctx Message) (Anchor t m a) = AnchorResult t a
  ui' = unComponent . ui'

instance t' ~ t => UI t' m Inline (Icon t) where
  type Return t' m Inline (Icon t) = ()
  ui' = unComponent . ui'
instance (m' ~ m, t' ~ t) => UI t' m' Inline (Icons t m a) where
  type Return t' m' Inline (Icons t m a) = a
  ui' = unComponent . ui'
-- | Force images to appear inline in inline context
instance t ~ t' => UI t' m Inline (Image t) where
  type Return t' m Inline (Image t) = ()
  ui' (Image url conf) = unComponent $ ui' $ Image url $ conf & inline |~ True
instance (m ~ m', t ~ t') => UI t' m' Inline (Label t m a) where
  type Return t' m' Inline (Label t m a) = a
  ui' = unComponent . ui'

--------------------------------------------------------------------------------
-- Segment instances

instance (t ~ t', m ~ m') => UI t' m' None (Segment t m a) where
  type Return t' m' None (Segment t m a) = a
  ui' (Segment config@SegmentConfig{..} content)
    = reComponent $ elWithAnim' "div" elConfig content
    where
      elConfig = _config <> def { _classes = segmentConfigClasses config }

--------------------------------------------------------------------------------
-- Sticky instances

-- | This function is very basic and not efficient in comparison to the real
-- semantic-ui javascript
runSticky :: Bool -> DOM.Element -> DOM.JSM ()
runSticky pushing sticky = do
  Just window <- DOM.currentWindow
  Just context <- Node.getParentElement sticky

  domTokenList <- Element.getClassList sticky
  let removeClass :: DOM.MonadJSM m => DOM.JSString -> m ()
      removeClass c = DOMTokenList.remove domTokenList [c]
      addClass :: DOM.MonadJSM m => DOM.JSString -> m ()
      addClass c = DOMTokenList.add domTokenList [c]

      setTop :: DOM.MonadJSM m => Bool -> m ()
      setTop True = addClass "top" >> removeClass "bottom"
      setTop False = addClass "bottom" >> removeClass "top"

      setFixed :: DOM.MonadJSM m => Bool -> m ()
      setFixed True = addClass "fixed" >> removeClass "bound"
      setFixed False = addClass "bound" >> removeClass "fixed"

  -- Set initial values
  setTop True
  setFixed False

  void $ EventM.on window GlobalEventHandlers.scroll $ do

    stickyRect <- Element.getBoundingClientRect sticky
    contextRect <- Element.getBoundingClientRect context

    stickyTop <- DOMRect.getY stickyRect
    stickyHeight <- DOMRect.getHeight stickyRect
    let stickyBottom = stickyTop + stickyHeight

    contextTop <- DOMRect.getY contextRect
    contextHeight <- DOMRect.getHeight contextRect
    let contextBottom = contextTop + contextHeight

    isFixed <- DOMTokenList.contains domTokenList ("fixed" :: DOM.JSString)
    isTop <- DOMTokenList.contains domTokenList ("top" :: DOM.JSString)

    if isFixed
    then -- line 515
      if isTop
      then do
        -- Top fixed sticky reached top of context
        when (contextTop >= stickyTop) $ setFixed False
        -- Top fixed sticky reached bottom of context
        when (stickyBottom >= contextBottom) $ setFixed False >> setTop False
      else do
        -- Bottom fixed sticky reached bottom of context
        when (contextBottom <= stickyBottom) $ setFixed False
        -- Bottom fixed sticky reached top of context
        when (stickyTop <= contextTop) $ setFixed False >> setTop True

    else -- line 557
      if isTop
      then do
        -- Top bound sticky context went off page
        when (contextTop <= 0) $ setFixed True
        -- Catch fast scrolls: the bottom of the context is now above the
        -- viewport
        when (contextBottom <= 0) $ setFixed False >> setTop False
      else do
        if pushing
        then do
          windowHeight <- Window.getInnerHeight window
          -- Context bottom crossed lower window bound
          when (fromIntegral windowHeight <= contextBottom) $ setFixed True
        else
          -- Bottom bound sticky crossed fully into view
          when (stickyTop >= 0) $ setFixed True >> setTop True
        -- Catch fast scrolls: the top of the context is now in view
        when (contextTop >= 0) $ setFixed False

  return ()

instance (t ~ t', m ~ m') => UI t' m' None (Sticky t m a) where
  type Return t' m' None (Sticky t m a) = a
  ui' (Sticky config@StickyConfig{..} content) = do

    (stickyEl, a) <- reComponent $ elWithAnim' "div" elConfig content

    liftJSM $ runSticky _pushing (_element_raw stickyEl)

    return (stickyEl, a)

    where
      elConfig = _config <> def { _classes = stickyConfigClasses config }

