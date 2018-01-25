{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Dom.SemanticUI.Dropdown where

import Control.Lens
import Control.Monad.Reader
import Control.Applicative (Alternative(..))
import Data.Foldable (for_, traverse_)
import Data.Traversable (for)
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Default
import Data.Text (Text)
import Language.Javascript.JSaddle
import Reflex hiding (list)
import Data.Map.Lazy (Map)
import Data.Align

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition
import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Input

import Reflex.Dom.Core hiding (SetValue, DropdownConfig, list, textInput)

import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.Types as Types

import qualified Data.Map.Lazy as M
import qualified Data.Text as T

{-
data DropdownAction
  = Activate
  | Combo
  | Select
  | Hide
  deriving (Eq, Show)

instance ToJSVal DropdownAction where
  toJSVal action = valMakeString $ case action of
    Activate -> "activate"
    Combo -> "combo"
    -- Select doesn't seem to work, so we use activate and prevent the text from
    -- being set in the dropdown element by removing the wrapping "default text"
    -- div around the placeholder.
    Select -> "activate"
    Hide -> "hide"
-}

data DropdownStyle = DropdownButton | DropdownLabel
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText DropdownStyle where
  toClassText DropdownButton = "button"
  toClassText DropdownLabel = "label"

-- | Config for new semantic dropdowns
data DropdownConfig t = DropdownConfig
--  { _dropdownValue :: SetValue t a
  { _dropdownPlaceholder :: Active t Text
  , _dropdownSelection :: Active t Bool
  , _dropdownCompact :: Active t Bool
  , _dropdownFluid :: Active t Bool
  , _dropdownItem :: Active t Bool
  , _dropdownInline :: Active t Bool
  , _dropdownAs :: Active t (Maybe DropdownStyle)
  , _dropdownUnselectable :: Bool
  , _dropdownElConfig :: ActiveElConfig t
  }
--  , _textOnly :: Bool
--  , _maxSelections :: Maybe Int
--  , _useLabels :: Bool
--  , _fullTextSearch :: Bool
--  , _action :: DropdownAction

instance Reflex t => Default (DropdownConfig t) where
  def = DropdownConfig
    { _dropdownPlaceholder = pure mempty
    , _dropdownSelection = pure False
    , _dropdownCompact = pure False
    , _dropdownFluid = pure False
    , _dropdownItem = pure False
    , _dropdownInline = pure False
    , _dropdownAs = pure Nothing
    , _dropdownUnselectable = False
    , _dropdownElConfig = def
    }

dropdownConfigClasses
  :: Reflex t => DropdownConfig t -> Dynamic t Bool -> Active t Classes
dropdownConfigClasses DropdownConfig {..} isOpen = activeClasses
  [ Static $ Just "ui selection dropdown"
  , boolClass "compact" _dropdownCompact
  , boolClass "fluid" _dropdownFluid
  , boolClass "selection" _dropdownSelection
  , boolClass "item" _dropdownItem
  , boolClass "inline" _dropdownInline
  , Dynamic $ boolClass "active" isOpen
  , fmap toClassText <$> _dropdownAs
  ]

dropdown
  :: forall t m k f. (Monad f, Ord k, MonadWidget t m, Ord (f k), Foldable f)
  => DropdownConfig t -> f k -> Dynamic t (Map k (m ()))
  -> m (Dynamic t (f k))
dropdown conf init = fmap snd . dropdown' conf init

lookupNextKey :: (Foldable f, Monad f, Ord k) => f k -> f k -> Map k a -> f k
-- FIXME: needs to be lookupMin but requires newer containers than is provided
-- with the current reflex-platform ghc version
lookupNextKey init f m
  | null f = if not $ M.null m then pure $ fst $ M.findMin m else init
  | otherwise = f >>= \k -> case M.lookupIndex k m of
    Just i -> pure $ fst $ M.elemAt (min (pred $ M.size m) $ succ i) m
    Nothing -> init

lookupPrevKey :: (Foldable f, Monad f, Ord k) => f k -> f k -> Map k a -> f k
-- FIXME: needs to be lookupMax but requires newer containers than is provided
-- with the current reflex-platform ghc version
lookupPrevKey init f m
  | null f = if not $ M.null m then pure $ fst $ M.findMax m else init
  | otherwise = f >>= \k -> case M.lookupIndex k m of
    Just i -> pure $ fst $ M.elemAt (max 0 $ pred i) m
    Nothing -> init


-- | Create a dynamically-changing set of widgets, one of which is selected at
-- any time.
selectViewListWithKey' :: forall t m k v a.
  (Adjustable t m, Ord k, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t k
  -> Dynamic t (Map k v)
  -> (k -> Dynamic t v -> Dynamic t Bool -> m (El t, Event t a))
  -> m (Dynamic t (Map k (El t)), Event t k)
selectViewListWithKey' selection vals mkChild = do
  let selectionDemux = demux selection
  selectChild <- listWithKey vals $ \k v -> do
    let selected = demuxed selectionDemux k
    (e, selectSelf) <- mkChild k v selected
    pure (e, k <$ selectSelf)
  pure
    ( fmap fst <$> selectChild
    , switchPromptlyDyn $ leftmost . fmap snd . M.elems <$> selectChild
    )

dropdown'
  :: forall t m k f. (Monad f, Ord k, MonadWidget t m, Ord (f k), Foldable f)
  => DropdownConfig t -> f k -> Dynamic t (Map k (m ()))
  -> m (El t, Dynamic t (f k))
dropdown' config@DropdownConfig {..} init items = mdo
  (e, a) <- uiElement' "div" (elConf isOpen) $ mdo
    icon "dropdown" def

    dyn $ ffor selection $ \f ->
      if null f
      then divClass "default text" $ text "Default text"
      else for_ f $ \k -> void $ divClass "text" $ dyn $
        fromMaybe blank . M.lookup k <$> items

    let menuEl = Types.uncheckedCastTo Types.Element $ _element_raw menuEl'
    (menuEl', selection) <- menu eOpen $ do
      (dynKeyToEl, eMaybeK) <- selectViewListWithKey' selection
        (M.mapKeysMonotonic pure <$> items) renderItem

      selection <- holdDyn init $ leftmost
        [ tag (lookupNextKey init <$> current selection <*> current items) $
            keydown ArrowDown e
        , tag (lookupPrevKey init <$> current selection <*> current items) $
            keydown ArrowUp e
        , case _dropdownUnselectable of
          True -> attachWith (\old new -> if old == new then init else new)
            (current selection) eMaybeK
          False -> eMaybeK
        ]

        -- Alter the scroll position of the dropdown menu when the selected item
        -- is outside of its bounds
      performEvent_ $ ffor (updated $ M.lookup <$> selection <*> dynKeyToEl) $
        traverse_ $ \itemEl' -> liftJSM $ do

        let itemEl = Types.uncheckedCastTo Types.Element
                    $ _element_raw itemEl'
            itemHTMLEl = Types.uncheckedCastTo Types.HTMLElement
                       $ _element_raw itemEl'
        itemOffset <- round <$> HTMLElement.getOffsetTop itemHTMLEl
        itemHeight <- round <$> Element.getClientHeight itemEl
        scrollTop <- Element.getScrollTop menuEl
        menuHeight <- round <$> Element.getClientHeight menuEl
        let itemBelow = itemOffset + itemHeight > scrollTop + menuHeight
            itemAbove = itemOffset < scrollTop
        when (itemBelow || itemAbove) $ Element.setScrollTop menuEl itemOffset


      pure (selection)

    pure selection

  -- Add event listeners
  let htmlElement = Types.uncheckedCastTo Types.HTMLElement $ _element_raw e
  liftJSM $ EventM.on htmlElement GlobalEventHandlers.keyDown $ do
    EventM.uiKeyCode >>= \k -> case keyCodeLookup (fromIntegral k) of
      Space -> EventM.preventDefault
      ArrowDown -> EventM.preventDefault
      ArrowUp -> EventM.preventDefault
      Escape -> HTMLElement.blur htmlElement
      _ -> pure ()

  let toggleEvents = keydown Space e <> keydown Enter e
      closeEvents = domEvent Blur e
      openEvents = domEvent Click e <> keydown ArrowDown e <> keydown ArrowUp e

  isOpen <- holdUniqDyn <=< holdDyn False $ leftmost
    [ False <$ closeEvents, True <$ openEvents
    , not <$> tag (current isOpen) toggleEvents
    ]

  let eOpen = updated isOpen

  pure (e, a)

  where
    elConf isOpen = {- _dropdownElConfig <> -} def
      { _classes = dropdownConfigClasses config isOpen
      , _attrs = pure ("tabindex" =: "0")
      }

    renderItem k dV dSelected = do
      let itemConf = def & classes .~ Dynamic dClasses
          dClasses = dSelected <&> \case
            True -> "item active selected"
            False -> "item"

      (e, _) <- uiElement' "div" itemConf $ do
        dyn dV
      pure (e, domEvent Click e)

    menu eOpen = uiElement' "div" $ def
      & classes |~ "menu"
      & transition ?~ menuTransition eOpen
    menuTransition eOpen = def
      & transConfigInitialDirection .~ Out
      & transConfigForceVisible .~ True
      & transConfigEvent .~ fmap mkTransition eOpen
    mkTransition dir = Transition SlideDown $ def
      & transitionDuration .~ 0.2
      & transitionCancelling .~ True

--------------------------------------------------------------------------------
{-
searchDropdown
  :: forall t m k f.
    (Monad f, Ord k, MonadWidget t m, Ord (f k), Foldable f)
  => DropdownConfig t
  -> f k
  -> Dynamic t (Map k (m ()))
  -> Dynamic t [(Text, k)]
  -> m (Dynamic t (f k))
searchDropdown config init items = fmap snd . searchDropdown' config init items

searchDropdown'
  :: forall t m k f.
    (Monad f, Ord k, MonadWidget t m, Ord (f k), Foldable f)
  => DropdownConfig t -> f k -> Dynamic t (Map k (m ()))
  -> Dynamic t [(Text, k)]
  -> m (El t, Dynamic t (f k))
searchDropdown' config@DropdownConfig {..} init items search = mdo

  (dropdownEl, (ti, a)) <- uiElement' "div" (elConf isOpen) $ mdo
    icon "dropdown" def

    -- The search input
    ti <- textInput $ def
      & textInputAttrs |~ ("class" =: "search" <> "autocomplete" =: "off"
                        <> "tabindex" =: "0")
      & textInputPlaceholder .~ (selection <&> \s ->
        if null s then "Default text" else "")
      -- Clear input when selection is updated
      & textInputValue . event ?~ ("" <$ closeEvents)

    tiValue <- holdUniqDyn $ _textInput_value ti
    let hasInput = not . T.null <$> tiValue :: Dynamic t Bool

    -- Display the selected value
    dyn $ ffor ((,) <$> selection <*> hasInput) $ \(s, hi) -> case hi of
      False -> for_ s $ \k -> void $ divClass "text" $ dyn $
        fromMaybe blank . M.lookup k <$> items
      True -> blank

    let restricted = restrict <$> tiValue <*> search <*> items
        restrict :: Text -> [(Text, k)] -> Map k (m ()) -> Map k (m ())
        restrict "" _ m = m
        restrict query all m =
          let check = T.isInfixOf (T.toLower query) . T.toLower
              possible = map snd $ filter (check . fst) all
           in M.filterWithKey (\k _ -> k `elem` possible) m


--    if selected.offsetTop > menu.scrollTop
--    then menu.scrollTop = selected.offsetTop
--    else ()

    let menuEl = Types.uncheckedCastTo Types.Element $ _element_raw menuEl'
    (menuEl', selection) <- menu eOpen $ do
      eMaybeK :: Event t (f k, El t) <- selectViewListWithKey selection
        (M.mapKeysMonotonic pure <$> restricted) renderItem

      -- Alter the scroll position of the dropdown menu when the selected item
      -- is outside of its bounds
      {-
        liftJSM $ do
          error "here"
          itemEl <- Types.unsafeCastTo Types.Element
                $ _element_raw itemEl'
          itemHTMLEl <- Types.unsafeCastTo Types.HTMLElement
                $ _element_raw itemEl'
          itemOffset <- round <$> HTMLElement.getOffsetTop itemHTMLEl
          itemHeight <- round <$> Element.getClientHeight itemEl
          scrollTop <- Element.getScrollTop menuEl
          menuHeight <- round <$> Element.getClientHeight menuEl
          --when (itemOffset + itemHeight > scrollTop + menuHeight || itemOffset < scrollTop) $ Element.setScrollTop menuEl itemOffset
          error $ show scrollTop
          consoleLog scrollTop
          consoleLog itemOffset
          Element.setScrollTop menuEl itemOffset
-}

      holdDyn init $ leftmost
        [ tag (lookupNextKey' init <$> current selection <*> current restricted)
          $ keydown ArrowDown dropdownEl
        , tag (lookupPrevKey' init <$> current selection <*> current restricted)
          $ keydown ArrowUp dropdownEl
        , case _dropdownUnselectable of
          True -> attachWith (\old (new, _) -> if old == new then init else new)
            (current selection) eMaybeK
          False -> fmap fst eMaybeK
        ]

    performEvent_ $ ffor (updated selection) $ \(_, itemEl') -> liftJSM $ error "1"

    pure (ti, selection)

  -- Add event listeners
  let inputElement = Types.uncheckedCastTo Types.HTMLElement $ _textInput_element ti
      divElement = Types.uncheckedCastTo Types.HTMLElement $ _element_raw dropdownEl
  liftJSM $ do
    EventM.on inputElement GlobalEventHandlers.keyDown $ do
      EventM.uiKeyCode >>= \k -> case keyCodeLookup (fromIntegral k) of
        Space -> EventM.preventDefault
        ArrowDown -> EventM.preventDefault
        ArrowUp -> EventM.preventDefault
        Escape -> HTMLElement.blur inputElement >> HTMLElement.blur divElement
        Enter -> HTMLElement.blur inputElement >> HTMLElement.blur divElement
        _ -> pure ()
    EventM.on divElement GlobalEventHandlers.keyDown $ do
      EventM.uiKeyCode >>= \k -> case keyCodeLookup (fromIntegral k) of
        Escape -> HTMLElement.blur inputElement >> HTMLElement.blur divElement
        _ -> pure ()

  -- This is a hack to allow the users click to be registered before blurring
  -- closes the menu
  tiBlur <- delay 0 $ domEvent Blur ti

  let toggleEvents = domEvent Click dropdownEl -- <> keydown Space dropdownEl <> keydown Enter dropdownEl
      closeEvents = tiBlur <> domEvent Blur dropdownEl
      openEvents = domEvent Focus ti <> keydown ArrowDown dropdownEl <> keydown ArrowUp dropdownEl

  isOpen <- holdUniqDyn <=< holdDyn False $ leftmost
    [ False <$ closeEvents, True <$ openEvents
    , not <$> tag (current isOpen) toggleEvents
    ]

  let eOpen = updated isOpen

  pure (dropdownEl, a)

  where
    elConf isOpen = {- _dropdownElConfig <> -} def
      { _classes = dropdownConfigClasses config isOpen <> "search"
      }

    renderItem k dV dSelected = do
      let itemConf = def & classes .~ Dynamic dClasses
          dClasses = dSelected <&> \case
            True -> "item active selected"
            False -> "item"

      (e, _) <- uiElement' "div" itemConf $ do
        dyn dV
      pure (e <$ domEvent Click e)

    menu eOpen = uiElement' "div" $ def
      & classes |~ "menu"
      & transition ?~ menuTransition eOpen
    menuTransition eOpen = def
      & transConfigInitialDirection .~ Out
      & transConfigForceVisible .~ True
      & transConfigEvent .~ fmap mkTransition eOpen
    mkTransition dir = Transition SlideDown $ def
      & transitionDuration .~ 0.2
      & transitionCancelling .~ True
-}
