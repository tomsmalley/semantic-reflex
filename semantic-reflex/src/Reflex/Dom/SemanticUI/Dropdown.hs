{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Dom.SemanticUI.Dropdown where

import Control.Lens ((<&>), (?~))
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Control.Monad.Reader
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Default
import Data.Text (Text)
import Language.Javascript.JSaddle (liftJSM)
import Reflex hiding (list)
import Data.Map.Lazy (Map)

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition
import Reflex.Dom.SemanticUI.Icon

import Reflex.Dom.Core hiding (SetValue, Dropdown, DropdownConfig, list, textInput)

import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.Types as Types

import qualified Data.Map.Lazy as M

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
--  { _dropdownConfig_Value :: SetValue t a
  { _dropdownConfig_placeholder :: Dynamic t Text
  , _dropdownConfig_selection :: Dynamic t Bool
  , _dropdownConfig_compact :: Dynamic t Bool
  , _dropdownConfig_fluid :: Dynamic t Bool
  , _dropdownConfig_item :: Dynamic t Bool
  , _dropdownConfig_inline :: Dynamic t Bool
  , _dropdownConfig_as :: Dynamic t (Maybe DropdownStyle)
  , _dropdownConfig_unselectable :: Bool
  , _dropdownConfig_closeOnClickSelection :: Bool
  , _dropdownConfig_elConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''DropdownConfig

instance HasElConfig t (DropdownConfig t) where
  elConfig = dropdownConfig_elConfig

instance Reflex t => Default (DropdownConfig t) where
  def = DropdownConfig
    { _dropdownConfig_placeholder = pure mempty
    , _dropdownConfig_selection = pure False
    , _dropdownConfig_compact = pure False
    , _dropdownConfig_fluid = pure False
    , _dropdownConfig_item = pure False
    , _dropdownConfig_inline = pure False
    , _dropdownConfig_as = pure Nothing
    , _dropdownConfig_unselectable = False
    , _dropdownConfig_closeOnClickSelection = True
    , _dropdownConfig_elConfig = def
    }

dropdownConfigClasses
  :: Reflex t => DropdownConfig t -> Dynamic t Bool -> Dynamic t Classes
dropdownConfigClasses DropdownConfig {..} isOpen = dynClasses'
  [ pure $ Just "ui selection dropdown"
  , boolClass "compact" _dropdownConfig_compact
  , boolClass "fluid" _dropdownConfig_fluid
  , boolClass "selection" _dropdownConfig_selection
  , boolClass "item" _dropdownConfig_item
  , boolClass "inline" _dropdownConfig_inline
  , boolClass "active" isOpen
  , fmap toClassText <$> _dropdownConfig_as
  ]

lookupNextKey :: (Foldable f, Monad f, Ord k) => f k -> f k -> Map k a -> f k
-- FIXME: needs to be lookupMin but requires newer containers than is provided
-- with the current reflex-platform ghc version
lookupNextKey ini mCurrent m
  | null mCurrent = if not $ M.null m then pure $ fst $ M.findMin m else ini
  | otherwise = mCurrent >>= \k -> case M.lookupIndex k m of
    Just i -> pure $ fst $ M.elemAt (min (pred $ M.size m) $ succ i) m
    Nothing -> ini

lookupPrevKey :: (Foldable f, Monad f, Ord k) => f k -> f k -> Map k a -> f k
-- FIXME: needs to be lookupMax but requires newer containers than is provided
-- with the current reflex-platform ghc version
lookupPrevKey ini mCurrent m
  | null mCurrent = if not $ M.null m then pure $ fst $ M.findMax m else ini
  | otherwise = mCurrent >>= \k -> case M.lookupIndex k m of
    Just i -> pure $ fst $ M.elemAt (max 0 $ pred i) m
    Nothing -> ini

data Dropdown t a = Dropdown
  { _dropdown_value :: Dynamic t a
  , _dropdown_blur :: Event t ()
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''Dropdown

dropdown
  :: forall active t m k f.
    ( SingActive active, Monad f, Ord k, UI t m, Ord (f k), Foldable f
    , Types.MonadJSM (Performable m), Types.MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace )
  => DropdownConfig t -> f k -> TaggedActive active t (Map k (m ()))
  -> m (Dropdown t (f k))
dropdown conf ini = fmap snd . dropdown' conf ini

dropdown'
  :: forall active t m k f.
    ( SingActive active, Monad f, Ord k, UI t m, Ord (f k), Foldable f
    , Types.MonadJSM (Performable m), Types.MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace )
  => DropdownConfig t -> f k -> TaggedActive active t (Map k (m ()))
  -> m (El t, Dropdown t (f k))
dropdown' config@DropdownConfig {..} ini items = mdo

  let elConf = {- TODO: _dropdownConfig_ElConfig <> -} def
        { _classes = Dyn $ dropdownConfigClasses config isOpen
        , _attrs = pure ("tabindex" =: "0")
        }

  (e, (selection, clickItem)) <- ui' "div" elConf $ mdo
    icon "dropdown" def

    void $ dyn $ ffor dSelection $ \f ->
      if null f
      then divClass "default text" $ dynText _dropdownConfig_placeholder
      else for_ f $ \k -> void $ divClass "text" $ taggedActive id (void . dyn)
        $ fromMaybe blank . M.lookup k <$> items

    let menuEl = Types.uncheckedCastTo Types.Element $ _element_raw menuEl'
        menuA = def
          & actionInitialDirection .~ Out
          & actionForceVisible .~ True
          & actionEvent ?~ fmap mkTransition eOpen
        mkTransition open = Transition SlideDown $ def
          & transitionDuration .~ 0.2
          & transitionCancelling .~ True
          & transitionDirection ?~ if open then In else Out

    (menuEl', (clickItem, dSelection)) <- ui' "div" (def & classes |~ "menu" & action ?~ menuA) $ do
      (elemMap, eMaybeK) <- taggedActiveSelectViewListWithKey dSelection
        (M.mapKeysMonotonic pure <$> items) $ \_k v dSelected -> do
          let itemConf = def & classes .~ Dyn dClasses
              dClasses = dSelected <&> \case
                True -> "item active selected"
                False -> "item"
          (itemEl, _) <- ui' "div" itemConf $ taggedActive id (void . dyn) v

          -- Prevent the click events from propagating to the wrapper element
          -- These click events are used to close the menu, and click events on
          -- the wrapper are used to toggle it
          let itemHTML = Types.uncheckedCastTo Types.HTMLElement $ _element_raw itemEl
          void $ liftJSM $ EventM.on itemHTML GlobalEventHandlers.click $ EventM.stopPropagation

          pure (itemEl, domEvent Click itemEl)

        -- Alter the scroll position of the dropdown menu when the selected item
        -- is outside of its bounds
      performEvent_ $ ffor (updated $ M.lookup <$> dSelection <*> taggedActive pure id elemMap) $
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

      fmap ((,) (void eMaybeK)) $ holdDyn ini $ leftmost
        [ flip tag (keydown ArrowDown e) $ lookupNextKey ini
            <$> current dSelection <*> taggedActive pure current items
        , flip tag (keydown ArrowUp e) $ lookupPrevKey ini
            <$> current dSelection <*> taggedActive pure current items
        , case _dropdownConfig_unselectable of
          True -> attachWith (\old new -> if old == new then ini else new)
            (current dSelection) eMaybeK
          False -> eMaybeK
        ]

    pure (dSelection, clickItem)

  -- Add event listeners
  let htmlElement = Types.uncheckedCastTo Types.HTMLElement $ _element_raw e
  void $ liftJSM $ EventM.on htmlElement GlobalEventHandlers.keyDown $ do
    EventM.uiKeyCode >>= \k -> case keyCodeLookup (fromIntegral k) of
      Space -> EventM.preventDefault
      ArrowDown -> EventM.preventDefault
      ArrowUp -> EventM.preventDefault
      Escape -> HTMLElement.blur htmlElement
      _ -> pure ()

  let toggleEvents = domEvent Click e <> keydown Space e <> keydown Enter e
      closeEvents = domEvent Blur e <> if _dropdownConfig_closeOnClickSelection then clickItem else never
      openEvents = keydown ArrowDown e <> keydown ArrowUp e

  isOpen <- holdUniqDyn <=< holdDyn False $ leftmost
    [ False <$ closeEvents, True <$ openEvents
    , not <$> tag (current isOpen) toggleEvents
    ]

  let eOpen = updated isOpen

  pure $ (,) e $ Dropdown
    { _dropdown_value = selection
    , _dropdown_blur = closeEvents
    }

