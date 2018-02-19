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

import Reflex.Dom.Core hiding (SetValue, DropdownConfig, list, textInput)

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
--  { _dropdownValue :: SetValue t a
  { _dropdownPlaceholder :: Dynamic t Text
  , _dropdownSelection :: Dynamic t Bool
  , _dropdownCompact :: Dynamic t Bool
  , _dropdownFluid :: Dynamic t Bool
  , _dropdownItem :: Dynamic t Bool
  , _dropdownInline :: Dynamic t Bool
  , _dropdownAs :: Dynamic t (Maybe DropdownStyle)
  , _dropdownUnselectable :: Bool
  , _dropdownElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''DropdownConfig

instance HasElConfig t (DropdownConfig t) where
  elConfig = dropdownElConfig

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
  :: Reflex t => DropdownConfig t -> Dynamic t Bool -> Dynamic t Classes
dropdownConfigClasses DropdownConfig {..} isOpen = dynClasses'
  [ pure $ Just "ui selection dropdown"
  , boolClass "compact" _dropdownCompact
  , boolClass "fluid" _dropdownFluid
  , boolClass "selection" _dropdownSelection
  , boolClass "item" _dropdownItem
  , boolClass "inline" _dropdownInline
  , boolClass "active" isOpen
  , fmap toClassText <$> _dropdownAs
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


dropdown
  :: forall active t m k f.
    (SingActive active, Monad f, Ord k, MonadWidget t m, Ord (f k), Foldable f)
  => DropdownConfig t -> f k -> TaggedActive active t (Map k (m ()))
  -> m (Dynamic t (f k))
dropdown conf ini = fmap snd . dropdown' conf ini

dropdown'
  :: forall active t m k f.
    (SingActive active, Monad f, Ord k, MonadWidget t m, Ord (f k), Foldable f)
  => DropdownConfig t -> f k -> TaggedActive active t (Map k (m ()))
  -> m (El t, Dynamic t (f k))
dropdown' config@DropdownConfig {..} ini items = mdo

  let elConf = {- TODO: _dropdownElConfig <> -} def
        { _classes = Dyn $ dropdownConfigClasses config isOpen
        , _attrs = pure ("tabindex" =: "0")
        }

  (e, a) <- ui' "div" elConf $ mdo
    icon "dropdown" def

    void $ dyn $ ffor dSelection $ \f ->
      if null f
      then divClass "default text" $ dynText _dropdownPlaceholder
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

    (menuEl', dSelection) <- ui' "div" (def & classes |~ "menu" & action ?~ menuA) $ do
      (elemMap, eMaybeK) <- taggedActiveSelectViewListWithKey dSelection
        (M.mapKeysMonotonic pure <$> items) $ \_k v dSelected -> do
          let itemConf = def & classes .~ Dyn dClasses
              dClasses = dSelected <&> \case
                True -> "item active selected"
                False -> "item"
          (e, _) <- ui' "div" itemConf $ taggedActive id (void . dyn) v
          pure (e, domEvent Click e)

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

      holdDyn ini $ leftmost
        [ flip tag (keydown ArrowDown e) $ lookupNextKey ini
            <$> current dSelection <*> taggedActive pure current items
        , flip tag (keydown ArrowUp e) $ lookupPrevKey ini
            <$> current dSelection <*> taggedActive pure current items
        , case _dropdownUnselectable of
          True -> attachWith (\old new -> if old == new then ini else new)
            (current dSelection) eMaybeK
          False -> eMaybeK
        ]

    pure dSelection

  -- Add event listeners
  let htmlElement = Types.uncheckedCastTo Types.HTMLElement $ _element_raw e
  void $ liftJSM $ EventM.on htmlElement GlobalEventHandlers.keyDown $ do
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

