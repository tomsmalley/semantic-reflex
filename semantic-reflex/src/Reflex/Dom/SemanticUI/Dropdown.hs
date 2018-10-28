{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Semantic-UI Dropdown widgets
--
-- <https://semantic-ui.com/modules/dropdown.html>
module Reflex.Dom.SemanticUI.Dropdown
  (

  -- * Single select dropdowns
    dropdown
  , dropdownWithWrapper
  , textDropdown
  , Dropdown(..)
  , DropdownConfig(..)

  -- * Searchable dropdown configuration
  , SearchDropdownConfig(..)
  , searchByToText
  , searchByShow
  , allowAdditionsValue

  -- * Lenses
  , dropdown_value
  , dropdown_search
  , dropdown_change
  , dropdown_open
  , dropdown_element

  , dropdownConfig_placeholder
  , dropdownConfig_selection
  , dropdownConfig_compact
  , dropdownConfig_fluid
  , dropdownConfig_inline
  , dropdownConfig_unselectable
  , dropdownConfig_pointing
  , dropdownConfig_upward
  , dropdownConfig_search

  , searchDropdownConfig_function
  , searchDropdownConfig_allowAdditions

  ) where

import Control.Lens
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
import Control.Monad.Reader
import Data.Bool (bool)
import Data.Default
import Data.Foldable (for_, traverse_)
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import Language.Javascript.JSaddle (liftJSM)
import Reflex hiding (list)
import Reflex.Dom.Builder.Class
import Reflex.Dom.Class
import Reflex.Dom.Core (El, GhcjsDomSpace, HasValue(..), keydown)
import Reflex.Dom.Widget.Basic

import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.Types as Types

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition
import Reflex.Dom.SemanticUI.Icon

-- | Configuration for searchable dropdowns. You can use 'searchByToText' or
-- 'searchByShow' to construct this easily.
data SearchDropdownConfig a = SearchDropdownConfig
  { _searchDropdownConfig_function :: Text -> a -> Bool
  -- ^ The function used for searching items. First argument is the text in the
  -- search box, second value is some item from the list.
  , _searchDropdownConfig_allowAdditions :: Bool
  -- ^ By default, when a user exits a searchable dropdown without choosing an
  -- item, the search text is cleared. When allowAdditions is set, the text is
  -- not cleared. You should use 'allowAdditionsValue' to get the current value.
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''SearchDropdownConfig

-- | Case insensitive search through the text given by a function
searchByToText :: (a -> Text) -> SearchDropdownConfig a
searchByToText toText = SearchDropdownConfig
  { _searchDropdownConfig_function = \s a ->
    T.toCaseFold s `T.isInfixOf` T.toCaseFold (toText a)
  , _searchDropdownConfig_allowAdditions = False
  }

-- | Case insensitive search through the text given by the 'Show' instance
searchByShow :: Show a => SearchDropdownConfig a
searchByShow = searchByToText $ T.pack . show

-- | Config for new semantic dropdowns
data DropdownConfig t a = DropdownConfig
  { _dropdownConfig_placeholder :: Dynamic t Text
  -- ^ Placeholder text
  , _dropdownConfig_selection :: Dynamic t Bool
  -- ^ Selection dropdowns appear like form inputs
  , _dropdownConfig_compact :: Dynamic t Bool
  -- ^ A compact dropdown has no minimum width
  , _dropdownConfig_fluid :: Dynamic t Bool
  -- ^ Fluid dropdowns flow to fill their container
  , _dropdownConfig_inline :: Dynamic t Bool
  -- ^ A dropdown can be formatted to appear inline in other content
  , _dropdownConfig_unselectable :: Bool
  -- ^ If this is true and an item is clicked when it is already the current
  -- value, the value will be reset to the initial value
  , _dropdownConfig_pointing :: Dynamic t Bool
  -- ^ A dropdown menu can be pointing
  , _dropdownConfig_upward :: Dynamic t Bool
  -- ^ The menu can open upwards instead of downwards
  , _dropdownConfig_search :: Maybe (SearchDropdownConfig a)
  -- ^ A dropdown can have a search input for narrowing a list of results
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''DropdownConfig

instance Reflex t => Default (DropdownConfig t a) where
  def = DropdownConfig
    { _dropdownConfig_placeholder = pure mempty
    , _dropdownConfig_selection = pure True
    , _dropdownConfig_compact = pure False
    , _dropdownConfig_fluid = pure False
    , _dropdownConfig_inline = pure False
    , _dropdownConfig_pointing = pure False
    , _dropdownConfig_upward = pure False
    , _dropdownConfig_unselectable = False
    , _dropdownConfig_search = Nothing
    }

dropdownConfigClasses
  :: Reflex t => DropdownConfig t a -> Dynamic t Bool -> Dynamic t Classes
dropdownConfigClasses DropdownConfig {..} isOpen = dynClasses'
  [ pure $ Just "ui dropdown"
  , boolClass "compact" _dropdownConfig_compact
  , boolClass "fluid" _dropdownConfig_fluid
  , boolClass "selection" _dropdownConfig_selection
  , boolClass "inline" _dropdownConfig_inline
  , boolClass "pointing" _dropdownConfig_pointing
  , boolClass "upward" _dropdownConfig_upward
  , boolClass "active" isOpen
  , pure $ "search" <$ _dropdownConfig_search
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

-- | Result of running a 'dropdown'
data Dropdown t a = Dropdown
  { _dropdown_value :: Dynamic t a
  -- ^ Currently selected value
  , _dropdown_search :: Dynamic t Text
  -- ^ Current entry in the search box, if present
  , _dropdown_change :: Event t a
  -- ^ User driven change events
  , _dropdown_open :: Dynamic t Bool
  -- ^ Is the menu open?
  , _dropdown_element :: El t
  -- ^ Wrapper element
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''Dropdown

instance HasValue (Dropdown t a) where
  type Value (Dropdown t a) = Dynamic t a
  value = _dropdown_value

-- | Get a 'Dynamic' of 'Either' some user added 'Text' value or a real selection.
-- Useful in conjunction with '_searchDropdownConfig_allowAdditions' = 'True'.
allowAdditionsValue :: Reflex t => Dropdown t a -> Dynamic t (Either Text a)
allowAdditionsValue d = f <$> _dropdown_search d <*> value d <*> _dropdown_open d
  where f search a open
          | open || T.null search = Right a
          | otherwise = Left search

-- | Single selection dropdown widget
dropdown
  :: forall active t m k f.
    ( SingActive active, Monad f, Ord k, UI t m, Ord (f k), Foldable f
    , Types.MonadJSM (Performable m), Types.MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace )
  => DropdownConfig t k
  -> f k
  -> Event t (f k)
  -> TaggedActive active t (Map k (m ()))
  -> m (Dropdown t (f k))
dropdown = dropdownWithWrapper $ \f -> ui' "div" (f def)

-- | Single selection dropdown widget, with custom wrapper. This can, for
-- example, be used to turn labels into dropdowns:
--
-- > dropdownWithWrapper (\f -> label' (f def)) def Nothing never items
--
-- You'll probably want to set _dropdownConfig_selection to 'False' when using
-- this.
--
dropdownWithWrapper
  :: forall active t m k f.
    ( SingActive active, Monad f, Ord k, UI t m, Ord (f k), Foldable f
    , Types.MonadJSM (Performable m), Types.MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace )
  => (forall x. (forall cfg. HasElConfig t cfg => cfg -> cfg) -> m x -> m (El t, x))
  -> DropdownConfig t k
  -> f k
  -> Event t (f k)
  -> TaggedActive active t (Map k (m ()))
  -> m (Dropdown t (f k))
dropdownWithWrapper wrapper config@DropdownConfig {..} ini setVal items = mdo

  let alterConfig :: forall cfg. HasElConfig t cfg => cfg -> cfg
      alterConfig cfg = cfg
        & classes <>~ Dyn (dropdownConfigClasses config isOpen)
        & attrs <>~ pure (M.singleton "tabindex" "0")
      search :: Maybe (Dynamic t Text)
      search = value <$> searchInput

  -- Wrapper element
  (dropdownElement, (searchInput, clickItem)) <- wrapper alterConfig $ mdo

    -- Add the search text input element when required
    searchInput <- for _dropdownConfig_search $ \_ -> do
      i <- inputElement $ def
        & initialAttributes .~ M.singleton "class" "search"
        & inputElementConfig_setValue .~ leftmost
          [ "" <$ anyChoice
          , case _dropdownConfig_search of
            Just SearchDropdownConfig {..} | _searchDropdownConfig_allowAdditions -> never
            _ -> "" <$ ffilter not (updated isOpen)
          ]
      -- Prevent the click events from propagating to the wrapper element
      let htmlEl = Types.uncheckedCastTo Types.HTMLElement $ _inputElement_raw i
      void $ liftJSM $ EventM.on htmlEl GlobalEventHandlers.click $ EventM.stopPropagation
      pure i

    -- Selected element / default text
    let mkTextClasses sel mtxt = T.unwords $ fmapMaybe id
          [ Just "text"
          , "default" <$ guard (null sel)
          , case mtxt of
            Just t | not (T.null t) -> Just "filtered"
            _ -> Nothing
          ]
    elDynClass "div" (mkTextClasses <$> selection <*> sequence search) $
      dyn_ $ ffor selection $ \sel ->
        if null sel
        then dynText _dropdownConfig_placeholder
        else for_ sel $ \k -> taggedActive id (void . dyn) $ fromMaybe blank . M.lookup k <$> items

    -- Togglable menu with list of items
    (menuEl, clickItem) <- ui' "div" (mkMenuConfig _dropdownConfig_upward $ updated isOpen) $ do

      -- List the items
      (elemMap, userClickItem :: Event t k) <-
        taggedActiveSelectViewListWithKey' selection items $ \k v isSelected -> do
          let mkClasses selected visible = mconcat
                [ "item"
                , if selected then "selected" else ""
                , if Just False == visible then "filtered" else ""
                ]
              cs = mkClasses <$> isSelected <*> sequence isVisible
              isVisible = do
                SearchDropdownConfig {..} <- _dropdownConfig_search
                ds <- search
                pure $ flip _searchDropdownConfig_function k <$> ds
          (itemEl, _) <- ui' "div" (def & classes .~ Dyn cs) $ taggedActive id (void . dyn) v

          -- Prevent the click events from propagating to the wrapper element
          -- These click events are used to close the menu, and click events on
          -- the wrapper are used to toggle it
          let itemHTML = Types.uncheckedCastTo Types.HTMLElement $ _element_raw itemEl
          void $ liftJSM $ EventM.on itemHTML GlobalEventHandlers.click $ EventM.stopPropagation

          pure ((isVisible, itemEl), domEvent Click itemEl)

      -- Display a message if all items have been filtered out
      for_ _dropdownConfig_search $ \_ -> do
        let visible = do
              m <- taggedActive pure id elemMap
              or <$> sequence (M.mapMaybe fst m)
        dyn_ $ ffor visible $ \v -> unless v $
          divClass "message" $ text "No results found."

      -- Adjust scroll position if the selected element is out of view
      alterScroll menuEl selection $ M.map snd . M.mapKeysMonotonic pure <$> elemMap

      pure userClickItem

    icon "dropdown" $ def & iconConfig_flipped .~ Dyn
      (bool Nothing (Just VerticallyFlipped) <$> _dropdownConfig_upward)
    pure (searchInput, clickItem)

      -- Changes caused by user interactions
  let userChoice :: Event t (f k) = leftmost
        [ flip tag (keydown ArrowDown dropdownElement) $ lookupNextKey ini
            <$> current selection <*> taggedActive pure current items
        , flip tag (keydown ArrowUp dropdownElement) $ lookupPrevKey ini
            <$> current selection <*> taggedActive pure current items
        , case _dropdownConfig_unselectable of
          True -> attachWith (\old new -> if old == pure new then ini else pure new)
            (current selection) clickItem
          False -> pure <$> clickItem
        ]
      -- Changes caused by user or setVal
      anyChoice = leftmost [userChoice, setVal]

  -- Currently selected value
  selection :: Dynamic t (f k) <- holdDyn ini anyChoice

  -- Stop keyboard events from escaping the dropdown context
  let htmlElement = Types.uncheckedCastTo Types.HTMLElement $ _element_raw dropdownElement
  void $ liftJSM $ EventM.on htmlElement GlobalEventHandlers.keyDown $ do
    EventM.uiKeyCode >>= \k -> case keyCodeLookup (fromIntegral k) of
      ArrowDown -> EventM.preventDefault
      ArrowUp -> EventM.preventDefault
      Escape -> HTMLElement.blur htmlElement
      _ -> pure ()

  -- Determine when to open / close the menu
  let searchFocusChange = maybe never (updated . _inputElement_hasFocus) searchInput
      (searchFocus, searchBlur) = fanEither $ ffor searchFocusChange $ \case
        True -> Left ()
        False -> Right ()
      toggleEvents = leftmost
        [ domEvent Click dropdownElement
        , keydown Enter dropdownElement
        ]
      closeEvents = leftmost
        [ domEvent Blur dropdownElement
        , void clickItem
        , searchBlur
        ]
      openEvents = leftmost
        [ keydown ArrowDown dropdownElement
        , keydown ArrowUp dropdownElement
        , searchFocus
        ]
  isOpen <- holdUniqDyn <=< holdDyn False $ leftmost
    [ False <$ closeEvents, True <$ openEvents
    , not <$> tag (current isOpen) toggleEvents
    ]

  pure $ Dropdown
    { _dropdown_value = selection
    , _dropdown_search = fromMaybe "" search
    , _dropdown_change = userChoice
    , _dropdown_open = isOpen
    , _dropdown_element = dropdownElement
    }

-- | Dropdown menu element config, this hides/shows the menu according to the
-- input event
mkMenuConfig :: Reflex t => Dynamic t Bool -> Event t Bool -> ActiveElConfig t
mkMenuConfig upward transition = def
  & classes |~ "menu"
  & action ?~ def
    { _action_initialDirection = Out
    , _action_transition = attachWith mkTransition (current upward) transition
    , _action_transitionStateClasses = forceVisible
    }
  where mkTransition up open = Transition (if up then SlideUp else SlideDown) (Just $ if open then In else Out) $ def
          & transitionConfig_duration .~ 0.2
          & transitionConfig_cancelling .~ True

-- | Alter the scroll position of the dropdown menu when the selected item
-- is outside of its bounds
alterScroll
  :: (Types.MonadJSM (Performable m), Ord k, PerformEvent t m)
  => El t
  -> Dynamic t k
  -> TaggedActive active t (Map k (El t))
  -> m ()
alterScroll menuEl' val elMap = do
  let menuEl = _element_raw menuEl'
  performEvent_ $ ffor (updated $ M.lookup <$> val <*> taggedActive pure id elMap) $
    traverse_ $ \itemEl' -> liftJSM $ do
    let itemEl = Types.uncheckedCastTo Types.Element $ _element_raw itemEl'
        itemHTMLEl = Types.uncheckedCastTo Types.HTMLElement $ _element_raw itemEl'
    itemOffset <- round <$> HTMLElement.getOffsetTop itemHTMLEl
    itemHeight <- round <$> Element.getClientHeight itemEl
    scrollTop <- Element.getScrollTop menuEl
    menuHeight <- round <$> Element.getClientHeight menuEl
    let itemBelow = itemOffset + itemHeight > scrollTop + menuHeight
        itemAbove = itemOffset < scrollTop
    when (itemBelow || itemAbove) $ Element.setScrollTop menuEl itemOffset

-- | Searchable text dropdown. Behaves mostly like a text input, but with values
-- that can also be selected from a dropdown box.
textDropdown
  :: forall active t m.
    ( SingActive active, UI t m
    , Types.MonadJSM (Performable m), Types.MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace )
  => DropdownConfig t Text -> Text -> Event t Text -> TaggedActive active t [Text]
  -> m (Dropdown t Text)
textDropdown conf ini evt items = do
  let conf' = conf
        { _dropdownConfig_search = Just (searchByToText id)
          { _searchDropdownConfig_allowAdditions = True
          }
        }
  d <- dropdown conf' (Identity ini) (fmapCheap Identity evt) $ ffor items $
    M.fromList . fmap (\t -> (t, text t))
  let v = ffor (allowAdditionsValue d) $ \case
        Left t -> t
        Right (Identity t) -> t
  pure $ Dropdown
    { _dropdown_search = v
    , _dropdown_value = v
    , _dropdown_change = runIdentity <$> _dropdown_change d
    , _dropdown_open = _dropdown_open d
    , _dropdown_element = _dropdown_element d
    }

