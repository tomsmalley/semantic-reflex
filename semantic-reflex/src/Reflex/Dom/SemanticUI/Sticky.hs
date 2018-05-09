{-# LANGUAGE TypeFamilies #-}

module Reflex.Dom.SemanticUI.Sticky where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Control.Monad
import Data.Default
import Data.Semigroup hiding (First)
import Reflex
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.GlobalEventHandlers as GlobalEventHandlers
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.DOMRect as DOMRect
import qualified GHCJS.DOM.DOMTokenList as DOMTokenList
import Language.Javascript.JSaddle (MonadJSM, liftJSM)

data StickyConfig t = StickyConfig
  { _stickyConfig_pushing :: Bool
  , _stickyConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''StickyConfig
#endif

instance HasElConfig t (StickyConfig t) where
  elConfig = stickyConfig_elConfig

instance Reflex t => Default (StickyConfig t) where
  def = StickyConfig
    { _stickyConfig_pushing = False
    , _stickyConfig_elConfig = def
    }

stickyConfigClasses :: Reflex t => StickyConfig t -> Active t Classes
stickyConfigClasses StickyConfig {..} = dynClasses
  [ pure $ Just "ui sticky top bound"
  ]

-- | This function is very basic and not efficient in comparison to the real
-- semantic-ui javascript
runSticky :: Bool -> DOM.Element -> DOM.JSM ()
runSticky pushing stickyEl = do
  Just window <- DOM.currentWindow
  Just context <- Node.getParentElement stickyEl

  domTokenList <- Element.getClassList stickyEl
  let removeClassM :: DOM.MonadJSM m => DOM.JSString -> m ()
      removeClassM c = DOMTokenList.remove domTokenList [c]
      addClassM :: DOM.MonadJSM m => DOM.JSString -> m ()
      addClassM c = DOMTokenList.add domTokenList [c]

      setTopM :: DOM.MonadJSM m => Bool -> m ()
      setTopM True = addClassM "top" >> removeClassM "bottom"
      setTopM False = addClassM "bottom" >> removeClassM "top"

      setFixedM :: DOM.MonadJSM m => Bool -> m ()
      setFixedM True = addClassM "fixed" >> removeClassM "bound"
      setFixedM False = addClassM "bound" >> removeClassM "fixed"

  void $ EventM.on window GlobalEventHandlers.scroll $ do

    stickyRect <- Element.getBoundingClientRect stickyEl
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
        when (contextTop >= stickyTop) $ setFixedM False
        -- Top fixed sticky reached bottom of context
        when (stickyBottom >= contextBottom) $ setFixedM False >> setTopM False
      else do
        -- Bottom fixed sticky reached bottom of context
        when (contextBottom <= stickyBottom) $ setFixedM False
        -- Bottom fixed sticky reached top of context
        when (stickyTop <= contextTop) $ setFixedM False >> setTopM True

    else -- line 557
      if isTop
      then do
        -- Top bound sticky context went off page
        when (contextTop <= 0) $ setFixedM True
        -- Catch fast scrolls: the bottom of the context is now above the
        -- viewport
        when (contextBottom <= 0) $ setFixedM False >> setTopM False
      else do
        if pushing
        then do
          windowHeight <- Window.getInnerHeight window
          -- Context bottom crossed lower window bound
          when (fromIntegral windowHeight <= contextBottom) $ setFixedM True
        else
          -- Bottom bound sticky crossed fully into view
          when (stickyTop >= 0) $ setFixedM True >> setTopM True
        -- Catch fast scrolls: the top of the context is now in view
        when (contextTop >= 0) $ setFixedM False

  return ()

sticky'
  :: (MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace, UI t m)
  => StickyConfig t -> m a -> m (El t, a)
sticky' config@StickyConfig{..} content = do

  (stickyEl, a) <- ui' "div" elConf content

  liftJSM $ runSticky _stickyConfig_pushing (_element_raw stickyEl)

  return (stickyEl, a)

  where
    elConf = _stickyConfig_elConfig <> def { _classes = stickyConfigClasses config }

sticky
  :: (MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace, UI t m)
  => StickyConfig t -> m a -> m a
sticky conf = fmap snd . sticky' conf

#ifndef USE_TEMPLATE_HASKELL
#include "Sticky.th.hs"
#endif
