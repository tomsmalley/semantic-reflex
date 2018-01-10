{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}

module Reflex.Dom.SemanticUI.Sticky where

import Control.Monad
import Data.Default
import Data.Semigroup hiding (First)
import Reflex
import Reflex.Dom.Core

import Reflex.Dom.Active
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
import Language.Javascript.JSaddle (liftJSM)

data StickyConfig t = StickyConfig
  { _stickyPushing :: Bool
  , _stickyElConfig :: ActiveElConfig t
  }

instance Reflex t => Default (StickyConfig t) where
  def = StickyConfig
    { _stickyPushing = False
    , _stickyElConfig = def
    }

stickyConfigClasses :: Reflex t => StickyConfig t -> Active t Classes
stickyConfigClasses StickyConfig {..} = activeClasses
  [ Static $ Just "ui sticky"
  ]

-- | This function is very basic and not efficient in comparison to the real
-- semantic-ui javascript
runSticky :: Bool -> DOM.Element -> DOM.JSM ()
runSticky pushing sticky = do
  Just window <- DOM.currentWindow
  Just context <- Node.getParentElement sticky

  domTokenList <- Element.getClassList sticky
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

  -- Set initial values
  setTopM True
  setFixedM False

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

sticky' :: MonadWidget t m => StickyConfig t -> m a -> m (El t, a)
sticky' config@StickyConfig{..} content = do

  (stickyEl, a) <- uiElement' "div" elConf content

  liftJSM $ runSticky _stickyPushing (_element_raw stickyEl)

  return (stickyEl, a)

  where
    elConf = _stickyElConfig <> def { _classes = stickyConfigClasses config }

