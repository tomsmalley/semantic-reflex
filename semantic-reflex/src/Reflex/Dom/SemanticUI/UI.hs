{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Dom.SemanticUI.UI where

import Data.Type.Equality
import Control.Monad (void)
import Control.Monad.Trans
import Data.Default (Default(..))
import Data.Semigroup ((<>))

import Data.Proxy

import Reflex.Dom.Core (El, MonadWidget)

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Divider
import Reflex.Dom.SemanticUI.Lenses
import Reflex.Dom.SemanticUI.Transition


type family Tagged (m :: * -> *)

class Example t (a :: *) where
  type ExampleReturn t a
  a :: (Monad m, Tagged m ~ t) => a -> m (ExampleReturn t a)

data A
data A' a
data B = B

instance Example A B where
  type ExampleReturn A B = ()
  a B = return ()

instance Example (A' a) B where
  type ExampleReturn (A' a) B = ()
  a B = return ()

---

newtype Test r m a = Test
  { runTest :: m a
  } deriving (Functor, Applicative, Monad)

type instance Tagged (Test r m) = r

test :: Monad m => Test A m ()
test = a B

---

class UI t (m :: * -> *) r a where
  type Return t m r a
  ui' :: (Restriction m ~ r, MonadWidget t m) => a -> m (El t, Return t m r a)

