-- | Semantic-UI Flag elements
--
-- <https://semantic-ui.com/elements/flag.html>
module Reflex.Dom.SemanticUI.Flag where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Control.Lens.Iso
import Control.Monad (void)
import Data.Default
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

-- | Config for 'flag's.
data FlagConfig t = FlagConfig
  { _flagConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''FlagConfig
#endif

instance HasElConfig t (FlagConfig t) where
  elConfig = flagConfig_elConfig

instance Reflex t => Default (FlagConfig t) where
  def = FlagConfig def

-- | Create a flag, returning the 'Element'. Available types are listed here:
-- <https://semantic-ui.com/elements/flag.html>
flag'
  :: UI t m => Active t Text -> FlagConfig t
  -> m (Element EventResult (DomBuilderSpace m) t)
flag' dynFlag FlagConfig {..} = fst <$> ui' "i" elConf blank
  where
    elConf = _flagConfig_elConfig <> def
      { _classes = flip addClass "flag" <$> dynFlag }

-- | Create a flag. Avaliable types are listed here:
-- <https://semantic-ui.com/elements/flag.html>
flag :: UI t m => Active t Text -> FlagConfig t -> m ()
flag f = void . flag' f

#ifndef USE_TEMPLATE_HASKELL
#include "Flag.th.hs"
#endif
