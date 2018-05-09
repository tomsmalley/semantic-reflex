module Reflex.Dom.SemanticUI.Table where

#ifdef USE_TEMPLATE_HASKELL
import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
#else
import Control.Lens.Type
#endif

import Data.Default
import Data.Semigroup ((<>))
import Reflex
import Reflex.Dom.Core

import Reflex.Active
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Transition

data TableType = Celled | Definition | Structured
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText TableType where
  toClassText Celled = "celled"
  toClassText Definition = "definition"
  toClassText Structured = "structured"


data TableConfig t = TableConfig
  { _tableConfig_type :: Active t (Maybe TableType)
  , _tableConfig_color :: Active t (Maybe Color)
  , _tableConfig_attached :: Active t (Maybe VerticalAttached)
  , _tableConfig_elConfig :: ActiveElConfig t
  }
#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''TableConfig
#endif

instance HasElConfig t (TableConfig t) where
  elConfig = tableConfig_elConfig

instance Reflex t => Default (TableConfig t) where
  def = TableConfig
    { _tableConfig_type = pure Nothing
    , _tableConfig_color = pure Nothing
    , _tableConfig_attached = pure Nothing
    , _tableConfig_elConfig = def
    }

tableConfigClasses :: Reflex t => TableConfig t -> Active t Classes
tableConfigClasses TableConfig {..} = dynClasses
  [ pure $ Just "ui table"
  , fmap toClassText <$> _tableConfig_type
  , fmap toClassText <$> _tableConfig_color
  , fmap toClassText <$> _tableConfig_attached
--  , boolClass "link" _listLink
  ]

table'
  :: UI t m => TableConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
table' config@TableConfig {..} widget
  = ui' "table" elConf widget
  where
    elConf = _tableConfig_elConfig <> def
      { _classes = tableConfigClasses config }

table :: UI t m => TableConfig t -> m a -> m a
table c = fmap snd . table' c

thead :: DomBuilder t m => m a -> m a
thead = el "thead"

tbody :: DomBuilder t m => m a -> m a
tbody = el "tbody"

th :: DomBuilder t m => m a -> m a
th = el "th"

td :: DomBuilder t m => m a -> m a
td = el "td"

tr :: DomBuilder t m => m a -> m a
tr = el "tr"

#ifndef USE_TEMPLATE_HASKELL
#include "Table.th.hs"
#endif
