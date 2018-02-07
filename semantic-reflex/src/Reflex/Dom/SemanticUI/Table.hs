{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI.Table where

import Control.Lens.TH (makeLensesWith, lensRules, simpleLenses)
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
  { _tableType :: Active t TableType
  , _tableColor :: Active t (Maybe Color)
  , _tableAttached :: Active t (Maybe VerticalAttached)
  , _tableElConfig :: ActiveElConfig t
  }
makeLensesWith (lensRules & simpleLenses .~ True) ''TableConfig

instance HasElConfig t (TableConfig t) where
  elConfig = tableElConfig

instance Reflex t => Default (TableConfig t) where
  def = TableConfig
    { _tableType = pure Celled
    , _tableColor = pure Nothing
    , _tableAttached = pure Nothing
    , _tableElConfig = def
    }

tableConfigClasses :: Reflex t => TableConfig t -> Active t Classes
tableConfigClasses TableConfig {..} = dynClasses
  [ pure $ Just "ui table"
  , Just . toClassText <$> _tableType
  , fmap toClassText <$> _tableColor
  , fmap toClassText <$> _tableAttached
--  , boolClass "link" _listLink
  ]

table'
  :: UI t m => TableConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
table' config@TableConfig {..} widget
  = uiElement' "table" elConf widget
  where
    elConf = _tableElConfig <> def
      { _classes = tableConfigClasses config }

table :: UI t m => TableConfig t -> m a -> m a
table c = fmap snd . table' c

thead :: MonadWidget t m => m a -> m a
thead = el "thead"

tbody :: MonadWidget t m => m a -> m a
tbody = el "tbody"

th :: MonadWidget t m => m a -> m a
th = el "th"

td :: MonadWidget t m => m a -> m a
td = el "td"

tr :: MonadWidget t m => m a -> m a
tr = el "tr"
