-- src/Reflex/Dom/SemanticUI/Table.hs:34:1-63: Splicing declarations
tableConfig_attached ::
  forall t_a2LaA.
  Lens' (TableConfig t_a2LaA) (Active t_a2LaA (Maybe VerticalAttached))
tableConfig_attached
  f_a2LwE
  (TableConfig x1_a2LwF x2_a2LwG x3_a2LwH x4_a2LwI)
  = fmap
      (\ y1_a2LwJ -> TableConfig x1_a2LwF x2_a2LwG y1_a2LwJ x4_a2LwI)
      (f_a2LwE x3_a2LwH)
{-# INLINE tableConfig_attached #-}
tableConfig_color ::
  forall t_a2LaA.
  Lens' (TableConfig t_a2LaA) (Active t_a2LaA (Maybe Color))
tableConfig_color
  f_a2LwK
  (TableConfig x1_a2LwL x2_a2LwM x3_a2LwN x4_a2LwO)
  = fmap
      (\ y1_a2LwP -> TableConfig x1_a2LwL y1_a2LwP x3_a2LwN x4_a2LwO)
      (f_a2LwK x2_a2LwM)
{-# INLINE tableConfig_color #-}
tableConfig_elConfig ::
  forall t_a2LaA.
  Lens' (TableConfig t_a2LaA) (ActiveElConfig t_a2LaA)
tableConfig_elConfig
  f_a2LwR
  (TableConfig x1_a2LwS x2_a2LwT x3_a2LwU x4_a2LwV)
  = fmap
      (\ y1_a2LwW -> TableConfig x1_a2LwS x2_a2LwT x3_a2LwU y1_a2LwW)
      (f_a2LwR x4_a2LwV)
{-# INLINE tableConfig_elConfig #-}
tableConfig_type ::
  forall t_a2LaA.
  Lens' (TableConfig t_a2LaA) (Active t_a2LaA (Maybe TableType))
tableConfig_type
  f_a2LwX
  (TableConfig x1_a2LwY x2_a2LwZ x3_a2Lx0 x4_a2Lx1)
  = fmap
      (\ y1_a2Lx3 -> TableConfig y1_a2Lx3 x2_a2LwZ x3_a2Lx0 x4_a2Lx1)
      (f_a2LwX x1_a2LwY)
{-# INLINE tableConfig_type #-}
