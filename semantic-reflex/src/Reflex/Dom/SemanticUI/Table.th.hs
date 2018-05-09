-- src/Reflex/Dom/SemanticUI/Table.hs:34:1-63: Splicing declarations
tableConfig_attached ::
  forall t_a1Zs7.
  Lens' (TableConfig t_a1Zs7) (Active t_a1Zs7 (Maybe VerticalAttached))
tableConfig_attached
  f_a1ZMh
  (TableConfig x1_a1ZMi x2_a1ZMj x3_a1ZMk x4_a1ZMl)
  = fmap
      (\ y1_a1ZMm -> TableConfig x1_a1ZMi x2_a1ZMj y1_a1ZMm x4_a1ZMl)
      (f_a1ZMh x3_a1ZMk)
{-# INLINE tableConfig_attached #-}
tableConfig_color ::
  forall t_a1Zs7.
  Lens' (TableConfig t_a1Zs7) (Active t_a1Zs7 (Maybe Color))
tableConfig_color
  f_a1ZMo
  (TableConfig x1_a1ZMp x2_a1ZMq x3_a1ZMr x4_a1ZMs)
  = fmap
      (\ y1_a1ZMu -> TableConfig x1_a1ZMp y1_a1ZMu x3_a1ZMr x4_a1ZMs)
      (f_a1ZMo x2_a1ZMq)
{-# INLINE tableConfig_color #-}
tableConfig_elConfig ::
  forall t_a1Zs7.
  Lens' (TableConfig t_a1Zs7) (ActiveElConfig t_a1Zs7)
tableConfig_elConfig
  f_a1ZMv
  (TableConfig x1_a1ZMw x2_a1ZMx x3_a1ZMy x4_a1ZMz)
  = fmap
      (\ y1_a1ZMA -> TableConfig x1_a1ZMw x2_a1ZMx x3_a1ZMy y1_a1ZMA)
      (f_a1ZMv x4_a1ZMz)
{-# INLINE tableConfig_elConfig #-}
tableConfig_type ::
  forall t_a1Zs7.
  Lens' (TableConfig t_a1Zs7) (Active t_a1Zs7 (Maybe TableType))
tableConfig_type
  f_a1ZMC
  (TableConfig x1_a1ZMD x2_a1ZME x3_a1ZMF x4_a1ZMG)
  = fmap
      (\ y1_a1ZMH -> TableConfig y1_a1ZMH x2_a1ZME x3_a1ZMF x4_a1ZMG)
      (f_a1ZMC x1_a1ZMD)
{-# INLINE tableConfig_type #-}
