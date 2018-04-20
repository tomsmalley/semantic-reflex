-- src/Reflex/Dom/SemanticUI/Table.hs:30:1-63: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''TableConfig
--   ======>
tableConfig_attached ::
  forall t_a4fAz.
  Control.Lens.Type.Lens' (TableConfig t_a4fAz) (Active t_a4fAz (Maybe VerticalAttached))
tableConfig_attached
  f_a4fGq
  (TableConfig x1_a4fGr x2_a4fGs x3_a4fGt x4_a4fGu)
  = fmap
      (\ y1_a4fGv -> TableConfig x1_a4fGr x2_a4fGs y1_a4fGv x4_a4fGu)
      (f_a4fGq x3_a4fGt)
{-# INLINE tableConfig_attached #-}
tableConfig_color ::
  forall t_a4fAz.
  Control.Lens.Type.Lens' (TableConfig t_a4fAz) (Active t_a4fAz (Maybe Color))
tableConfig_color
  f_a4fGw
  (TableConfig x1_a4fGx x2_a4fGy x3_a4fGz x4_a4fGA)
  = fmap
      (\ y1_a4fGB -> TableConfig x1_a4fGx y1_a4fGB x3_a4fGz x4_a4fGA)
      (f_a4fGw x2_a4fGy)
{-# INLINE tableConfig_color #-}
tableConfig_elConfig ::
  forall t_a4fAz.
  Control.Lens.Type.Lens' (TableConfig t_a4fAz) (ActiveElConfig t_a4fAz)
tableConfig_elConfig
  f_a4fGC
  (TableConfig x1_a4fGD x2_a4fGE x3_a4fGF x4_a4fGG)
  = fmap
      (\ y1_a4fGH -> TableConfig x1_a4fGD x2_a4fGE x3_a4fGF y1_a4fGH)
      (f_a4fGC x4_a4fGG)
{-# INLINE tableConfig_elConfig #-}
tableConfig_type ::
  forall t_a4fAz.
  Control.Lens.Type.Lens' (TableConfig t_a4fAz) (Active t_a4fAz (Maybe TableType))
tableConfig_type
  f_a4fGI
  (TableConfig x1_a4fGJ x2_a4fGK x3_a4fGL x4_a4fGM)
  = fmap
      (\ y1_a4fGN -> TableConfig y1_a4fGN x2_a4fGK x3_a4fGL x4_a4fGM)
      (f_a4fGI x1_a4fGJ)
{-# INLINE tableConfig_type #-}

