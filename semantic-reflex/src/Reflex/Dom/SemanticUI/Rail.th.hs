-- src/Reflex/Dom/SemanticUI/Rail.hs:57:1-62: Splicing declarations
railConfig_attached ::
  forall t_a2ckb. Lens' (RailConfig t_a2ckb) (Active t_a2ckb Bool)
railConfig_attached
  f_a2cFD
  (RailConfig x1_a2cFE x2_a2cFF x3_a2cFG x4_a2cFH x5_a2cFI x6_a2cFJ)
  = fmap
      (\ y1_a2cFK
         -> RailConfig
              x1_a2cFE x2_a2cFF y1_a2cFK x4_a2cFH x5_a2cFI x6_a2cFJ)
      (f_a2cFD x3_a2cFG)
{-# INLINE railConfig_attached #-}
railConfig_close ::
  forall t_a2ckb.
  Lens' (RailConfig t_a2ckb) (Active t_a2ckb (Maybe RailClose))
railConfig_close
  f_a2cFM
  (RailConfig x1_a2cFN x2_a2cFO x3_a2cFP x4_a2cFQ x5_a2cFR x6_a2cFS)
  = fmap
      (\ y1_a2cFT
         -> RailConfig
              x1_a2cFN x2_a2cFO x3_a2cFP y1_a2cFT x5_a2cFR x6_a2cFS)
      (f_a2cFM x4_a2cFQ)
{-# INLINE railConfig_close #-}
railConfig_dividing ::
  forall t_a2ckb. Lens' (RailConfig t_a2ckb) (Active t_a2ckb Bool)
railConfig_dividing
  f_a2cFU
  (RailConfig x1_a2cFV x2_a2cFW x3_a2cFX x4_a2cFY x5_a2cFZ x6_a2cG0)
  = fmap
      (\ y1_a2cG1
         -> RailConfig
              y1_a2cG1 x2_a2cFW x3_a2cFX x4_a2cFY x5_a2cFZ x6_a2cG0)
      (f_a2cFU x1_a2cFV)
{-# INLINE railConfig_dividing #-}
railConfig_elConfig ::
  forall t_a2ckb. Lens' (RailConfig t_a2ckb) (ActiveElConfig t_a2ckb)
railConfig_elConfig
  f_a2cG2
  (RailConfig x1_a2cG3 x2_a2cG4 x3_a2cG5 x4_a2cG6 x5_a2cG7 x6_a2cG8)
  = fmap
      (\ y1_a2cG9
         -> RailConfig
              x1_a2cG3 x2_a2cG4 x3_a2cG5 x4_a2cG6 x5_a2cG7 y1_a2cG9)
      (f_a2cG2 x6_a2cG8)
{-# INLINE railConfig_elConfig #-}
railConfig_internal ::
  forall t_a2ckb. Lens' (RailConfig t_a2ckb) (Active t_a2ckb Bool)
railConfig_internal
  f_a2cGa
  (RailConfig x1_a2cGb x2_a2cGc x3_a2cGd x4_a2cGe x5_a2cGf x6_a2cGg)
  = fmap
      (\ y1_a2cGh
         -> RailConfig
              x1_a2cGb y1_a2cGh x3_a2cGd x4_a2cGe x5_a2cGf x6_a2cGg)
      (f_a2cGa x2_a2cGc)
{-# INLINE railConfig_internal #-}
railConfig_size ::
  forall t_a2ckb.
  Lens' (RailConfig t_a2ckb) (Active t_a2ckb (Maybe Size))
railConfig_size
  f_a2cGj
  (RailConfig x1_a2cGk x2_a2cGl x3_a2cGm x4_a2cGn x5_a2cGo x6_a2cGp)
  = fmap
      (\ y1_a2cGq
         -> RailConfig
              x1_a2cGk x2_a2cGl x3_a2cGm x4_a2cGn y1_a2cGq x6_a2cGp)
      (f_a2cGj x5_a2cGo)
{-# INLINE railConfig_size #-}
