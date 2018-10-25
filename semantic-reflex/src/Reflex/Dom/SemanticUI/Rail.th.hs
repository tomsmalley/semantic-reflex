-- src/Reflex/Dom/SemanticUI/Rail.hs:57:1-62: Splicing declarations
railConfig_attached ::
  forall t_a2mKA. Lens' (RailConfig t_a2mKA) (Active t_a2mKA Bool)
railConfig_attached
  f_a2nu2
  (RailConfig x1_a2nu3 x2_a2nu4 x3_a2nu6 x4_a2nu7 x5_a2nu9 x6_a2nua)
  = fmap
      (\ y1_a2nuc
         -> RailConfig
              x1_a2nu3 x2_a2nu4 y1_a2nuc x4_a2nu7 x5_a2nu9 x6_a2nua)
      (f_a2nu2 x3_a2nu6)
{-# INLINE railConfig_attached #-}
railConfig_close ::
  forall t_a2mKA.
  Lens' (RailConfig t_a2mKA) (Active t_a2mKA (Maybe RailClose))
railConfig_close
  f_a2nul
  (RailConfig x1_a2nuo x2_a2nuq x3_a2nur x4_a2nus x5_a2nut x6_a2nuu)
  = fmap
      (\ y1_a2nuv
         -> RailConfig
              x1_a2nuo x2_a2nuq x3_a2nur y1_a2nuv x5_a2nut x6_a2nuu)
      (f_a2nul x4_a2nus)
{-# INLINE railConfig_close #-}
railConfig_dividing ::
  forall t_a2mKA. Lens' (RailConfig t_a2mKA) (Active t_a2mKA Bool)
railConfig_dividing
  f_a2nuy
  (RailConfig x1_a2nuA x2_a2nuB x3_a2nuC x4_a2nuD x5_a2nuE x6_a2nuF)
  = fmap
      (\ y1_a2nuG
         -> RailConfig
              y1_a2nuG x2_a2nuB x3_a2nuC x4_a2nuD x5_a2nuE x6_a2nuF)
      (f_a2nuy x1_a2nuA)
{-# INLINE railConfig_dividing #-}
railConfig_elConfig ::
  forall t_a2mKA. Lens' (RailConfig t_a2mKA) (ActiveElConfig t_a2mKA)
railConfig_elConfig
  f_a2nuN
  (RailConfig x1_a2nuP x2_a2nuR x3_a2nuS x4_a2nuU x5_a2nuV x6_a2nuW)
  = fmap
      (\ y1_a2nuX
         -> RailConfig
              x1_a2nuP x2_a2nuR x3_a2nuS x4_a2nuU x5_a2nuV y1_a2nuX)
      (f_a2nuN x6_a2nuW)
{-# INLINE railConfig_elConfig #-}
railConfig_internal ::
  forall t_a2mKA. Lens' (RailConfig t_a2mKA) (Active t_a2mKA Bool)
railConfig_internal
  f_a2nv1
  (RailConfig x1_a2nv2 x2_a2nv3 x3_a2nv4 x4_a2nv6 x5_a2nv7 x6_a2nv8)
  = fmap
      (\ y1_a2nv9
         -> RailConfig
              x1_a2nv2 y1_a2nv9 x3_a2nv4 x4_a2nv6 x5_a2nv7 x6_a2nv8)
      (f_a2nv1 x2_a2nv3)
{-# INLINE railConfig_internal #-}
railConfig_size ::
  forall t_a2mKA.
  Lens' (RailConfig t_a2mKA) (Active t_a2mKA (Maybe Size))
railConfig_size
  f_a2nvc
  (RailConfig x1_a2nvd x2_a2nve x3_a2nvf x4_a2nvg x5_a2nvh x6_a2nvj)
  = fmap
      (\ y1_a2nvk
         -> RailConfig
              x1_a2nvd x2_a2nve x3_a2nvf x4_a2nvg y1_a2nvk x6_a2nvj)
      (f_a2nvc x5_a2nvh)
{-# INLINE railConfig_size #-}
