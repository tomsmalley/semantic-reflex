-- src/Reflex/Dom/SemanticUI/Rail.hs:53:1-62: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''RailConfig
--   ======>
railConfig_attached ::
  forall t_a46XH.
  Control.Lens.Type.Lens' (RailConfig t_a46XH) (Active t_a46XH Bool)
railConfig_attached
  f_a478o
  (RailConfig x1_a478p x2_a478q x3_a478r x4_a478s x5_a478t x6_a478u)
  = fmap
      (\ y1_a478v
         -> RailConfig
              x1_a478p x2_a478q y1_a478v x4_a478s x5_a478t x6_a478u)
      (f_a478o x3_a478r)
{-# INLINE railConfig_attached #-}
railConfig_close ::
  forall t_a46XH.
  Control.Lens.Type.Lens' (RailConfig t_a46XH) (Active t_a46XH (Maybe RailClose))
railConfig_close
  f_a478w
  (RailConfig x1_a478x x2_a478y x3_a478z x4_a478A x5_a478B x6_a478C)
  = fmap
      (\ y1_a478D
         -> RailConfig
              x1_a478x x2_a478y x3_a478z y1_a478D x5_a478B x6_a478C)
      (f_a478w x4_a478A)
{-# INLINE railConfig_close #-}
railConfig_dividing ::
  forall t_a46XH.
  Control.Lens.Type.Lens' (RailConfig t_a46XH) (Active t_a46XH Bool)
railConfig_dividing
  f_a478E
  (RailConfig x1_a478F x2_a478G x3_a478H x4_a478I x5_a478J x6_a478K)
  = fmap
      (\ y1_a478L
         -> RailConfig
              y1_a478L x2_a478G x3_a478H x4_a478I x5_a478J x6_a478K)
      (f_a478E x1_a478F)
{-# INLINE railConfig_dividing #-}
railConfig_elConfig ::
  forall t_a46XH.
  Control.Lens.Type.Lens' (RailConfig t_a46XH) (ActiveElConfig t_a46XH)
railConfig_elConfig
  f_a478M
  (RailConfig x1_a478N x2_a478O x3_a478P x4_a478Q x5_a478R x6_a478S)
  = fmap
      (\ y1_a478T
         -> RailConfig
              x1_a478N x2_a478O x3_a478P x4_a478Q x5_a478R y1_a478T)
      (f_a478M x6_a478S)
{-# INLINE railConfig_elConfig #-}
railConfig_internal ::
  forall t_a46XH.
  Control.Lens.Type.Lens' (RailConfig t_a46XH) (Active t_a46XH Bool)
railConfig_internal
  f_a478U
  (RailConfig x1_a478V x2_a478W x3_a478X x4_a478Y x5_a478Z x6_a4790)
  = fmap
      (\ y1_a4791
         -> RailConfig
              x1_a478V y1_a4791 x3_a478X x4_a478Y x5_a478Z x6_a4790)
      (f_a478U x2_a478W)
{-# INLINE railConfig_internal #-}
railConfig_size ::
  forall t_a46XH.
  Control.Lens.Type.Lens' (RailConfig t_a46XH) (Active t_a46XH (Maybe Size))
railConfig_size
  f_a4792
  (RailConfig x1_a4793 x2_a4794 x3_a4795 x4_a4796 x5_a4797 x6_a4798)
  = fmap
      (\ y1_a4799
         -> RailConfig
              x1_a4793 x2_a4794 x3_a4795 x4_a4796 y1_a4799 x6_a4798)
      (f_a4792 x5_a4797)
{-# INLINE railConfig_size #-}

