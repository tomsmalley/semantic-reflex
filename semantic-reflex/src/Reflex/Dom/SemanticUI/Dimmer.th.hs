-- src/Reflex/Dom/SemanticUI/Dimmer.hs:57:1-64: Splicing declarations
dimmerConfig_closeOnClick ::
  forall t_aT6l. Lens' (DimmerConfig t_aT6l) (Dynamic t_aT6l Bool)
dimmerConfig_closeOnClick
  f_aTc2
  (DimmerConfig x1_aTc3
                x2_aTc4
                x3_aTc5
                x4_aTc6
                x5_aTc7
                x6_aTc8
                x7_aTc9)
  = fmap
      (\ y1_aTca
         -> DimmerConfig
              x1_aTc3 x2_aTc4 x3_aTc5 x4_aTc6 x5_aTc7 y1_aTca x7_aTc9)
      (f_aTc2 x6_aTc8)
{-# INLINE dimmerConfig_closeOnClick #-}
dimmerConfig_dimmed ::
  forall t_aT6l.
  Lens' (DimmerConfig t_aT6l) (SetValue' t_aT6l Direction (Maybe Direction))
dimmerConfig_dimmed
  f_aTcb
  (DimmerConfig x1_aTcc
                x2_aTcd
                x3_aTce
                x4_aTcf
                x5_aTcg
                x6_aTch
                x7_aTci)
  = fmap
      (\ y1_aTcj
         -> DimmerConfig
              x1_aTcc x2_aTcd y1_aTcj x4_aTcf x5_aTcg x6_aTch x7_aTci)
      (f_aTcb x3_aTce)
{-# INLINE dimmerConfig_dimmed #-}
dimmerConfig_duration ::
  forall t_aT6l.
  Lens' (DimmerConfig t_aT6l) (Dynamic t_aT6l NominalDiffTime)
dimmerConfig_duration
  f_aTck
  (DimmerConfig x1_aTcl
                x2_aTcm
                x3_aTcn
                x4_aTco
                x5_aTcp
                x6_aTcq
                x7_aTcr)
  = fmap
      (\ y1_aTcs
         -> DimmerConfig
              x1_aTcl x2_aTcm x3_aTcn x4_aTco y1_aTcs x6_aTcq x7_aTcr)
      (f_aTck x5_aTcp)
{-# INLINE dimmerConfig_duration #-}
dimmerConfig_elConfig ::
  forall t_aT6l. Lens' (DimmerConfig t_aT6l) (ActiveElConfig t_aT6l)
dimmerConfig_elConfig
  f_aTct
  (DimmerConfig x1_aTcu
                x2_aTcv
                x3_aTcw
                x4_aTcx
                x5_aTcy
                x6_aTcz
                x7_aTcA)
  = fmap
      (\ y1_aTcB
         -> DimmerConfig
              x1_aTcu x2_aTcv x3_aTcw x4_aTcx x5_aTcy x6_aTcz y1_aTcB)
      (f_aTct x7_aTcA)
{-# INLINE dimmerConfig_elConfig #-}
dimmerConfig_inverted ::
  forall t_aT6l. Lens' (DimmerConfig t_aT6l) (Dynamic t_aT6l Bool)
dimmerConfig_inverted
  f_aTcC
  (DimmerConfig x1_aTcD
                x2_aTcE
                x3_aTcF
                x4_aTcG
                x5_aTcH
                x6_aTcI
                x7_aTcJ)
  = fmap
      (\ y1_aTcK
         -> DimmerConfig
              y1_aTcK x2_aTcE x3_aTcF x4_aTcG x5_aTcH x6_aTcI x7_aTcJ)
      (f_aTcC x1_aTcD)
{-# INLINE dimmerConfig_inverted #-}
dimmerConfig_page ::
  forall t_aT6l. Lens' (DimmerConfig t_aT6l) Bool
dimmerConfig_page
  f_aTcL
  (DimmerConfig x1_aTcM
                x2_aTcN
                x3_aTcO
                x4_aTcP
                x5_aTcQ
                x6_aTcR
                x7_aTcS)
  = fmap
      (\ y1_aTcT
         -> DimmerConfig
              x1_aTcM y1_aTcT x3_aTcO x4_aTcP x5_aTcQ x6_aTcR x7_aTcS)
      (f_aTcL x2_aTcN)
{-# INLINE dimmerConfig_page #-}
dimmerConfig_transitionType ::
  forall t_aT6l.
  Lens' (DimmerConfig t_aT6l) (Dynamic t_aT6l TransitionType)
dimmerConfig_transitionType
  f_aTcU
  (DimmerConfig x1_aTcV
                x2_aTcW
                x3_aTcX
                x4_aTcY
                x5_aTcZ
                x6_aTd0
                x7_aTd1)
  = fmap
      (\ y1_aTd2
         -> DimmerConfig
              x1_aTcV x2_aTcW x3_aTcX y1_aTd2 x5_aTcZ x6_aTd0 x7_aTd1)
      (f_aTcU x4_aTcY)
{-# INLINE dimmerConfig_transitionType #-}
