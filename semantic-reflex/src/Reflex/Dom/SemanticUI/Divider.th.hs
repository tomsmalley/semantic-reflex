-- src/Reflex/Dom/SemanticUI/Divider.hs:40:1-64: Splicing declarations
dividerConfig_clearing ::
  forall t_a2sRZ. Lens' (DividerConfig t_a2sRZ) (Active t_a2sRZ Bool)
dividerConfig_clearing
  f_a2sYx
  (DividerConfig x1_a2sYy
                 x2_a2sYz
                 x3_a2sYA
                 x4_a2sYB
                 x5_a2sYC
                 x6_a2sYD)
  = fmap
      (\ y1_a2sYE
         -> DividerConfig
              x1_a2sYy x2_a2sYz x3_a2sYA x4_a2sYB y1_a2sYE x6_a2sYD)
      (f_a2sYx x5_a2sYC)
{-# INLINE dividerConfig_clearing #-}
dividerConfig_elConfig ::
  forall t_a2sRZ.
  Lens' (DividerConfig t_a2sRZ) (ActiveElConfig t_a2sRZ)
dividerConfig_elConfig
  f_a2sYF
  (DividerConfig x1_a2sYG
                 x2_a2sYH
                 x3_a2sYI
                 x4_a2sYJ
                 x5_a2sYK
                 x6_a2sYL)
  = fmap
      (\ y1_a2sYM
         -> DividerConfig
              x1_a2sYG x2_a2sYH x3_a2sYI x4_a2sYJ x5_a2sYK y1_a2sYM)
      (f_a2sYF x6_a2sYL)
{-# INLINE dividerConfig_elConfig #-}
dividerConfig_fitted ::
  forall t_a2sRZ. Lens' (DividerConfig t_a2sRZ) (Active t_a2sRZ Bool)
dividerConfig_fitted
  f_a2sYN
  (DividerConfig x1_a2sYO
                 x2_a2sYP
                 x3_a2sYQ
                 x4_a2sYR
                 x5_a2sYS
                 x6_a2sYT)
  = fmap
      (\ y1_a2sYU
         -> DividerConfig
              x1_a2sYO y1_a2sYU x3_a2sYQ x4_a2sYR x5_a2sYS x6_a2sYT)
      (f_a2sYN x2_a2sYP)
{-# INLINE dividerConfig_fitted #-}
dividerConfig_hidden ::
  forall t_a2sRZ. Lens' (DividerConfig t_a2sRZ) (Active t_a2sRZ Bool)
dividerConfig_hidden
  f_a2sYV
  (DividerConfig x1_a2sYW
                 x2_a2sYX
                 x3_a2sYY
                 x4_a2sYZ
                 x5_a2sZ0
                 x6_a2sZ1)
  = fmap
      (\ y1_a2sZ2
         -> DividerConfig
              x1_a2sYW x2_a2sYX y1_a2sZ2 x4_a2sYZ x5_a2sZ0 x6_a2sZ1)
      (f_a2sYV x3_a2sYY)
{-# INLINE dividerConfig_hidden #-}
dividerConfig_inverted ::
  forall t_a2sRZ. Lens' (DividerConfig t_a2sRZ) (Active t_a2sRZ Bool)
dividerConfig_inverted
  f_a2sZ3
  (DividerConfig x1_a2sZ4
                 x2_a2sZ5
                 x3_a2sZ6
                 x4_a2sZ7
                 x5_a2sZ8
                 x6_a2sZa)
  = fmap
      (\ y1_a2sZb
         -> DividerConfig
              y1_a2sZb x2_a2sZ5 x3_a2sZ6 x4_a2sZ7 x5_a2sZ8 x6_a2sZa)
      (f_a2sZ3 x1_a2sZ4)
{-# INLINE dividerConfig_inverted #-}
dividerConfig_section ::
  forall t_a2sRZ. Lens' (DividerConfig t_a2sRZ) (Active t_a2sRZ Bool)
dividerConfig_section
  f_a2sZc
  (DividerConfig x1_a2sZd
                 x2_a2sZe
                 x3_a2sZf
                 x4_a2sZg
                 x5_a2sZh
                 x6_a2sZi)
  = fmap
      (\ y1_a2sZj
         -> DividerConfig
              x1_a2sZd x2_a2sZe x3_a2sZf y1_a2sZj x5_a2sZh x6_a2sZi)
      (f_a2sZc x4_a2sZg)
{-# INLINE dividerConfig_section #-}
