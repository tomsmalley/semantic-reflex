-- src/Reflex/Dom/SemanticUI/Divider.hs:36:1-64: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) 'DividerConfig
--   ======>
dividerConfig_clearing ::
  forall t_a2Mre.
  Control.Lens.Type.Lens' (DividerConfig t_a2Mre) (Active t_a2Mre Bool)
dividerConfig_clearing
  f_a2Ms3
  (DividerConfig x1_a2Ms4
                 x2_a2Ms5
                 x3_a2Ms6
                 x4_a2Ms7
                 x5_a2Ms8
                 x6_a2Ms9)
  = fmap
      (\ y1_a2Msa
         -> DividerConfig
              x1_a2Ms4 x2_a2Ms5 x3_a2Ms6 x4_a2Ms7 y1_a2Msa x6_a2Ms9)
      (f_a2Ms3 x5_a2Ms8)
{-# INLINE dividerConfig_clearing #-}
dividerConfig_elConfig ::
  forall t_a2Mre.
  Control.Lens.Type.Lens' (DividerConfig t_a2Mre) (ActiveElConfig t_a2Mre)
dividerConfig_elConfig
  f_a2Msb
  (DividerConfig x1_a2Msc
                 x2_a2Msd
                 x3_a2Mse
                 x4_a2Msf
                 x5_a2Msg
                 x6_a2Msh)
  = fmap
      (\ y1_a2Msi
         -> DividerConfig
              x1_a2Msc x2_a2Msd x3_a2Mse x4_a2Msf x5_a2Msg y1_a2Msi)
      (f_a2Msb x6_a2Msh)
{-# INLINE dividerConfig_elConfig #-}
dividerConfig_fitted ::
  forall t_a2Mre.
  Control.Lens.Type.Lens' (DividerConfig t_a2Mre) (Active t_a2Mre Bool)
dividerConfig_fitted
  f_a2Msj
  (DividerConfig x1_a2Msk
                 x2_a2Msl
                 x3_a2Msm
                 x4_a2Msn
                 x5_a2Mso
                 x6_a2Msp)
  = fmap
      (\ y1_a2Msq
         -> DividerConfig
              x1_a2Msk y1_a2Msq x3_a2Msm x4_a2Msn x5_a2Mso x6_a2Msp)
      (f_a2Msj x2_a2Msl)
{-# INLINE dividerConfig_fitted #-}
dividerConfig_hidden ::
  forall t_a2Mre.
  Control.Lens.Type.Lens' (DividerConfig t_a2Mre) (Active t_a2Mre Bool)
dividerConfig_hidden
  f_a2Msr
  (DividerConfig x1_a2Mss
                 x2_a2Mst
                 x3_a2Msu
                 x4_a2Msv
                 x5_a2Msw
                 x6_a2Msx)
  = fmap
      (\ y1_a2Msy
         -> DividerConfig
              x1_a2Mss x2_a2Mst y1_a2Msy x4_a2Msv x5_a2Msw x6_a2Msx)
      (f_a2Msr x3_a2Msu)
{-# INLINE dividerConfig_hidden #-}
dividerConfig_inverted ::
  forall t_a2Mre.
  Control.Lens.Type.Lens' (DividerConfig t_a2Mre) (Active t_a2Mre Bool)
dividerConfig_inverted
  f_a2Msz
  (DividerConfig x1_a2MsA
                 x2_a2MsB
                 x3_a2MsC
                 x4_a2MsD
                 x5_a2MsE
                 x6_a2MsF)
  = fmap
      (\ y1_a2MsG
         -> DividerConfig
              y1_a2MsG x2_a2MsB x3_a2MsC x4_a2MsD x5_a2MsE x6_a2MsF)
      (f_a2Msz x1_a2MsA)
{-# INLINE dividerConfig_inverted #-}
dividerConfig_section ::
  forall t_a2Mre.
  Control.Lens.Type.Lens' (DividerConfig t_a2Mre) (Active t_a2Mre Bool)
dividerConfig_section
  f_a2MsH
  (DividerConfig x1_a2MsI
                 x2_a2MsJ
                 x3_a2MsK
                 x4_a2MsL
                 x5_a2MsM
                 x6_a2MsN)
  = fmap
      (\ y1_a2MsO
         -> DividerConfig
              x1_a2MsI x2_a2MsJ x3_a2MsK y1_a2MsO x5_a2MsM x6_a2MsN)
      (f_a2MsH x4_a2MsL)
{-# INLINE dividerConfig_section #-}

