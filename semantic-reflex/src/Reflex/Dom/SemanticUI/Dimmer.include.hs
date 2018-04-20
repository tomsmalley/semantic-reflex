-- src/Reflex/Dom/SemanticUI/Dimmer.hs:52:1-64: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''DimmerConfig
--   ======>
dimmerConfig_closeOnClick ::
  forall t_a2IQd.
  Control.Lens.Type.Lens' (DimmerConfig t_a2IQd) (Dynamic t_a2IQd Bool)
dimmerConfig_closeOnClick
  f_a2IR6
  (DimmerConfig x1_a2IR7
                x2_a2IR8
                x3_a2IR9
                x4_a2IRa
                x5_a2IRb
                x6_a2IRc
                x7_a2IRd)
  = fmap
      (\ y1_a2IRe
         -> DimmerConfig
              x1_a2IR7 x2_a2IR8 x3_a2IR9 x4_a2IRa x5_a2IRb y1_a2IRe x7_a2IRd)
      (f_a2IR6 x6_a2IRc)
{-# INLINE dimmerConfig_closeOnClick #-}
dimmerConfig_dimmed ::
  forall t_a2IQd.
  Control.Lens.Type.Lens' (DimmerConfig t_a2IQd) (SetValue' t_a2IQd Direction (Maybe Direction))
dimmerConfig_dimmed
  f_a2IRf
  (DimmerConfig x1_a2IRg
                x2_a2IRh
                x3_a2IRi
                x4_a2IRj
                x5_a2IRk
                x6_a2IRl
                x7_a2IRm)
  = fmap
      (\ y1_a2IRn
         -> DimmerConfig
              x1_a2IRg x2_a2IRh y1_a2IRn x4_a2IRj x5_a2IRk x6_a2IRl x7_a2IRm)
      (f_a2IRf x3_a2IRi)
{-# INLINE dimmerConfig_dimmed #-}
dimmerConfig_duration ::
  forall t_a2IQd.
  Control.Lens.Type.Lens' (DimmerConfig t_a2IQd) (Dynamic t_a2IQd NominalDiffTime)
dimmerConfig_duration
  f_a2IRo
  (DimmerConfig x1_a2IRp
                x2_a2IRq
                x3_a2IRr
                x4_a2IRs
                x5_a2IRt
                x6_a2IRu
                x7_a2IRv)
  = fmap
      (\ y1_a2IRw
         -> DimmerConfig
              x1_a2IRp x2_a2IRq x3_a2IRr x4_a2IRs y1_a2IRw x6_a2IRu x7_a2IRv)
      (f_a2IRo x5_a2IRt)
{-# INLINE dimmerConfig_duration #-}
dimmerConfig_elConfig ::
  forall t_a2IQd.
  Control.Lens.Type.Lens' (DimmerConfig t_a2IQd) (ActiveElConfig t_a2IQd)
dimmerConfig_elConfig
  f_a2IRx
  (DimmerConfig x1_a2IRy
                x2_a2IRz
                x3_a2IRA
                x4_a2IRB
                x5_a2IRC
                x6_a2IRD
                x7_a2IRE)
  = fmap
      (\ y1_a2IRF
         -> DimmerConfig
              x1_a2IRy x2_a2IRz x3_a2IRA x4_a2IRB x5_a2IRC x6_a2IRD y1_a2IRF)
      (f_a2IRx x7_a2IRE)
{-# INLINE dimmerConfig_elConfig #-}
dimmerConfig_inverted ::
  forall t_a2IQd.
  Control.Lens.Type.Lens' (DimmerConfig t_a2IQd) (Dynamic t_a2IQd Bool)
dimmerConfig_inverted
  f_a2IRG
  (DimmerConfig x1_a2IRH
                x2_a2IRI
                x3_a2IRJ
                x4_a2IRK
                x5_a2IRL
                x6_a2IRM
                x7_a2IRN)
  = fmap
      (\ y1_a2IRO
         -> DimmerConfig
              y1_a2IRO x2_a2IRI x3_a2IRJ x4_a2IRK x5_a2IRL x6_a2IRM x7_a2IRN)
      (f_a2IRG x1_a2IRH)
{-# INLINE dimmerConfig_inverted #-}
dimmerConfig_page ::
  forall t_a2IQd. Control.Lens.Type.Lens' (DimmerConfig t_a2IQd) Bool
dimmerConfig_page
  f_a2IRP
  (DimmerConfig x1_a2IRQ
                x2_a2IRR
                x3_a2IRS
                x4_a2IRT
                x5_a2IRU
                x6_a2IRV
                x7_a2IRW)
  = fmap
      (\ y1_a2IRX
         -> DimmerConfig
              x1_a2IRQ y1_a2IRX x3_a2IRS x4_a2IRT x5_a2IRU x6_a2IRV x7_a2IRW)
      (f_a2IRP x2_a2IRR)
{-# INLINE dimmerConfig_page #-}
dimmerConfig_transitionType ::
  forall t_a2IQd.
  Control.Lens.Type.Lens' (DimmerConfig t_a2IQd) (Dynamic t_a2IQd TransitionType)
dimmerConfig_transitionType
  f_a2IRY
  (DimmerConfig x1_a2IRZ
                x2_a2IS0
                x3_a2IS1
                x4_a2IS2
                x5_a2IS3
                x6_a2IS4
                x7_a2IS5)
  = fmap
      (\ y1_a2IS6
         -> DimmerConfig
              x1_a2IRZ x2_a2IS0 x3_a2IS1 y1_a2IS6 x5_a2IS3 x6_a2IS4 x7_a2IS5)
      (f_a2IRY x4_a2IS2)
{-# INLINE dimmerConfig_transitionType #-}

