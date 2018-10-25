-- src/Reflex/Dom/SemanticUI/Divider.hs:40:1-64: Splicing declarations
dividerConfig_clearing ::
  forall t_a1Efx. Lens' (DividerConfig t_a1Efx) (Active t_a1Efx Bool)
dividerConfig_clearing
  f_a1EkS
  (DividerConfig x1_a1EkT
                 x2_a1EkU
                 x3_a1EkV
                 x4_a1EkW
                 x5_a1EkX
                 x6_a1EkY)
  = fmap
      (\ y1_a1EkZ
         -> DividerConfig
              x1_a1EkT x2_a1EkU x3_a1EkV x4_a1EkW y1_a1EkZ x6_a1EkY)
      (f_a1EkS x5_a1EkX)
{-# INLINE dividerConfig_clearing #-}
dividerConfig_elConfig ::
  forall t_a1Efx.
  Lens' (DividerConfig t_a1Efx) (ActiveElConfig t_a1Efx)
dividerConfig_elConfig
  f_a1El0
  (DividerConfig x1_a1El1
                 x2_a1El2
                 x3_a1El3
                 x4_a1El4
                 x5_a1El5
                 x6_a1El6)
  = fmap
      (\ y1_a1El7
         -> DividerConfig
              x1_a1El1 x2_a1El2 x3_a1El3 x4_a1El4 x5_a1El5 y1_a1El7)
      (f_a1El0 x6_a1El6)
{-# INLINE dividerConfig_elConfig #-}
dividerConfig_fitted ::
  forall t_a1Efx. Lens' (DividerConfig t_a1Efx) (Active t_a1Efx Bool)
dividerConfig_fitted
  f_a1El8
  (DividerConfig x1_a1El9
                 x2_a1Ela
                 x3_a1Elb
                 x4_a1Elc
                 x5_a1Eld
                 x6_a1Ele)
  = fmap
      (\ y1_a1Elf
         -> DividerConfig
              x1_a1El9 y1_a1Elf x3_a1Elb x4_a1Elc x5_a1Eld x6_a1Ele)
      (f_a1El8 x2_a1Ela)
{-# INLINE dividerConfig_fitted #-}
dividerConfig_hidden ::
  forall t_a1Efx. Lens' (DividerConfig t_a1Efx) (Active t_a1Efx Bool)
dividerConfig_hidden
  f_a1Elg
  (DividerConfig x1_a1Elh
                 x2_a1Eli
                 x3_a1Elj
                 x4_a1Ell
                 x5_a1Elm
                 x6_a1Eln)
  = fmap
      (\ y1_a1Elo
         -> DividerConfig
              x1_a1Elh x2_a1Eli y1_a1Elo x4_a1Ell x5_a1Elm x6_a1Eln)
      (f_a1Elg x3_a1Elj)
{-# INLINE dividerConfig_hidden #-}
dividerConfig_inverted ::
  forall t_a1Efx. Lens' (DividerConfig t_a1Efx) (Active t_a1Efx Bool)
dividerConfig_inverted
  f_a1Elq
  (DividerConfig x1_a1Elr
                 x2_a1Elt
                 x3_a1Elu
                 x4_a1Elv
                 x5_a1Elw
                 x6_a1Elx)
  = fmap
      (\ y1_a1Ely
         -> DividerConfig
              y1_a1Ely x2_a1Elt x3_a1Elu x4_a1Elv x5_a1Elw x6_a1Elx)
      (f_a1Elq x1_a1Elr)
{-# INLINE dividerConfig_inverted #-}
dividerConfig_section ::
  forall t_a1Efx. Lens' (DividerConfig t_a1Efx) (Active t_a1Efx Bool)
dividerConfig_section
  f_a1ElB
  (DividerConfig x1_a1ElC
                 x2_a1ElD
                 x3_a1ElE
                 x4_a1ElF
                 x5_a1ElG
                 x6_a1ElH)
  = fmap
      (\ y1_a1ElI
         -> DividerConfig
              x1_a1ElC x2_a1ElD x3_a1ElE y1_a1ElI x5_a1ElG x6_a1ElH)
      (f_a1ElB x4_a1ElF)
{-# INLINE dividerConfig_section #-}
