-- src/Reflex/Dom/SemanticUI/Dimmer.hs:57:1-64: Splicing declarations
dimmerConfig_closeOnClick ::
  forall t_a18NW. Lens' (DimmerConfig t_a18NW) (Dynamic t_a18NW Bool)
dimmerConfig_closeOnClick
  f_a18OW
  (DimmerConfig x1_a18OX
                x2_a18OY
                x3_a18OZ
                x4_a18P0
                x5_a18P1
                x6_a18P2
                x7_a18P3)
  = fmap
      (\ y1_a18P4
         -> DimmerConfig
              x1_a18OX x2_a18OY x3_a18OZ x4_a18P0 x5_a18P1 y1_a18P4 x7_a18P3)
      (f_a18OW x6_a18P2)
{-# INLINE dimmerConfig_closeOnClick #-}
dimmerConfig_dimmed ::
  forall t_a18NW.
  Lens' (DimmerConfig t_a18NW) (SetValue' t_a18NW Direction (Maybe Direction))
dimmerConfig_dimmed
  f_a18P5
  (DimmerConfig x1_a18P6
                x2_a18P7
                x3_a18P8
                x4_a18P9
                x5_a18Pa
                x6_a18Pb
                x7_a18Pc)
  = fmap
      (\ y1_a18Pd
         -> DimmerConfig
              x1_a18P6 x2_a18P7 y1_a18Pd x4_a18P9 x5_a18Pa x6_a18Pb x7_a18Pc)
      (f_a18P5 x3_a18P8)
{-# INLINE dimmerConfig_dimmed #-}
dimmerConfig_duration ::
  forall t_a18NW.
  Lens' (DimmerConfig t_a18NW) (Dynamic t_a18NW NominalDiffTime)
dimmerConfig_duration
  f_a18Pe
  (DimmerConfig x1_a18Pf
                x2_a18Pg
                x3_a18Ph
                x4_a18Pi
                x5_a18Pj
                x6_a18Pk
                x7_a18Pl)
  = fmap
      (\ y1_a18Pm
         -> DimmerConfig
              x1_a18Pf x2_a18Pg x3_a18Ph x4_a18Pi y1_a18Pm x6_a18Pk x7_a18Pl)
      (f_a18Pe x5_a18Pj)
{-# INLINE dimmerConfig_duration #-}
dimmerConfig_elConfig ::
  forall t_a18NW.
  Lens' (DimmerConfig t_a18NW) (ActiveElConfig t_a18NW)
dimmerConfig_elConfig
  f_a18Pn
  (DimmerConfig x1_a18Po
                x2_a18Pp
                x3_a18Pq
                x4_a18Pr
                x5_a18Ps
                x6_a18Pt
                x7_a18Pu)
  = fmap
      (\ y1_a18Pv
         -> DimmerConfig
              x1_a18Po x2_a18Pp x3_a18Pq x4_a18Pr x5_a18Ps x6_a18Pt y1_a18Pv)
      (f_a18Pn x7_a18Pu)
{-# INLINE dimmerConfig_elConfig #-}
dimmerConfig_inverted ::
  forall t_a18NW. Lens' (DimmerConfig t_a18NW) (Dynamic t_a18NW Bool)
dimmerConfig_inverted
  f_a18Pw
  (DimmerConfig x1_a18Px
                x2_a18Py
                x3_a18Pz
                x4_a18PA
                x5_a18PB
                x6_a18PC
                x7_a18PD)
  = fmap
      (\ y1_a18PE
         -> DimmerConfig
              y1_a18PE x2_a18Py x3_a18Pz x4_a18PA x5_a18PB x6_a18PC x7_a18PD)
      (f_a18Pw x1_a18Px)
{-# INLINE dimmerConfig_inverted #-}
dimmerConfig_page ::
  forall t_a18NW. Lens' (DimmerConfig t_a18NW) Bool
dimmerConfig_page
  f_a18PF
  (DimmerConfig x1_a18PG
                x2_a18PH
                x3_a18PI
                x4_a18PJ
                x5_a18PK
                x6_a18PL
                x7_a18PM)
  = fmap
      (\ y1_a18PN
         -> DimmerConfig
              x1_a18PG y1_a18PN x3_a18PI x4_a18PJ x5_a18PK x6_a18PL x7_a18PM)
      (f_a18PF x2_a18PH)
{-# INLINE dimmerConfig_page #-}
dimmerConfig_transitionType ::
  forall t_a18NW.
  Lens' (DimmerConfig t_a18NW) (Dynamic t_a18NW TransitionType)
dimmerConfig_transitionType
  f_a18PO
  (DimmerConfig x1_a18PP
                x2_a18PQ
                x3_a18PR
                x4_a18PS
                x5_a18PT
                x6_a18PU
                x7_a18PV)
  = fmap
      (\ y1_a18PW
         -> DimmerConfig
              x1_a18PP x2_a18PQ x3_a18PR y1_a18PW x5_a18PT x6_a18PU x7_a18PV)
      (f_a18PO x4_a18PS)
{-# INLINE dimmerConfig_transitionType #-}
