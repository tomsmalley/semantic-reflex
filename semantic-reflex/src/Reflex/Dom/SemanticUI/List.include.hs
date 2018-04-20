-- src/Reflex/Dom/SemanticUI/List.hs:53:1-62: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''ListConfig
--   ======>
listConfig_aligned ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (Active t_a3QlD (Maybe ListAligned))
listConfig_aligned
  f_a3QBW
  (ListConfig x1_a3QBX
              x2_a3QBY
              x3_a3QBZ
              x4_a3QC0
              x5_a3QC1
              x6_a3QC2
              x7_a3QC3
              x8_a3QC4
              x9_a3QC5
              x10_a3QC6
              x11_a3QC7
              x12_a3QC8
              x13_a3QC9)
  = fmap
      (\ y1_a3QCa
         -> ListConfig
              x1_a3QBX
              x2_a3QBY
              x3_a3QBZ
              x4_a3QC0
              x5_a3QC1
              x6_a3QC2
              x7_a3QC3
              x8_a3QC4
              x9_a3QC5
              x10_a3QC6
              x11_a3QC7
              y1_a3QCa
              x13_a3QC9)
      (f_a3QBW x12_a3QC8)
{-# INLINE listConfig_aligned #-}
listConfig_animated ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (Active t_a3QlD Bool)
listConfig_animated
  f_a3QCb
  (ListConfig x1_a3QCc
              x2_a3QCd
              x3_a3QCe
              x4_a3QCf
              x5_a3QCg
              x6_a3QCh
              x7_a3QCi
              x8_a3QCj
              x9_a3QCk
              x10_a3QCl
              x11_a3QCm
              x12_a3QCn
              x13_a3QCo)
  = fmap
      (\ y1_a3QCp
         -> ListConfig
              x1_a3QCc
              x2_a3QCd
              x3_a3QCe
              x4_a3QCf
              x5_a3QCg
              y1_a3QCp
              x7_a3QCi
              x8_a3QCj
              x9_a3QCk
              x10_a3QCl
              x11_a3QCm
              x12_a3QCn
              x13_a3QCo)
      (f_a3QCb x6_a3QCh)
{-# INLINE listConfig_animated #-}
listConfig_celled ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (Active t_a3QlD Bool)
listConfig_celled
  f_a3QCq
  (ListConfig x1_a3QCr
              x2_a3QCs
              x3_a3QCt
              x4_a3QCu
              x5_a3QCv
              x6_a3QCw
              x7_a3QCx
              x8_a3QCy
              x9_a3QCz
              x10_a3QCA
              x11_a3QCB
              x12_a3QCC
              x13_a3QCD)
  = fmap
      (\ y1_a3QCE
         -> ListConfig
              x1_a3QCr
              x2_a3QCs
              x3_a3QCt
              x4_a3QCu
              x5_a3QCv
              x6_a3QCw
              x7_a3QCx
              x8_a3QCy
              y1_a3QCE
              x10_a3QCA
              x11_a3QCB
              x12_a3QCC
              x13_a3QCD)
      (f_a3QCq x9_a3QCz)
{-# INLINE listConfig_celled #-}
listConfig_divided ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (Active t_a3QlD Bool)
listConfig_divided
  f_a3QCF
  (ListConfig x1_a3QCG
              x2_a3QCH
              x3_a3QCI
              x4_a3QCJ
              x5_a3QCK
              x6_a3QCL
              x7_a3QCM
              x8_a3QCN
              x9_a3QCO
              x10_a3QCP
              x11_a3QCQ
              x12_a3QCR
              x13_a3QCS)
  = fmap
      (\ y1_a3QCT
         -> ListConfig
              x1_a3QCG
              x2_a3QCH
              x3_a3QCI
              x4_a3QCJ
              x5_a3QCK
              x6_a3QCL
              x7_a3QCM
              y1_a3QCT
              x9_a3QCO
              x10_a3QCP
              x11_a3QCQ
              x12_a3QCR
              x13_a3QCS)
      (f_a3QCF x8_a3QCN)
{-# INLINE listConfig_divided #-}
listConfig_elConfig ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (ActiveElConfig t_a3QlD)
listConfig_elConfig
  f_a3QCU
  (ListConfig x1_a3QCV
              x2_a3QCW
              x3_a3QCX
              x4_a3QCY
              x5_a3QCZ
              x6_a3QD0
              x7_a3QD1
              x8_a3QD2
              x9_a3QD3
              x10_a3QD4
              x11_a3QD5
              x12_a3QD6
              x13_a3QD7)
  = fmap
      (\ y1_a3QD8
         -> ListConfig
              x1_a3QCV
              x2_a3QCW
              x3_a3QCX
              x4_a3QCY
              x5_a3QCZ
              x6_a3QD0
              x7_a3QD1
              x8_a3QD2
              x9_a3QD3
              x10_a3QD4
              x11_a3QD5
              x12_a3QD6
              y1_a3QD8)
      (f_a3QCU x13_a3QD7)
{-# INLINE listConfig_elConfig #-}
listConfig_floated ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (Active t_a3QlD (Maybe Floated))
listConfig_floated
  f_a3QD9
  (ListConfig x1_a3QDa
              x2_a3QDb
              x3_a3QDc
              x4_a3QDd
              x5_a3QDe
              x6_a3QDf
              x7_a3QDg
              x8_a3QDh
              x9_a3QDi
              x10_a3QDj
              x11_a3QDk
              x12_a3QDl
              x13_a3QDm)
  = fmap
      (\ y1_a3QDn
         -> ListConfig
              x1_a3QDa
              x2_a3QDb
              x3_a3QDc
              x4_a3QDd
              x5_a3QDe
              x6_a3QDf
              x7_a3QDg
              x8_a3QDh
              x9_a3QDi
              x10_a3QDj
              y1_a3QDn
              x12_a3QDl
              x13_a3QDm)
      (f_a3QD9 x11_a3QDk)
{-# INLINE listConfig_floated #-}
listConfig_horizontal ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (Active t_a3QlD Bool)
listConfig_horizontal
  f_a3QDo
  (ListConfig x1_a3QDp
              x2_a3QDq
              x3_a3QDr
              x4_a3QDs
              x5_a3QDt
              x6_a3QDu
              x7_a3QDv
              x8_a3QDw
              x9_a3QDx
              x10_a3QDy
              x11_a3QDz
              x12_a3QDA
              x13_a3QDB)
  = fmap
      (\ y1_a3QDC
         -> ListConfig
              x1_a3QDp
              x2_a3QDq
              y1_a3QDC
              x4_a3QDs
              x5_a3QDt
              x6_a3QDu
              x7_a3QDv
              x8_a3QDw
              x9_a3QDx
              x10_a3QDy
              x11_a3QDz
              x12_a3QDA
              x13_a3QDB)
      (f_a3QDo x3_a3QDr)
{-# INLINE listConfig_horizontal #-}
listConfig_inverted ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (Active t_a3QlD Bool)
listConfig_inverted
  f_a3QDD
  (ListConfig x1_a3QDE
              x2_a3QDF
              x3_a3QDG
              x4_a3QDH
              x5_a3QDI
              x6_a3QDJ
              x7_a3QDK
              x8_a3QDL
              x9_a3QDM
              x10_a3QDN
              x11_a3QDO
              x12_a3QDP
              x13_a3QDQ)
  = fmap
      (\ y1_a3QDR
         -> ListConfig
              x1_a3QDE
              x2_a3QDF
              x3_a3QDG
              y1_a3QDR
              x5_a3QDI
              x6_a3QDJ
              x7_a3QDK
              x8_a3QDL
              x9_a3QDM
              x10_a3QDN
              x11_a3QDO
              x12_a3QDP
              x13_a3QDQ)
      (f_a3QDD x4_a3QDH)
{-# INLINE listConfig_inverted #-}
listConfig_link ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (Active t_a3QlD Bool)
listConfig_link
  f_a3QDS
  (ListConfig x1_a3QDT
              x2_a3QDU
              x3_a3QDV
              x4_a3QDW
              x5_a3QDX
              x6_a3QDY
              x7_a3QDZ
              x8_a3QE0
              x9_a3QE1
              x10_a3QE2
              x11_a3QE3
              x12_a3QE4
              x13_a3QE5)
  = fmap
      (\ y1_a3QE6
         -> ListConfig
              x1_a3QDT
              y1_a3QE6
              x3_a3QDV
              x4_a3QDW
              x5_a3QDX
              x6_a3QDY
              x7_a3QDZ
              x8_a3QE0
              x9_a3QE1
              x10_a3QE2
              x11_a3QE3
              x12_a3QE4
              x13_a3QE5)
      (f_a3QDS x2_a3QDU)
{-# INLINE listConfig_link #-}
listConfig_relaxed ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (Active t_a3QlD (Maybe Relaxed))
listConfig_relaxed
  f_a3QE7
  (ListConfig x1_a3QE8
              x2_a3QE9
              x3_a3QEa
              x4_a3QEb
              x5_a3QEc
              x6_a3QEd
              x7_a3QEe
              x8_a3QEf
              x9_a3QEg
              x10_a3QEh
              x11_a3QEi
              x12_a3QEj
              x13_a3QEk)
  = fmap
      (\ y1_a3QEl
         -> ListConfig
              x1_a3QE8
              x2_a3QE9
              x3_a3QEa
              x4_a3QEb
              x5_a3QEc
              x6_a3QEd
              y1_a3QEl
              x8_a3QEf
              x9_a3QEg
              x10_a3QEh
              x11_a3QEi
              x12_a3QEj
              x13_a3QEk)
      (f_a3QE7 x7_a3QEe)
{-# INLINE listConfig_relaxed #-}
listConfig_selection ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (Active t_a3QlD Bool)
listConfig_selection
  f_a3QEm
  (ListConfig x1_a3QEn
              x2_a3QEo
              x3_a3QEp
              x4_a3QEq
              x5_a3QEr
              x6_a3QEs
              x7_a3QEt
              x8_a3QEu
              x9_a3QEv
              x10_a3QEw
              x11_a3QEx
              x12_a3QEy
              x13_a3QEz)
  = fmap
      (\ y1_a3QEA
         -> ListConfig
              x1_a3QEn
              x2_a3QEo
              x3_a3QEp
              x4_a3QEq
              y1_a3QEA
              x6_a3QEs
              x7_a3QEt
              x8_a3QEu
              x9_a3QEv
              x10_a3QEw
              x11_a3QEx
              x12_a3QEy
              x13_a3QEz)
      (f_a3QEm x5_a3QEr)
{-# INLINE listConfig_selection #-}
listConfig_size ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (Active t_a3QlD (Maybe Size))
listConfig_size
  f_a3QEB
  (ListConfig x1_a3QEC
              x2_a3QED
              x3_a3QEE
              x4_a3QEF
              x5_a3QEG
              x6_a3QEH
              x7_a3QEI
              x8_a3QEJ
              x9_a3QEK
              x10_a3QEL
              x11_a3QEM
              x12_a3QEN
              x13_a3QEO)
  = fmap
      (\ y1_a3QEP
         -> ListConfig
              x1_a3QEC
              x2_a3QED
              x3_a3QEE
              x4_a3QEF
              x5_a3QEG
              x6_a3QEH
              x7_a3QEI
              x8_a3QEJ
              x9_a3QEK
              y1_a3QEP
              x11_a3QEM
              x12_a3QEN
              x13_a3QEO)
      (f_a3QEB x10_a3QEL)
{-# INLINE listConfig_size #-}
listConfig_type ::
  forall t_a3QlD.
  Control.Lens.Type.Lens' (ListConfig t_a3QlD) (Active t_a3QlD (Maybe ListType))
listConfig_type
  f_a3QEQ
  (ListConfig x1_a3QER
              x2_a3QES
              x3_a3QET
              x4_a3QEU
              x5_a3QEV
              x6_a3QEW
              x7_a3QEX
              x8_a3QEY
              x9_a3QEZ
              x10_a3QF0
              x11_a3QF1
              x12_a3QF2
              x13_a3QF3)
  = fmap
      (\ y1_a3QF4
         -> ListConfig
              y1_a3QF4
              x2_a3QES
              x3_a3QET
              x4_a3QEU
              x5_a3QEV
              x6_a3QEW
              x7_a3QEX
              x8_a3QEY
              x9_a3QEZ
              x10_a3QF0
              x11_a3QF1
              x12_a3QF2
              x13_a3QF3)
      (f_a3QEQ x1_a3QER)
{-# INLINE listConfig_type #-}
-- src/Reflex/Dom/SemanticUI/List.hs:103:1-66: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''ListItemConfig
--   ======>
listItemConfig_elConfig ::
  forall t_a3QF5 m_a3QF6.
  Control.Lens.Type.Lens' (ListItemConfig t_a3QF5 m_a3QF6) (ActiveElConfig t_a3QF5)
listItemConfig_elConfig
  f_a3QOw
  (ListItemConfig x1_a3QOx x2_a3QOy x3_a3QOz)
  = fmap
      (\ y1_a3QOA -> ListItemConfig x1_a3QOx x2_a3QOy y1_a3QOA)
      (f_a3QOw x3_a3QOz)
{-# INLINE listItemConfig_elConfig #-}
listItemConfig_element ::
  forall t_a3QF5 m_a3QF6.
  Control.Lens.Type.Lens' (ListItemConfig t_a3QF5 m_a3QF6) ListItemElement
listItemConfig_element
  f_a3QOB
  (ListItemConfig x1_a3QOC x2_a3QOD x3_a3QOE)
  = fmap
      (\ y1_a3QOF -> ListItemConfig x1_a3QOC y1_a3QOF x3_a3QOE)
      (f_a3QOB x2_a3QOD)
{-# INLINE listItemConfig_element #-}
listItemConfig_preContent ::
  forall t_a3QF5 m_a3QF6.
  Control.Lens.Type.Lens' (ListItemConfig t_a3QF5 m_a3QF6) (Maybe (m_a3QF6 ()))
listItemConfig_preContent
  f_a3QOG
  (ListItemConfig x1_a3QOH x2_a3QOI x3_a3QOJ)
  = fmap
      (\ y1_a3QOK -> ListItemConfig y1_a3QOK x2_a3QOI x3_a3QOJ)
      (f_a3QOG x1_a3QOH)
{-# INLINE listItemConfig_preContent #-}

