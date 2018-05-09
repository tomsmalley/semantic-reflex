-- src/Reflex/Dom/SemanticUI/Menu.hs:36:1-23: Splicing declarations
menuConfig_color ::
  forall t_a2lvR.
  Lens' (MenuConfig t_a2lvR) (Active t_a2lvR (Maybe Color))
menuConfig_color
  f_a2lCP
  (MenuConfig x1_a2lCQ
              x2_a2lCR
              x3_a2lCS
              x4_a2lCT
              x5_a2lCV
              x6_a2lCW
              x7_a2lCX
              x8_a2lCY
              x9_a2lCZ
              x10_a2lD0
              x11_a2lD1
              x12_a2lD2)
  = fmap
      (\ y1_a2lD3
         -> MenuConfig
              y1_a2lD3
              x2_a2lCR
              x3_a2lCS
              x4_a2lCT
              x5_a2lCV
              x6_a2lCW
              x7_a2lCX
              x8_a2lCY
              x9_a2lCZ
              x10_a2lD0
              x11_a2lD1
              x12_a2lD2)
      (f_a2lCP x1_a2lCQ)
{-# INLINE menuConfig_color #-}
menuConfig_compact ::
  forall t_a2lvR. Lens' (MenuConfig t_a2lvR) (Active t_a2lvR Bool)
menuConfig_compact
  f_a2lD5
  (MenuConfig x1_a2lD6
              x2_a2lD7
              x3_a2lD8
              x4_a2lD9
              x5_a2lDa
              x6_a2lDb
              x7_a2lDc
              x8_a2lDd
              x9_a2lDe
              x10_a2lDf
              x11_a2lDg
              x12_a2lDh)
  = fmap
      (\ y1_a2lDi
         -> MenuConfig
              x1_a2lD6
              x2_a2lD7
              x3_a2lD8
              x4_a2lD9
              x5_a2lDa
              x6_a2lDb
              x7_a2lDc
              x8_a2lDd
              x9_a2lDe
              y1_a2lDi
              x11_a2lDg
              x12_a2lDh)
      (f_a2lD5 x10_a2lDf)
{-# INLINE menuConfig_compact #-}
menuConfig_elConfig ::
  forall t_a2lvR. Lens' (MenuConfig t_a2lvR) (ActiveElConfig t_a2lvR)
menuConfig_elConfig
  f_a2lDk
  (MenuConfig x1_a2lDl
              x2_a2lDm
              x3_a2lDn
              x4_a2lDo
              x5_a2lDp
              x6_a2lDq
              x7_a2lDr
              x8_a2lDs
              x9_a2lDt
              x10_a2lDu
              x11_a2lDv
              x12_a2lDw)
  = fmap
      (\ y1_a2lDx
         -> MenuConfig
              x1_a2lDl
              x2_a2lDm
              x3_a2lDn
              x4_a2lDo
              x5_a2lDp
              x6_a2lDq
              x7_a2lDr
              x8_a2lDs
              x9_a2lDt
              x10_a2lDu
              x11_a2lDv
              y1_a2lDx)
      (f_a2lDk x12_a2lDw)
{-# INLINE menuConfig_elConfig #-}
menuConfig_floated ::
  forall t_a2lvR.
  Lens' (MenuConfig t_a2lvR) (Active t_a2lvR (Maybe Floated))
menuConfig_floated
  f_a2lDz
  (MenuConfig x1_a2lDA
              x2_a2lDB
              x3_a2lDC
              x4_a2lDD
              x5_a2lDE
              x6_a2lDF
              x7_a2lDG
              x8_a2lDH
              x9_a2lDI
              x10_a2lDJ
              x11_a2lDL
              x12_a2lDM)
  = fmap
      (\ y1_a2lDN
         -> MenuConfig
              x1_a2lDA
              x2_a2lDB
              x3_a2lDC
              x4_a2lDD
              x5_a2lDE
              x6_a2lDF
              x7_a2lDG
              x8_a2lDH
              x9_a2lDI
              x10_a2lDJ
              y1_a2lDN
              x12_a2lDM)
      (f_a2lDz x11_a2lDL)
{-# INLINE menuConfig_floated #-}
menuConfig_fluid ::
  forall t_a2lvR. Lens' (MenuConfig t_a2lvR) (Active t_a2lvR Bool)
menuConfig_fluid
  f_a2lDO
  (MenuConfig x1_a2lDP
              x2_a2lDQ
              x3_a2lDR
              x4_a2lDS
              x5_a2lDT
              x6_a2lDU
              x7_a2lDV
              x8_a2lDX
              x9_a2lDY
              x10_a2lDZ
              x11_a2lE0
              x12_a2lE1)
  = fmap
      (\ y1_a2lE2
         -> MenuConfig
              x1_a2lDP
              x2_a2lDQ
              x3_a2lDR
              x4_a2lDS
              x5_a2lDT
              x6_a2lDU
              x7_a2lDV
              y1_a2lE2
              x9_a2lDY
              x10_a2lDZ
              x11_a2lE0
              x12_a2lE1)
      (f_a2lDO x8_a2lDX)
{-# INLINE menuConfig_fluid #-}
menuConfig_inverted ::
  forall t_a2lvR. Lens' (MenuConfig t_a2lvR) (Active t_a2lvR Bool)
menuConfig_inverted
  f_a2lE4
  (MenuConfig x1_a2lE5
              x2_a2lE6
              x3_a2lE7
              x4_a2lE8
              x5_a2lE9
              x6_a2lEa
              x7_a2lEb
              x8_a2lEc
              x9_a2lEd
              x10_a2lEe
              x11_a2lEf
              x12_a2lEg)
  = fmap
      (\ y1_a2lEh
         -> MenuConfig
              x1_a2lE5
              y1_a2lEh
              x3_a2lE7
              x4_a2lE8
              x5_a2lE9
              x6_a2lEa
              x7_a2lEb
              x8_a2lEc
              x9_a2lEd
              x10_a2lEe
              x11_a2lEf
              x12_a2lEg)
      (f_a2lE4 x2_a2lE6)
{-# INLINE menuConfig_inverted #-}
menuConfig_pointing ::
  forall t_a2lvR. Lens' (MenuConfig t_a2lvR) (Active t_a2lvR Bool)
menuConfig_pointing
  f_a2lEi
  (MenuConfig x1_a2lEj
              x2_a2lEk
              x3_a2lEl
              x4_a2lEm
              x5_a2lEn
              x6_a2lEo
              x7_a2lEp
              x8_a2lEq
              x9_a2lEr
              x10_a2lEs
              x11_a2lEt
              x12_a2lEu)
  = fmap
      (\ y1_a2lEv
         -> MenuConfig
              x1_a2lEj
              x2_a2lEk
              x3_a2lEl
              x4_a2lEm
              x5_a2lEn
              x6_a2lEo
              y1_a2lEv
              x8_a2lEq
              x9_a2lEr
              x10_a2lEs
              x11_a2lEt
              x12_a2lEu)
      (f_a2lEi x7_a2lEp)
{-# INLINE menuConfig_pointing #-}
menuConfig_right ::
  forall t_a2lvR. Lens' (MenuConfig t_a2lvR) (Active t_a2lvR Bool)
menuConfig_right
  f_a2lEw
  (MenuConfig x1_a2lEx
              x2_a2lEy
              x3_a2lEz
              x4_a2lEA
              x5_a2lEB
              x6_a2lEC
              x7_a2lED
              x8_a2lEE
              x9_a2lEF
              x10_a2lEG
              x11_a2lEH
              x12_a2lEI)
  = fmap
      (\ y1_a2lEJ
         -> MenuConfig
              x1_a2lEx
              x2_a2lEy
              x3_a2lEz
              x4_a2lEA
              x5_a2lEB
              y1_a2lEJ
              x7_a2lED
              x8_a2lEE
              x9_a2lEF
              x10_a2lEG
              x11_a2lEH
              x12_a2lEI)
      (f_a2lEw x6_a2lEC)
{-# INLINE menuConfig_right #-}
menuConfig_secondary ::
  forall t_a2lvR. Lens' (MenuConfig t_a2lvR) (Active t_a2lvR Bool)
menuConfig_secondary
  f_a2lEK
  (MenuConfig x1_a2lEL
              x2_a2lEM
              x3_a2lEN
              x4_a2lEO
              x5_a2lEP
              x6_a2lEQ
              x7_a2lER
              x8_a2lES
              x9_a2lET
              x10_a2lEU
              x11_a2lEV
              x12_a2lEW)
  = fmap
      (\ y1_a2lEX
         -> MenuConfig
              x1_a2lEL
              x2_a2lEM
              x3_a2lEN
              x4_a2lEO
              y1_a2lEX
              x6_a2lEQ
              x7_a2lER
              x8_a2lES
              x9_a2lET
              x10_a2lEU
              x11_a2lEV
              x12_a2lEW)
      (f_a2lEK x5_a2lEP)
{-# INLINE menuConfig_secondary #-}
menuConfig_size ::
  forall t_a2lvR.
  Lens' (MenuConfig t_a2lvR) (Active t_a2lvR (Maybe Size))
menuConfig_size
  f_a2lEY
  (MenuConfig x1_a2lEZ
              x2_a2lF0
              x3_a2lF1
              x4_a2lF2
              x5_a2lF3
              x6_a2lF4
              x7_a2lF5
              x8_a2lF6
              x9_a2lF7
              x10_a2lF9
              x11_a2lFa
              x12_a2lFb)
  = fmap
      (\ y1_a2lFc
         -> MenuConfig
              x1_a2lEZ
              x2_a2lF0
              y1_a2lFc
              x4_a2lF2
              x5_a2lF3
              x6_a2lF4
              x7_a2lF5
              x8_a2lF6
              x9_a2lF7
              x10_a2lF9
              x11_a2lFa
              x12_a2lFb)
      (f_a2lEY x3_a2lF1)
{-# INLINE menuConfig_size #-}
menuConfig_text ::
  forall t_a2lvR. Lens' (MenuConfig t_a2lvR) (Active t_a2lvR Bool)
menuConfig_text
  f_a2lFd
  (MenuConfig x1_a2lFe
              x2_a2lFf
              x3_a2lFg
              x4_a2lFh
              x5_a2lFi
              x6_a2lFj
              x7_a2lFk
              x8_a2lFl
              x9_a2lFm
              x10_a2lFn
              x11_a2lFo
              x12_a2lFp)
  = fmap
      (\ y1_a2lFq
         -> MenuConfig
              x1_a2lFe
              x2_a2lFf
              x3_a2lFg
              x4_a2lFh
              x5_a2lFi
              x6_a2lFj
              x7_a2lFk
              x8_a2lFl
              y1_a2lFq
              x10_a2lFn
              x11_a2lFo
              x12_a2lFp)
      (f_a2lFd x9_a2lFm)
{-# INLINE menuConfig_text #-}
menuConfig_vertical ::
  forall t_a2lvR. Lens' (MenuConfig t_a2lvR) (Active t_a2lvR Bool)
menuConfig_vertical
  f_a2lFs
  (MenuConfig x1_a2lFt
              x2_a2lFu
              x3_a2lFv
              x4_a2lFw
              x5_a2lFx
              x6_a2lFy
              x7_a2lFz
              x8_a2lFA
              x9_a2lFB
              x10_a2lFC
              x11_a2lFD
              x12_a2lFE)
  = fmap
      (\ y1_a2lFF
         -> MenuConfig
              x1_a2lFt
              x2_a2lFu
              x3_a2lFv
              y1_a2lFF
              x5_a2lFx
              x6_a2lFy
              x7_a2lFz
              x8_a2lFA
              x9_a2lFB
              x10_a2lFC
              x11_a2lFD
              x12_a2lFE)
      (f_a2lFs x4_a2lFw)
{-# INLINE menuConfig_vertical #-}
-- src/Reflex/Dom/SemanticUI/Menu.hs:101:1-27: Splicing declarations
menuItemConfig_color ::
  forall t_a2lMd.
  Lens' (MenuItemConfig t_a2lMd) (Active t_a2lMd (Maybe Color))
menuItemConfig_color
  f_a2mJC
  (MenuItemConfig x1_a2mJD x2_a2mJE x3_a2mJF x4_a2mJG)
  = fmap
      (\ y1_a2mJH -> MenuItemConfig y1_a2mJH x2_a2mJE x3_a2mJF x4_a2mJG)
      (f_a2mJC x1_a2mJD)
{-# INLINE menuItemConfig_color #-}
menuItemConfig_disabled ::
  forall t_a2lMd.
  Lens' (MenuItemConfig t_a2lMd) (Active t_a2lMd Bool)
menuItemConfig_disabled
  f_a2mJJ
  (MenuItemConfig x1_a2mJK x2_a2mJL x3_a2mJM x4_a2mJN)
  = fmap
      (\ y1_a2mJO -> MenuItemConfig x1_a2mJK y1_a2mJO x3_a2mJM x4_a2mJN)
      (f_a2mJJ x2_a2mJL)
{-# INLINE menuItemConfig_disabled #-}
menuItemConfig_elConfig ::
  forall t_a2lMd.
  Lens' (MenuItemConfig t_a2lMd) (ActiveElConfig t_a2lMd)
menuItemConfig_elConfig
  f_a2mJQ
  (MenuItemConfig x1_a2mJR x2_a2mJS x3_a2mJT x4_a2mJU)
  = fmap
      (\ y1_a2mJV -> MenuItemConfig x1_a2mJR x2_a2mJS x3_a2mJT y1_a2mJV)
      (f_a2mJQ x4_a2mJU)
{-# INLINE menuItemConfig_elConfig #-}
menuItemConfig_link ::
  forall t_a2lMd. Lens' (MenuItemConfig t_a2lMd) MenuLink
menuItemConfig_link
  f_a2mJX
  (MenuItemConfig x1_a2mJY x2_a2mJZ x3_a2mK0 x4_a2mK1)
  = fmap
      (\ y1_a2mK2 -> MenuItemConfig x1_a2mJY x2_a2mJZ y1_a2mK2 x4_a2mK1)
      (f_a2mJX x3_a2mK0)
{-# INLINE menuItemConfig_link #-}
