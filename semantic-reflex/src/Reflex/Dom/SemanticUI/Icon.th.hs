-- src/Reflex/Dom/SemanticUI/Icon.hs:83:1-62: Splicing declarations
iconConfig_bordered ::
  forall t_a2Azx. Lens' (IconConfig t_a2Azx) (Active t_a2Azx Bool)
iconConfig_bordered
  f_a2ATy
  (IconConfig x1_a2ATz
              x2_a2ATA
              x3_a2ATB
              x4_a2ATC
              x5_a2ATD
              x6_a2ATE
              x7_a2ATF
              x8_a2ATG
              x9_a2ATH
              x10_a2ATI
              x11_a2ATJ
              x12_a2ATK
              x13_a2ATL
              x14_a2ATM)
  = fmap
      (\ y1_a2ATN
         -> IconConfig
              x1_a2ATz
              x2_a2ATA
              x3_a2ATB
              x4_a2ATC
              x5_a2ATD
              y1_a2ATN
              x7_a2ATF
              x8_a2ATG
              x9_a2ATH
              x10_a2ATI
              x11_a2ATJ
              x12_a2ATK
              x13_a2ATL
              x14_a2ATM)
      (f_a2ATy x6_a2ATE)
{-# INLINE iconConfig_bordered #-}
iconConfig_circular ::
  forall t_a2Azx. Lens' (IconConfig t_a2Azx) (Active t_a2Azx Bool)
iconConfig_circular
  f_a2ATO
  (IconConfig x1_a2ATP
              x2_a2ATQ
              x3_a2ATR
              x4_a2ATS
              x5_a2ATT
              x6_a2ATU
              x7_a2ATV
              x8_a2ATW
              x9_a2ATX
              x10_a2ATY
              x11_a2ATZ
              x12_a2AU0
              x13_a2AU1
              x14_a2AU2)
  = fmap
      (\ y1_a2AU3
         -> IconConfig
              x1_a2ATP
              x2_a2ATQ
              x3_a2ATR
              x4_a2ATS
              y1_a2AU3
              x6_a2ATU
              x7_a2ATV
              x8_a2ATW
              x9_a2ATX
              x10_a2ATY
              x11_a2ATZ
              x12_a2AU0
              x13_a2AU1
              x14_a2AU2)
      (f_a2ATO x5_a2ATT)
{-# INLINE iconConfig_circular #-}
iconConfig_color ::
  forall t_a2Azx.
  Lens' (IconConfig t_a2Azx) (Active t_a2Azx (Maybe Color))
iconConfig_color
  f_a2AU4
  (IconConfig x1_a2AU5
              x2_a2AU6
              x3_a2AU7
              x4_a2AU8
              x5_a2AU9
              x6_a2AUa
              x7_a2AUb
              x8_a2AUc
              x9_a2AUd
              x10_a2AUe
              x11_a2AUf
              x12_a2AUg
              x13_a2AUh
              x14_a2AUi)
  = fmap
      (\ y1_a2AUj
         -> IconConfig
              x1_a2AU5
              x2_a2AU6
              x3_a2AU7
              x4_a2AU8
              x5_a2AU9
              x6_a2AUa
              x7_a2AUb
              x8_a2AUc
              x9_a2AUd
              x10_a2AUe
              y1_a2AUj
              x12_a2AUg
              x13_a2AUh
              x14_a2AUi)
      (f_a2AU4 x11_a2AUf)
{-# INLINE iconConfig_color #-}
iconConfig_corner ::
  forall t_a2Azx.
  Lens' (IconConfig t_a2Azx) (Active t_a2Azx (Maybe Corner))
iconConfig_corner
  f_a2AUk
  (IconConfig x1_a2AUl
              x2_a2AUm
              x3_a2AUn
              x4_a2AUo
              x5_a2AUp
              x6_a2AUq
              x7_a2AUr
              x8_a2AUs
              x9_a2AUt
              x10_a2AUu
              x11_a2AUv
              x12_a2AUw
              x13_a2AUx
              x14_a2AUy)
  = fmap
      (\ y1_a2AUz
         -> IconConfig
              x1_a2AUl
              x2_a2AUm
              x3_a2AUn
              x4_a2AUo
              x5_a2AUp
              x6_a2AUq
              x7_a2AUr
              x8_a2AUs
              x9_a2AUt
              x10_a2AUu
              x11_a2AUv
              y1_a2AUz
              x13_a2AUx
              x14_a2AUy)
      (f_a2AUk x12_a2AUw)
{-# INLINE iconConfig_corner #-}
iconConfig_disabled ::
  forall t_a2Azx. Lens' (IconConfig t_a2Azx) (Active t_a2Azx Bool)
iconConfig_disabled
  f_a2AUA
  (IconConfig x1_a2AUB
              x2_a2AUC
              x3_a2AUD
              x4_a2AUE
              x5_a2AUF
              x6_a2AUG
              x7_a2AUH
              x8_a2AUI
              x9_a2AUJ
              x10_a2AUK
              x11_a2AUL
              x12_a2AUM
              x13_a2AUN
              x14_a2AUO)
  = fmap
      (\ y1_a2AUP
         -> IconConfig
              y1_a2AUP
              x2_a2AUC
              x3_a2AUD
              x4_a2AUE
              x5_a2AUF
              x6_a2AUG
              x7_a2AUH
              x8_a2AUI
              x9_a2AUJ
              x10_a2AUK
              x11_a2AUL
              x12_a2AUM
              x13_a2AUN
              x14_a2AUO)
      (f_a2AUA x1_a2AUB)
{-# INLINE iconConfig_disabled #-}
iconConfig_elConfig ::
  forall t_a2Azx. Lens' (IconConfig t_a2Azx) (ActiveElConfig t_a2Azx)
iconConfig_elConfig
  f_a2AUQ
  (IconConfig x1_a2AUR
              x2_a2AUS
              x3_a2AUT
              x4_a2AUU
              x5_a2AUV
              x6_a2AUW
              x7_a2AUX
              x8_a2AUY
              x9_a2AUZ
              x10_a2AV0
              x11_a2AV1
              x12_a2AV2
              x13_a2AV3
              x14_a2AV4)
  = fmap
      (\ y1_a2AV5
         -> IconConfig
              x1_a2AUR
              x2_a2AUS
              x3_a2AUT
              x4_a2AUU
              x5_a2AUV
              x6_a2AUW
              x7_a2AUX
              x8_a2AUY
              x9_a2AUZ
              x10_a2AV0
              x11_a2AV1
              x12_a2AV2
              x13_a2AV3
              y1_a2AV5)
      (f_a2AUQ x14_a2AV4)
{-# INLINE iconConfig_elConfig #-}
iconConfig_fitted ::
  forall t_a2Azx. Lens' (IconConfig t_a2Azx) (Active t_a2Azx Bool)
iconConfig_fitted
  f_a2AV6
  (IconConfig x1_a2AV7
              x2_a2AV8
              x3_a2AV9
              x4_a2AVa
              x5_a2AVb
              x6_a2AVc
              x7_a2AVd
              x8_a2AVe
              x9_a2AVf
              x10_a2AVg
              x11_a2AVh
              x12_a2AVi
              x13_a2AVj
              x14_a2AVk)
  = fmap
      (\ y1_a2AVl
         -> IconConfig
              x1_a2AV7
              x2_a2AV8
              y1_a2AVl
              x4_a2AVa
              x5_a2AVb
              x6_a2AVc
              x7_a2AVd
              x8_a2AVe
              x9_a2AVf
              x10_a2AVg
              x11_a2AVh
              x12_a2AVi
              x13_a2AVj
              x14_a2AVk)
      (f_a2AV6 x3_a2AV9)
{-# INLINE iconConfig_fitted #-}
iconConfig_flipped ::
  forall t_a2Azx.
  Lens' (IconConfig t_a2Azx) (Active t_a2Azx (Maybe Flipped))
iconConfig_flipped
  f_a2AVm
  (IconConfig x1_a2AVn
              x2_a2AVo
              x3_a2AVp
              x4_a2AVq
              x5_a2AVr
              x6_a2AVs
              x7_a2AVt
              x8_a2AVu
              x9_a2AVv
              x10_a2AVw
              x11_a2AVx
              x12_a2AVy
              x13_a2AVz
              x14_a2AVA)
  = fmap
      (\ y1_a2AVB
         -> IconConfig
              x1_a2AVn
              x2_a2AVo
              x3_a2AVp
              x4_a2AVq
              x5_a2AVr
              x6_a2AVs
              x7_a2AVt
              x8_a2AVu
              y1_a2AVB
              x10_a2AVw
              x11_a2AVx
              x12_a2AVy
              x13_a2AVz
              x14_a2AVA)
      (f_a2AVm x9_a2AVv)
{-# INLINE iconConfig_flipped #-}
iconConfig_inverted ::
  forall t_a2Azx. Lens' (IconConfig t_a2Azx) (Active t_a2Azx Bool)
iconConfig_inverted
  f_a2AVC
  (IconConfig x1_a2AVD
              x2_a2AVE
              x3_a2AVF
              x4_a2AVG
              x5_a2AVH
              x6_a2AVI
              x7_a2AVJ
              x8_a2AVK
              x9_a2AVL
              x10_a2AVM
              x11_a2AVN
              x12_a2AVO
              x13_a2AVP
              x14_a2AVQ)
  = fmap
      (\ y1_a2AVR
         -> IconConfig
              x1_a2AVD
              x2_a2AVE
              x3_a2AVF
              x4_a2AVG
              x5_a2AVH
              x6_a2AVI
              y1_a2AVR
              x8_a2AVK
              x9_a2AVL
              x10_a2AVM
              x11_a2AVN
              x12_a2AVO
              x13_a2AVP
              x14_a2AVQ)
      (f_a2AVC x7_a2AVJ)
{-# INLINE iconConfig_inverted #-}
iconConfig_link ::
  forall t_a2Azx. Lens' (IconConfig t_a2Azx) (Active t_a2Azx Bool)
iconConfig_link
  f_a2AVS
  (IconConfig x1_a2AVT
              x2_a2AVU
              x3_a2AVV
              x4_a2AVW
              x5_a2AVX
              x6_a2AVY
              x7_a2AVZ
              x8_a2AW0
              x9_a2AW1
              x10_a2AW2
              x11_a2AW3
              x12_a2AW4
              x13_a2AW5
              x14_a2AW6)
  = fmap
      (\ y1_a2AW7
         -> IconConfig
              x1_a2AVT
              x2_a2AVU
              x3_a2AVV
              y1_a2AW7
              x5_a2AVX
              x6_a2AVY
              x7_a2AVZ
              x8_a2AW0
              x9_a2AW1
              x10_a2AW2
              x11_a2AW3
              x12_a2AW4
              x13_a2AW5
              x14_a2AW6)
      (f_a2AVS x4_a2AVW)
{-# INLINE iconConfig_link #-}
iconConfig_loading ::
  forall t_a2Azx. Lens' (IconConfig t_a2Azx) (Active t_a2Azx Bool)
iconConfig_loading
  f_a2AW8
  (IconConfig x1_a2AW9
              x2_a2AWa
              x3_a2AWb
              x4_a2AWc
              x5_a2AWd
              x6_a2AWe
              x7_a2AWf
              x8_a2AWg
              x9_a2AWh
              x10_a2AWi
              x11_a2AWj
              x12_a2AWk
              x13_a2AWl
              x14_a2AWm)
  = fmap
      (\ y1_a2AWn
         -> IconConfig
              x1_a2AW9
              y1_a2AWn
              x3_a2AWb
              x4_a2AWc
              x5_a2AWd
              x6_a2AWe
              x7_a2AWf
              x8_a2AWg
              x9_a2AWh
              x10_a2AWi
              x11_a2AWj
              x12_a2AWk
              x13_a2AWl
              x14_a2AWm)
      (f_a2AW8 x2_a2AWa)
{-# INLINE iconConfig_loading #-}
iconConfig_rotated ::
  forall t_a2Azx.
  Lens' (IconConfig t_a2Azx) (Active t_a2Azx (Maybe Rotated))
iconConfig_rotated
  f_a2AWo
  (IconConfig x1_a2AWp
              x2_a2AWq
              x3_a2AWr
              x4_a2AWs
              x5_a2AWt
              x6_a2AWu
              x7_a2AWv
              x8_a2AWw
              x9_a2AWx
              x10_a2AWy
              x11_a2AWz
              x12_a2AWA
              x13_a2AWB
              x14_a2AWC)
  = fmap
      (\ y1_a2AWD
         -> IconConfig
              x1_a2AWp
              x2_a2AWq
              x3_a2AWr
              x4_a2AWs
              x5_a2AWt
              x6_a2AWu
              x7_a2AWv
              x8_a2AWw
              x9_a2AWx
              y1_a2AWD
              x11_a2AWz
              x12_a2AWA
              x13_a2AWB
              x14_a2AWC)
      (f_a2AWo x10_a2AWy)
{-# INLINE iconConfig_rotated #-}
iconConfig_size ::
  forall t_a2Azx.
  Lens' (IconConfig t_a2Azx) (Active t_a2Azx (Maybe Size))
iconConfig_size
  f_a2AWE
  (IconConfig x1_a2AWF
              x2_a2AWG
              x3_a2AWH
              x4_a2AWI
              x5_a2AWJ
              x6_a2AWK
              x7_a2AWL
              x8_a2AWM
              x9_a2AWN
              x10_a2AWO
              x11_a2AWP
              x12_a2AWQ
              x13_a2AWR
              x14_a2AWS)
  = fmap
      (\ y1_a2AWT
         -> IconConfig
              x1_a2AWF
              x2_a2AWG
              x3_a2AWH
              x4_a2AWI
              x5_a2AWJ
              x6_a2AWK
              x7_a2AWL
              y1_a2AWT
              x9_a2AWN
              x10_a2AWO
              x11_a2AWP
              x12_a2AWQ
              x13_a2AWR
              x14_a2AWS)
      (f_a2AWE x8_a2AWM)
{-# INLINE iconConfig_size #-}
iconConfig_title ::
  forall t_a2Azx.
  Lens' (IconConfig t_a2Azx) (Active t_a2Azx (Maybe Text))
iconConfig_title
  f_a2AWU
  (IconConfig x1_a2AWV
              x2_a2AWW
              x3_a2AWX
              x4_a2AWY
              x5_a2AWZ
              x6_a2AX0
              x7_a2AX1
              x8_a2AX2
              x9_a2AX3
              x10_a2AX4
              x11_a2AX5
              x12_a2AX6
              x13_a2AX7
              x14_a2AX8)
  = fmap
      (\ y1_a2AX9
         -> IconConfig
              x1_a2AWV
              x2_a2AWW
              x3_a2AWX
              x4_a2AWY
              x5_a2AWZ
              x6_a2AX0
              x7_a2AX1
              x8_a2AX2
              x9_a2AX3
              x10_a2AX4
              x11_a2AX5
              x12_a2AX6
              y1_a2AX9
              x14_a2AX8)
      (f_a2AWU x13_a2AX7)
{-# INLINE iconConfig_title #-}
-- src/Reflex/Dom/SemanticUI/Icon.hs:162:1-63: Splicing declarations
iconsConfig_elConfig ::
  forall t_a2AYf.
  Lens' (IconsConfig t_a2AYf) (ActiveElConfig t_a2AYf)
iconsConfig_elConfig f_a2BD2 (IconsConfig x1_a2BD3 x2_a2BD4)
  = fmap
      (\ y1_a2BD5 -> IconsConfig x1_a2BD3 y1_a2BD5) (f_a2BD2 x2_a2BD4)
{-# INLINE iconsConfig_elConfig #-}
iconsConfig_size ::
  forall t_a2AYf.
  Lens' (IconsConfig t_a2AYf) (Active t_a2AYf (Maybe Size))
iconsConfig_size f_a2BD7 (IconsConfig x1_a2BD8 x2_a2BD9)
  = fmap
      (\ y1_a2BDa -> IconsConfig y1_a2BDa x2_a2BD9) (f_a2BD7 x1_a2BD8)
{-# INLINE iconsConfig_size #-}
