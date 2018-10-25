-- src/Reflex/Dom/SemanticUI/Input.hs:93:1-63: Splicing declarations
inputConfig_action ::
  forall t_a2kaU.
  Lens' (InputConfig t_a2kaU) (Active t_a2kaU (Maybe InputAction))
inputConfig_action
  f_a2kkl
  (InputConfig x1_a2kkm
               x2_a2kkn
               x3_a2kko
               x4_a2kkp
               x5_a2kkq
               x6_a2kkr
               x7_a2kks
               x8_a2kkt
               x9_a2kku
               x10_a2kkv
               x11_a2kkw)
  = fmap
      (\ y1_a2kkx
         -> InputConfig
              x1_a2kkm
              x2_a2kkn
              x3_a2kko
              x4_a2kkp
              x5_a2kkq
              x6_a2kkr
              x7_a2kks
              x8_a2kkt
              y1_a2kkx
              x10_a2kkv
              x11_a2kkw)
      (f_a2kkl x9_a2kku)
{-# INLINE inputConfig_action #-}
inputConfig_disabled ::
  forall t_a2kaU. Lens' (InputConfig t_a2kaU) (Active t_a2kaU Bool)
inputConfig_disabled
  f_a2kky
  (InputConfig x1_a2kkz
               x2_a2kkA
               x3_a2kkB
               x4_a2kkC
               x5_a2kkD
               x6_a2kkE
               x7_a2kkF
               x8_a2kkG
               x9_a2kkH
               x10_a2kkI
               x11_a2kkJ)
  = fmap
      (\ y1_a2kkK
         -> InputConfig
              x1_a2kkz
              y1_a2kkK
              x3_a2kkB
              x4_a2kkC
              x5_a2kkD
              x6_a2kkE
              x7_a2kkF
              x8_a2kkG
              x9_a2kkH
              x10_a2kkI
              x11_a2kkJ)
      (f_a2kky x2_a2kkA)
{-# INLINE inputConfig_disabled #-}
inputConfig_elConfig ::
  forall t_a2kaU.
  Lens' (InputConfig t_a2kaU) (ActiveElConfig t_a2kaU)
inputConfig_elConfig
  f_a2kkL
  (InputConfig x1_a2kkM
               x2_a2kkN
               x3_a2kkO
               x4_a2kkP
               x5_a2kkQ
               x6_a2kkR
               x7_a2kkS
               x8_a2kkT
               x9_a2kkU
               x10_a2kkV
               x11_a2kkW)
  = fmap
      (\ y1_a2kkX
         -> InputConfig
              x1_a2kkM
              x2_a2kkN
              x3_a2kkO
              x4_a2kkP
              x5_a2kkQ
              x6_a2kkR
              x7_a2kkS
              x8_a2kkT
              x9_a2kkU
              x10_a2kkV
              y1_a2kkX)
      (f_a2kkL x11_a2kkW)
{-# INLINE inputConfig_elConfig #-}
inputConfig_error ::
  forall t_a2kaU. Lens' (InputConfig t_a2kaU) (Active t_a2kaU Bool)
inputConfig_error
  f_a2kkY
  (InputConfig x1_a2kkZ
               x2_a2kl0
               x3_a2kl1
               x4_a2kl2
               x5_a2kl3
               x6_a2kl4
               x7_a2kl5
               x8_a2kl6
               x9_a2kl7
               x10_a2kl8
               x11_a2kl9)
  = fmap
      (\ y1_a2kla
         -> InputConfig
              x1_a2kkZ
              x2_a2kl0
              y1_a2kla
              x4_a2kl2
              x5_a2kl3
              x6_a2kl4
              x7_a2kl5
              x8_a2kl6
              x9_a2kl7
              x10_a2kl8
              x11_a2kl9)
      (f_a2kkY x3_a2kl1)
{-# INLINE inputConfig_error #-}
inputConfig_fluid ::
  forall t_a2kaU. Lens' (InputConfig t_a2kaU) (Active t_a2kaU Bool)
inputConfig_fluid
  f_a2klb
  (InputConfig x1_a2klc
               x2_a2kld
               x3_a2kle
               x4_a2klf
               x5_a2klg
               x6_a2klh
               x7_a2kli
               x8_a2klj
               x9_a2klk
               x10_a2kll
               x11_a2klm)
  = fmap
      (\ y1_a2kln
         -> InputConfig
              x1_a2klc
              x2_a2kld
              x3_a2kle
              x4_a2klf
              x5_a2klg
              y1_a2kln
              x7_a2kli
              x8_a2klj
              x9_a2klk
              x10_a2kll
              x11_a2klm)
      (f_a2klb x6_a2klh)
{-# INLINE inputConfig_fluid #-}
inputConfig_icon ::
  forall t_a2kaU.
  Lens' (InputConfig t_a2kaU) (Active t_a2kaU (Maybe InputIcon))
inputConfig_icon
  f_a2klo
  (InputConfig x1_a2klp
               x2_a2klq
               x3_a2klr
               x4_a2kls
               x5_a2klt
               x6_a2klu
               x7_a2klv
               x8_a2klw
               x9_a2klx
               x10_a2kly
               x11_a2klz)
  = fmap
      (\ y1_a2klA
         -> InputConfig
              x1_a2klp
              x2_a2klq
              x3_a2klr
              x4_a2kls
              x5_a2klt
              x6_a2klu
              y1_a2klA
              x8_a2klw
              x9_a2klx
              x10_a2kly
              x11_a2klz)
      (f_a2klo x7_a2klv)
{-# INLINE inputConfig_icon #-}
inputConfig_inverted ::
  forall t_a2kaU. Lens' (InputConfig t_a2kaU) (Active t_a2kaU Bool)
inputConfig_inverted
  f_a2klB
  (InputConfig x1_a2klC
               x2_a2klD
               x3_a2klE
               x4_a2klF
               x5_a2klG
               x6_a2klH
               x7_a2klI
               x8_a2klJ
               x9_a2klK
               x10_a2klL
               x11_a2klM)
  = fmap
      (\ y1_a2klN
         -> InputConfig
              x1_a2klC
              x2_a2klD
              x3_a2klE
              x4_a2klF
              y1_a2klN
              x6_a2klH
              x7_a2klI
              x8_a2klJ
              x9_a2klK
              x10_a2klL
              x11_a2klM)
      (f_a2klB x5_a2klG)
{-# INLINE inputConfig_inverted #-}
inputConfig_labeled ::
  forall t_a2kaU.
  Lens' (InputConfig t_a2kaU) (Active t_a2kaU (Maybe Labeled))
inputConfig_labeled
  f_a2klO
  (InputConfig x1_a2klP
               x2_a2klQ
               x3_a2klR
               x4_a2klS
               x5_a2klT
               x6_a2klU
               x7_a2klV
               x8_a2klW
               x9_a2klX
               x10_a2klY
               x11_a2klZ)
  = fmap
      (\ y1_a2km0
         -> InputConfig
              x1_a2klP
              x2_a2klQ
              x3_a2klR
              x4_a2klS
              x5_a2klT
              x6_a2klU
              x7_a2klV
              y1_a2km0
              x9_a2klX
              x10_a2klY
              x11_a2klZ)
      (f_a2klO x8_a2klW)
{-# INLINE inputConfig_labeled #-}
inputConfig_loading ::
  forall t_a2kaU. Lens' (InputConfig t_a2kaU) (Active t_a2kaU Bool)
inputConfig_loading
  f_a2km1
  (InputConfig x1_a2km2
               x2_a2km3
               x3_a2km4
               x4_a2km5
               x5_a2km6
               x6_a2km7
               x7_a2km8
               x8_a2km9
               x9_a2kma
               x10_a2kmb
               x11_a2kmc)
  = fmap
      (\ y1_a2kmd
         -> InputConfig
              y1_a2kmd
              x2_a2km3
              x3_a2km4
              x4_a2km5
              x5_a2km6
              x6_a2km7
              x7_a2km8
              x8_a2km9
              x9_a2kma
              x10_a2kmb
              x11_a2kmc)
      (f_a2km1 x1_a2km2)
{-# INLINE inputConfig_loading #-}
inputConfig_size ::
  forall t_a2kaU.
  Lens' (InputConfig t_a2kaU) (Active t_a2kaU (Maybe Size))
inputConfig_size
  f_a2kme
  (InputConfig x1_a2kmf
               x2_a2kmg
               x3_a2kmh
               x4_a2kmi
               x5_a2kmj
               x6_a2kmk
               x7_a2kml
               x8_a2kmm
               x9_a2kmn
               x10_a2kmo
               x11_a2kmp)
  = fmap
      (\ y1_a2kmq
         -> InputConfig
              x1_a2kmf
              x2_a2kmg
              x3_a2kmh
              x4_a2kmi
              x5_a2kmj
              x6_a2kmk
              x7_a2kml
              x8_a2kmm
              x9_a2kmn
              y1_a2kmq
              x11_a2kmp)
      (f_a2kme x10_a2kmo)
{-# INLINE inputConfig_size #-}
inputConfig_transparent ::
  forall t_a2kaU. Lens' (InputConfig t_a2kaU) (Active t_a2kaU Bool)
inputConfig_transparent
  f_a2kmr
  (InputConfig x1_a2kms
               x2_a2kmt
               x3_a2kmu
               x4_a2kmv
               x5_a2kmw
               x6_a2kmx
               x7_a2kmy
               x8_a2kmz
               x9_a2kmA
               x10_a2kmB
               x11_a2kmC)
  = fmap
      (\ y1_a2kmD
         -> InputConfig
              x1_a2kms
              x2_a2kmt
              x3_a2kmu
              y1_a2kmD
              x5_a2kmw
              x6_a2kmx
              x7_a2kmy
              x8_a2kmz
              x9_a2kmA
              x10_a2kmB
              x11_a2kmC)
      (f_a2kmr x4_a2kmv)
{-# INLINE inputConfig_transparent #-}
-- src/Reflex/Dom/SemanticUI/Input.hs:167:1-67: Splicing declarations
textInputConfig_attrs ::
  forall t_a2knL.
  Lens' (TextInputConfig t_a2knL) (Dynamic t_a2knL (Map Text Text))
textInputConfig_attrs
  f_a2nQZ
  (TextInputConfig x1_a2nR2 x2_a2nR4 x3_a2nR5 x4_a2nR7)
  = fmap
      (\ y1_a2nR8 -> TextInputConfig x1_a2nR2 x2_a2nR4 x3_a2nR5 y1_a2nR8)
      (f_a2nQZ x4_a2nR7)
{-# INLINE textInputConfig_attrs #-}
textInputConfig_placeholder ::
  forall t_a2knL.
  Lens' (TextInputConfig t_a2knL) (Dynamic t_a2knL Text)
textInputConfig_placeholder
  f_a2nRo
  (TextInputConfig x1_a2nRq x2_a2nRr x3_a2nRt x4_a2nRu)
  = fmap
      (\ y1_a2nRw -> TextInputConfig x1_a2nRq y1_a2nRw x3_a2nRt x4_a2nRu)
      (f_a2nRo x2_a2nRr)
{-# INLINE textInputConfig_placeholder #-}
textInputConfig_type ::
  forall t_a2knL. Lens' (TextInputConfig t_a2knL) InputType
textInputConfig_type
  f_a2nRE
  (TextInputConfig x1_a2nRF x2_a2nRH x3_a2nRJ x4_a2nRL)
  = fmap
      (\ y1_a2nRM -> TextInputConfig x1_a2nRF x2_a2nRH y1_a2nRM x4_a2nRL)
      (f_a2nRE x3_a2nRJ)
{-# INLINE textInputConfig_type #-}
textInputConfig_value ::
  forall t_a2knL.
  Lens' (TextInputConfig t_a2knL) (SetValue t_a2knL Text)
textInputConfig_value
  f_a2nRT
  (TextInputConfig x1_a2nRV x2_a2nRW x3_a2nRX x4_a2nRZ)
  = fmap
      (\ y1_a2nS2 -> TextInputConfig y1_a2nS2 x2_a2nRW x3_a2nRX x4_a2nRZ)
      (f_a2nRT x1_a2nRV)
{-# INLINE textInputConfig_value #-}
