-- src/Reflex/Dom/SemanticUI/Button.hs:96:1-65: Splicing declarations
buttonsConfig_attached ::
  forall t_a1JXX.
  Lens' (ButtonsConfig t_a1JXX) (Active t_a1JXX (Maybe VerticalAttached))
buttonsConfig_attached
  f_a1Ke3
  (ButtonsConfig x1_a1Ke4
                 x2_a1Ke6
                 x3_a1Ke7
                 x4_a1Ke8
                 x5_a1Kea
                 x6_a1Keb
                 x7_a1Kec
                 x8_a1Ked
                 x9_a1Kee
                 x10_a1Kef
                 x11_a1Keh)
  = fmap
      (\ y1_a1Kej
         -> ButtonsConfig
              x1_a1Ke4
              x2_a1Ke6
              x3_a1Ke7
              x4_a1Ke8
              x5_a1Kea
              x6_a1Keb
              x7_a1Kec
              y1_a1Kej
              x9_a1Kee
              x10_a1Kef
              x11_a1Keh)
      (f_a1Ke3 x8_a1Ked)
{-# INLINE buttonsConfig_attached #-}
buttonsConfig_basic ::
  forall t_a1JXX. Lens' (ButtonsConfig t_a1JXX) (Active t_a1JXX Bool)
buttonsConfig_basic
  f_a1Kes
  (ButtonsConfig x1_a1Kev
                 x2_a1Kew
                 x3_a1Kex
                 x4_a1Key
                 x5_a1Kez
                 x6_a1KeA
                 x7_a1KeD
                 x8_a1KeE
                 x9_a1KeF
                 x10_a1KeG
                 x11_a1KeI)
  = fmap
      (\ y1_a1KeK
         -> ButtonsConfig
              y1_a1KeK
              x2_a1Kew
              x3_a1Kex
              x4_a1Key
              x5_a1Kez
              x6_a1KeA
              x7_a1KeD
              x8_a1KeE
              x9_a1KeF
              x10_a1KeG
              x11_a1KeI)
      (f_a1Kes x1_a1Kev)
{-# INLINE buttonsConfig_basic #-}
buttonsConfig_color ::
  forall t_a1JXX.
  Lens' (ButtonsConfig t_a1JXX) (Active t_a1JXX (Maybe Color))
buttonsConfig_color
  f_a1KeQ
  (ButtonsConfig x1_a1KeS
                 x2_a1KeT
                 x3_a1KeV
                 x4_a1KeW
                 x5_a1KeX
                 x6_a1KeY
                 x7_a1KeZ
                 x8_a1Kf1
                 x9_a1Kf2
                 x10_a1Kf3
                 x11_a1Kf4)
  = fmap
      (\ y1_a1Kf5
         -> ButtonsConfig
              x1_a1KeS
              x2_a1KeT
              x3_a1KeV
              x4_a1KeW
              x5_a1KeX
              y1_a1Kf5
              x7_a1KeZ
              x8_a1Kf1
              x9_a1Kf2
              x10_a1Kf3
              x11_a1Kf4)
      (f_a1KeQ x6_a1KeY)
{-# INLINE buttonsConfig_color #-}
buttonsConfig_compact ::
  forall t_a1JXX. Lens' (ButtonsConfig t_a1JXX) (Active t_a1JXX Bool)
buttonsConfig_compact
  f_a1Kfc
  (ButtonsConfig x1_a1Kfd
                 x2_a1Kff
                 x3_a1Kfg
                 x4_a1Kfh
                 x5_a1Kfi
                 x6_a1Kfj
                 x7_a1Kfk
                 x8_a1Kfl
                 x9_a1Kfm
                 x10_a1Kfo
                 x11_a1Kfp)
  = fmap
      (\ y1_a1Kfq
         -> ButtonsConfig
              x1_a1Kfd
              x2_a1Kff
              x3_a1Kfg
              x4_a1Kfh
              y1_a1Kfq
              x6_a1Kfj
              x7_a1Kfk
              x8_a1Kfl
              x9_a1Kfm
              x10_a1Kfo
              x11_a1Kfp)
      (f_a1Kfc x5_a1Kfi)
{-# INLINE buttonsConfig_compact #-}
buttonsConfig_elConfig ::
  forall t_a1JXX.
  Lens' (ButtonsConfig t_a1JXX) (ActiveElConfig t_a1JXX)
buttonsConfig_elConfig
  f_a1Kfx
  (ButtonsConfig x1_a1Kfz
                 x2_a1KfA
                 x3_a1KfB
                 x4_a1KfC
                 x5_a1KfD
                 x6_a1KfE
                 x7_a1KfF
                 x8_a1KfG
                 x9_a1KfI
                 x10_a1KfJ
                 x11_a1KfK)
  = fmap
      (\ y1_a1KfL
         -> ButtonsConfig
              x1_a1Kfz
              x2_a1KfA
              x3_a1KfB
              x4_a1KfC
              x5_a1KfD
              x6_a1KfE
              x7_a1KfF
              x8_a1KfG
              x9_a1KfI
              x10_a1KfJ
              y1_a1KfL)
      (f_a1Kfx x11_a1KfK)
{-# INLINE buttonsConfig_elConfig #-}
buttonsConfig_floated ::
  forall t_a1JXX.
  Lens' (ButtonsConfig t_a1JXX) (Active t_a1JXX (Maybe Floated))
buttonsConfig_floated
  f_a1KfS
  (ButtonsConfig x1_a1KfT
                 x2_a1KfV
                 x3_a1KfW
                 x4_a1KfX
                 x5_a1KfY
                 x6_a1KfZ
                 x7_a1Kg0
                 x8_a1Kg1
                 x9_a1Kg3
                 x10_a1Kg5
                 x11_a1Kg6)
  = fmap
      (\ y1_a1Kg7
         -> ButtonsConfig
              x1_a1KfT
              x2_a1KfV
              x3_a1KfW
              x4_a1KfX
              x5_a1KfY
              x6_a1KfZ
              x7_a1Kg0
              x8_a1Kg1
              x9_a1Kg3
              y1_a1Kg7
              x11_a1Kg6)
      (f_a1KfS x10_a1Kg5)
{-# INLINE buttonsConfig_floated #-}
buttonsConfig_icon ::
  forall t_a1JXX. Lens' (ButtonsConfig t_a1JXX) (Active t_a1JXX Bool)
buttonsConfig_icon
  f_a1Kgd
  (ButtonsConfig x1_a1Kge
                 x2_a1Kgf
                 x3_a1Kgg
                 x4_a1Kgi
                 x5_a1Kgk
                 x6_a1Kgl
                 x7_a1Kgm
                 x8_a1Kgn
                 x9_a1Kgo
                 x10_a1Kgp
                 x11_a1Kgq)
  = fmap
      (\ y1_a1Kgr
         -> ButtonsConfig
              x1_a1Kge
              y1_a1Kgr
              x3_a1Kgg
              x4_a1Kgi
              x5_a1Kgk
              x6_a1Kgl
              x7_a1Kgm
              x8_a1Kgn
              x9_a1Kgo
              x10_a1Kgp
              x11_a1Kgq)
      (f_a1Kgd x2_a1Kgf)
{-# INLINE buttonsConfig_icon #-}
buttonsConfig_labeledIcon ::
  forall t_a1JXX. Lens' (ButtonsConfig t_a1JXX) (Active t_a1JXX Bool)
buttonsConfig_labeledIcon
  f_a1Kgx
  (ButtonsConfig x1_a1Kgy
                 x2_a1KgB
                 x3_a1KgC
                 x4_a1KgD
                 x5_a1KgE
                 x6_a1KgF
                 x7_a1KgG
                 x8_a1KgH
                 x9_a1KgI
                 x10_a1KgJ
                 x11_a1KgL)
  = fmap
      (\ y1_a1KgM
         -> ButtonsConfig
              x1_a1Kgy
              x2_a1KgB
              y1_a1KgM
              x4_a1KgD
              x5_a1KgE
              x6_a1KgF
              x7_a1KgG
              x8_a1KgH
              x9_a1KgI
              x10_a1KgJ
              x11_a1KgL)
      (f_a1Kgx x3_a1KgC)
{-# INLINE buttonsConfig_labeledIcon #-}
buttonsConfig_size ::
  forall t_a1JXX.
  Lens' (ButtonsConfig t_a1JXX) (Active t_a1JXX (Maybe Size))
buttonsConfig_size
  f_a1KgT
  (ButtonsConfig x1_a1KgU
                 x2_a1KgV
                 x3_a1KgW
                 x4_a1KgX
                 x5_a1KgZ
                 x6_a1Kh0
                 x7_a1Kh1
                 x8_a1Kh2
                 x9_a1Kh3
                 x10_a1Kh4
                 x11_a1Kh5)
  = fmap
      (\ y1_a1Kh7
         -> ButtonsConfig
              x1_a1KgU
              x2_a1KgV
              x3_a1KgW
              x4_a1KgX
              x5_a1KgZ
              x6_a1Kh0
              y1_a1Kh7
              x8_a1Kh2
              x9_a1Kh3
              x10_a1Kh4
              x11_a1Kh5)
      (f_a1KgT x7_a1Kh1)
{-# INLINE buttonsConfig_size #-}
buttonsConfig_vertical ::
  forall t_a1JXX. Lens' (ButtonsConfig t_a1JXX) (Active t_a1JXX Bool)
buttonsConfig_vertical
  f_a1Khd
  (ButtonsConfig x1_a1Khf
                 x2_a1Khg
                 x3_a1Khh
                 x4_a1Khi
                 x5_a1Khj
                 x6_a1Khk
                 x7_a1Khl
                 x8_a1Khm
                 x9_a1Kho
                 x10_a1Khp
                 x11_a1Khr)
  = fmap
      (\ y1_a1Khs
         -> ButtonsConfig
              x1_a1Khf
              x2_a1Khg
              x3_a1Khh
              y1_a1Khs
              x5_a1Khj
              x6_a1Khk
              x7_a1Khl
              x8_a1Khm
              x9_a1Kho
              x10_a1Khp
              x11_a1Khr)
      (f_a1Khd x4_a1Khi)
{-# INLINE buttonsConfig_vertical #-}
buttonsConfig_width ::
  forall t_a1JXX.
  Lens' (ButtonsConfig t_a1JXX) (Active t_a1JXX (Maybe Width))
buttonsConfig_width
  f_a1Khy
  (ButtonsConfig x1_a1Khz
                 x2_a1KhA
                 x3_a1KhB
                 x4_a1KhC
                 x5_a1KhE
                 x6_a1KhG
                 x7_a1KhH
                 x8_a1KhI
                 x9_a1KhJ
                 x10_a1KhK
                 x11_a1KhL)
  = fmap
      (\ y1_a1KhN
         -> ButtonsConfig
              x1_a1Khz
              x2_a1KhA
              x3_a1KhB
              x4_a1KhC
              x5_a1KhE
              x6_a1KhG
              x7_a1KhH
              x8_a1KhI
              y1_a1KhN
              x10_a1KhK
              x11_a1KhL)
      (f_a1Khy x9_a1KhJ)
{-# INLINE buttonsConfig_width #-}
-- src/Reflex/Dom/SemanticUI/Button.hs:142:1-71: Splicing declarations
labeledButtonConfig_elConfig ::
  forall t_a1KBl.
  Lens' (LabeledButtonConfig t_a1KBl) (ActiveElConfig t_a1KBl)
labeledButtonConfig_elConfig
  f_a1M6Z
  (LabeledButtonConfig x1_a1M72 x2_a1M73)
  = fmap
      (\ y1_a1M74 -> LabeledButtonConfig x1_a1M72 y1_a1M74)
      (f_a1M6Z x2_a1M73)
{-# INLINE labeledButtonConfig_elConfig #-}
labeledButtonConfig_side ::
  forall t_a1KBl.
  Lens' (LabeledButtonConfig t_a1KBl) (Active t_a1KBl Labeled)
labeledButtonConfig_side
  f_a1M7f
  (LabeledButtonConfig x1_a1M7h x2_a1M7i)
  = fmap
      (\ y1_a1M7j -> LabeledButtonConfig y1_a1M7j x2_a1M7i)
      (f_a1M7f x1_a1M7h)
{-# INLINE labeledButtonConfig_side #-}
-- src/Reflex/Dom/SemanticUI/Button.hs:175:1-66: Splicing declarations
animatedButton_hiddenContent ::
  forall t_a1Me1 m_a1Me4.
  Lens' (AnimatedButton t_a1Me1 m_a1Me4) (m_a1Me4 ())
animatedButton_hiddenContent
  f_a1Nxh
  (AnimatedButton x1_a1Nxj x2_a1Nxk)
  = fmap
      (\ y1_a1Nxm -> AnimatedButton x1_a1Nxj y1_a1Nxm) (f_a1Nxh x2_a1Nxk)
{-# INLINE animatedButton_hiddenContent #-}
animatedButton_type ::
  forall t_a1Me1 m_a1Me4.
  Lens' (AnimatedButton t_a1Me1 m_a1Me4) (Active t_a1Me1 AnimatedButtonType)
animatedButton_type f_a1Nxv (AnimatedButton x1_a1Nxx x2_a1Nxy)
  = fmap
      (\ y1_a1NxA -> AnimatedButton y1_a1NxA x2_a1Nxy) (f_a1Nxv x1_a1Nxx)
{-# INLINE animatedButton_type #-}
-- src/Reflex/Dom/SemanticUI/Button.hs:226:1-64: Splicing declarations
buttonConfig_animated ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Maybe (AnimatedButton t_a1NM1 m_a1NM5))
buttonConfig_animated
  f_a1OBW
  (ButtonConfig x1_a1OBY
                x2_a1OBZ
                x3_a1OC0
                x4_a1OC1
                x5_a1OC2
                x6_a1OC3
                x7_a1OC4
                x8_a1OC5
                x9_a1OC7
                x10_a1OC9
                x11_a1OCa
                x12_a1OCc
                x13_a1OCd
                x14_a1OCe
                x15_a1OCf
                x16_a1OCg
                x17_a1OCh
                x18_a1OCi
                x19_a1OCk)
  = fmap
      (\ y1_a1OCl
         -> ButtonConfig
              x1_a1OBY
              x2_a1OBZ
              x3_a1OC0
              x4_a1OC1
              x5_a1OC2
              x6_a1OC3
              x7_a1OC4
              x8_a1OC5
              x9_a1OC7
              x10_a1OC9
              x11_a1OCa
              x12_a1OCc
              x13_a1OCd
              x14_a1OCe
              x15_a1OCf
              x16_a1OCg
              y1_a1OCl
              x18_a1OCi
              x19_a1OCk)
      (f_a1OBW x17_a1OCh)
{-# INLINE buttonConfig_animated #-}
buttonConfig_attached ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 (Maybe ExclusiveAttached))
buttonConfig_attached
  f_a1OCw
  (ButtonConfig x1_a1OCz
                x2_a1OCA
                x3_a1OCB
                x4_a1OCC
                x5_a1OCD
                x6_a1OCE
                x7_a1OCG
                x8_a1OCH
                x9_a1OCI
                x10_a1OCJ
                x11_a1OCK
                x12_a1OCM
                x13_a1OCN
                x14_a1OCO
                x15_a1OCP
                x16_a1OCR
                x17_a1OCS
                x18_a1OCT
                x19_a1OCV)
  = fmap
      (\ y1_a1OCW
         -> ButtonConfig
              x1_a1OCz
              x2_a1OCA
              x3_a1OCB
              x4_a1OCC
              x5_a1OCD
              x6_a1OCE
              x7_a1OCG
              x8_a1OCH
              x9_a1OCI
              x10_a1OCJ
              x11_a1OCK
              x12_a1OCM
              x13_a1OCN
              x14_a1OCO
              x15_a1OCP
              y1_a1OCW
              x17_a1OCS
              x18_a1OCT
              x19_a1OCV)
      (f_a1OCw x16_a1OCR)
{-# INLINE buttonConfig_attached #-}
buttonConfig_basic ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 Bool)
buttonConfig_basic
  f_a1OD4
  (ButtonConfig x1_a1OD5
                x2_a1OD6
                x3_a1OD8
                x4_a1OD9
                x5_a1ODa
                x6_a1ODc
                x7_a1ODe
                x8_a1ODf
                x9_a1ODg
                x10_a1ODh
                x11_a1ODi
                x12_a1ODj
                x13_a1ODk
                x14_a1ODl
                x15_a1ODm
                x16_a1ODo
                x17_a1ODp
                x18_a1ODr
                x19_a1ODs)
  = fmap
      (\ y1_a1ODu
         -> ButtonConfig
              x1_a1OD5
              x2_a1OD6
              y1_a1ODu
              x4_a1OD9
              x5_a1ODa
              x6_a1ODc
              x7_a1ODe
              x8_a1ODf
              x9_a1ODg
              x10_a1ODh
              x11_a1ODi
              x12_a1ODj
              x13_a1ODk
              x14_a1ODl
              x15_a1ODm
              x16_a1ODo
              x17_a1ODp
              x18_a1ODr
              x19_a1ODs)
      (f_a1OD4 x3_a1OD8)
{-# INLINE buttonConfig_basic #-}
buttonConfig_circular ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 Bool)
buttonConfig_circular
  f_a1ODB
  (ButtonConfig x1_a1ODD
                x2_a1ODF
                x3_a1ODG
                x4_a1ODH
                x5_a1ODI
                x6_a1ODJ
                x7_a1ODK
                x8_a1ODL
                x9_a1ODM
                x10_a1ODN
                x11_a1ODO
                x12_a1ODQ
                x13_a1ODS
                x14_a1ODU
                x15_a1ODV
                x16_a1ODW
                x17_a1ODX
                x18_a1ODY
                x19_a1ODZ)
  = fmap
      (\ y1_a1OE0
         -> ButtonConfig
              x1_a1ODD
              x2_a1ODF
              x3_a1ODG
              x4_a1ODH
              x5_a1ODI
              x6_a1ODJ
              x7_a1ODK
              y1_a1OE0
              x9_a1ODM
              x10_a1ODN
              x11_a1ODO
              x12_a1ODQ
              x13_a1ODS
              x14_a1ODU
              x15_a1ODV
              x16_a1ODW
              x17_a1ODX
              x18_a1ODY
              x19_a1ODZ)
      (f_a1ODB x8_a1ODL)
{-# INLINE buttonConfig_circular #-}
buttonConfig_color ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 (Maybe Color))
buttonConfig_color
  f_a1OE9
  (ButtonConfig x1_a1OEa
                x2_a1OEb
                x3_a1OEc
                x4_a1OEe
                x5_a1OEf
                x6_a1OEg
                x7_a1OEh
                x8_a1OEi
                x9_a1OEj
                x10_a1OEk
                x11_a1OEm
                x12_a1OEo
                x13_a1OEp
                x14_a1OEq
                x15_a1OEr
                x16_a1OEs
                x17_a1OEu
                x18_a1OEv
                x19_a1OEw)
  = fmap
      (\ y1_a1OEx
         -> ButtonConfig
              x1_a1OEa
              x2_a1OEb
              x3_a1OEc
              x4_a1OEe
              x5_a1OEf
              x6_a1OEg
              x7_a1OEh
              x8_a1OEi
              y1_a1OEx
              x10_a1OEk
              x11_a1OEm
              x12_a1OEo
              x13_a1OEp
              x14_a1OEq
              x15_a1OEr
              x16_a1OEs
              x17_a1OEu
              x18_a1OEv
              x19_a1OEw)
      (f_a1OE9 x9_a1OEj)
{-# INLINE buttonConfig_color #-}
buttonConfig_compact ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 Bool)
buttonConfig_compact
  f_a1OEG
  (ButtonConfig x1_a1OEH
                x2_a1OEI
                x3_a1OEJ
                x4_a1OEK
                x5_a1OEL
                x6_a1OEM
                x7_a1OEO
                x8_a1OEP
                x9_a1OES
                x10_a1OET
                x11_a1OEU
                x12_a1OEV
                x13_a1OEW
                x14_a1OEX
                x15_a1OEY
                x16_a1OEZ
                x17_a1OF0
                x18_a1OF1
                x19_a1OF3)
  = fmap
      (\ y1_a1OF5
         -> ButtonConfig
              x1_a1OEH
              y1_a1OF5
              x3_a1OEJ
              x4_a1OEK
              x5_a1OEL
              x6_a1OEM
              x7_a1OEO
              x8_a1OEP
              x9_a1OES
              x10_a1OET
              x11_a1OEU
              x12_a1OEV
              x13_a1OEW
              x14_a1OEX
              x15_a1OEY
              x16_a1OEZ
              x17_a1OF0
              x18_a1OF1
              x19_a1OF3)
      (f_a1OEG x2_a1OEI)
{-# INLINE buttonConfig_compact #-}
buttonConfig_disabled ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 Bool)
buttonConfig_disabled
  f_a1OFc
  (ButtonConfig x1_a1OFe
                x2_a1OFf
                x3_a1OFh
                x4_a1OFi
                x5_a1OFj
                x6_a1OFk
                x7_a1OFm
                x8_a1OFn
                x9_a1OFo
                x10_a1OFp
                x11_a1OFq
                x12_a1OFr
                x13_a1OFs
                x14_a1OFt
                x15_a1OFw
                x16_a1OFx
                x17_a1OFy
                x18_a1OFz
                x19_a1OFB)
  = fmap
      (\ y1_a1OFC
         -> ButtonConfig
              y1_a1OFC
              x2_a1OFf
              x3_a1OFh
              x4_a1OFi
              x5_a1OFj
              x6_a1OFk
              x7_a1OFm
              x8_a1OFn
              x9_a1OFo
              x10_a1OFp
              x11_a1OFq
              x12_a1OFr
              x13_a1OFs
              x14_a1OFt
              x15_a1OFw
              x16_a1OFx
              x17_a1OFy
              x18_a1OFz
              x19_a1OFB)
      (f_a1OFc x1_a1OFe)
{-# INLINE buttonConfig_disabled #-}
buttonConfig_elConfig ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (ActiveElConfig t_a1NM1)
buttonConfig_elConfig
  f_a1OFK
  (ButtonConfig x1_a1OFL
                x2_a1OFM
                x3_a1OFN
                x4_a1OFO
                x5_a1OFQ
                x6_a1OFS
                x7_a1OFT
                x8_a1OFU
                x9_a1OFV
                x10_a1OFW
                x11_a1OFY
                x12_a1OFZ
                x13_a1OG0
                x14_a1OG1
                x15_a1OG2
                x16_a1OG3
                x17_a1OG5
                x18_a1OG6
                x19_a1OG7)
  = fmap
      (\ y1_a1OG9
         -> ButtonConfig
              x1_a1OFL
              x2_a1OFM
              x3_a1OFN
              x4_a1OFO
              x5_a1OFQ
              x6_a1OFS
              x7_a1OFT
              x8_a1OFU
              x9_a1OFV
              x10_a1OFW
              x11_a1OFY
              x12_a1OFZ
              x13_a1OG0
              x14_a1OG1
              x15_a1OG2
              x16_a1OG3
              x17_a1OG5
              x18_a1OG6
              y1_a1OG9)
      (f_a1OFK x19_a1OG7)
{-# INLINE buttonConfig_elConfig #-}
buttonConfig_emphasis ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 (Maybe Emphasis))
buttonConfig_emphasis
  f_a1OGh
  (ButtonConfig x1_a1OGi
                x2_a1OGj
                x3_a1OGl
                x4_a1OGm
                x5_a1OGn
                x6_a1OGo
                x7_a1OGp
                x8_a1OGr
                x9_a1OGs
                x10_a1OGt
                x11_a1OGu
                x12_a1OGw
                x13_a1OGx
                x14_a1OGy
                x15_a1OGA
                x16_a1OGB
                x17_a1OGC
                x18_a1OGD
                x19_a1OGF)
  = fmap
      (\ y1_a1OGG
         -> ButtonConfig
              x1_a1OGi
              x2_a1OGj
              x3_a1OGl
              x4_a1OGm
              x5_a1OGn
              x6_a1OGo
              x7_a1OGp
              x8_a1OGr
              x9_a1OGs
              x10_a1OGt
              y1_a1OGG
              x12_a1OGw
              x13_a1OGx
              x14_a1OGy
              x15_a1OGA
              x16_a1OGB
              x17_a1OGC
              x18_a1OGD
              x19_a1OGF)
      (f_a1OGh x11_a1OGu)
{-# INLINE buttonConfig_emphasis #-}
buttonConfig_floated ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 (Maybe Floated))
buttonConfig_floated
  f_a1OGN
  (ButtonConfig x1_a1OGP
                x2_a1OGQ
                x3_a1OGR
                x4_a1OGU
                x5_a1OGV
                x6_a1OGW
                x7_a1OGX
                x8_a1OGY
                x9_a1OGZ
                x10_a1OH0
                x11_a1OH1
                x12_a1OH2
                x13_a1OH4
                x14_a1OH5
                x15_a1OH7
                x16_a1OH8
                x17_a1OHa
                x18_a1OHb
                x19_a1OHc)
  = fmap
      (\ y1_a1OHd
         -> ButtonConfig
              x1_a1OGP
              x2_a1OGQ
              x3_a1OGR
              x4_a1OGU
              x5_a1OGV
              x6_a1OGW
              x7_a1OGX
              x8_a1OGY
              x9_a1OGZ
              x10_a1OH0
              x11_a1OH1
              x12_a1OH2
              x13_a1OH4
              y1_a1OHd
              x15_a1OH7
              x16_a1OH8
              x17_a1OHa
              x18_a1OHb
              x19_a1OHc)
      (f_a1OGN x14_a1OH5)
{-# INLINE buttonConfig_floated #-}
buttonConfig_fluid ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 Bool)
buttonConfig_fluid
  f_a1OHm
  (ButtonConfig x1_a1OHn
                x2_a1OHo
                x3_a1OHp
                x4_a1OHq
                x5_a1OHr
                x6_a1OHs
                x7_a1OHt
                x8_a1OHu
                x9_a1OHw
                x10_a1OHx
                x11_a1OHA
                x12_a1OHB
                x13_a1OHC
                x14_a1OHD
                x15_a1OHE
                x16_a1OHF
                x17_a1OHG
                x18_a1OHH
                x19_a1OHI)
  = fmap
      (\ y1_a1OHK
         -> ButtonConfig
              x1_a1OHn
              x2_a1OHo
              x3_a1OHp
              x4_a1OHq
              x5_a1OHr
              x6_a1OHs
              y1_a1OHK
              x8_a1OHu
              x9_a1OHw
              x10_a1OHx
              x11_a1OHA
              x12_a1OHB
              x13_a1OHC
              x14_a1OHD
              x15_a1OHE
              x16_a1OHF
              x17_a1OHG
              x18_a1OHH
              x19_a1OHI)
      (f_a1OHm x7_a1OHt)
{-# INLINE buttonConfig_fluid #-}
buttonConfig_icon ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 Bool)
buttonConfig_icon
  f_a1OHS
  (ButtonConfig x1_a1OHU
                x2_a1OHV
                x3_a1OHW
                x4_a1OHX
                x5_a1OHY
                x6_a1OI0
                x7_a1OI1
                x8_a1OI2
                x9_a1OI3
                x10_a1OI5
                x11_a1OI6
                x12_a1OI7
                x13_a1OI8
                x14_a1OI9
                x15_a1OIb
                x16_a1OId
                x17_a1OIe
                x18_a1OIf
                x19_a1OIg)
  = fmap
      (\ y1_a1OIi
         -> ButtonConfig
              x1_a1OHU
              x2_a1OHV
              x3_a1OHW
              y1_a1OIi
              x5_a1OHY
              x6_a1OI0
              x7_a1OI1
              x8_a1OI2
              x9_a1OI3
              x10_a1OI5
              x11_a1OI6
              x12_a1OI7
              x13_a1OI8
              x14_a1OI9
              x15_a1OIb
              x16_a1OId
              x17_a1OIe
              x18_a1OIf
              x19_a1OIg)
      (f_a1OHS x4_a1OHX)
{-# INLINE buttonConfig_icon #-}
buttonConfig_inverted ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 Bool)
buttonConfig_inverted
  f_a1OIp
  (ButtonConfig x1_a1OIr
                x2_a1OIs
                x3_a1OIt
                x4_a1OIu
                x5_a1OIv
                x6_a1OIx
                x7_a1OIz
                x8_a1OIA
                x9_a1OIB
                x10_a1OIC
                x11_a1OIE
                x12_a1OIF
                x13_a1OIG
                x14_a1OIH
                x15_a1OII
                x16_a1OIJ
                x17_a1OIK
                x18_a1OIL
                x19_a1OIO)
  = fmap
      (\ y1_a1OIQ
         -> ButtonConfig
              x1_a1OIr
              x2_a1OIs
              x3_a1OIt
              x4_a1OIu
              y1_a1OIQ
              x6_a1OIx
              x7_a1OIz
              x8_a1OIA
              x9_a1OIB
              x10_a1OIC
              x11_a1OIE
              x12_a1OIF
              x13_a1OIG
              x14_a1OIH
              x15_a1OII
              x16_a1OIJ
              x17_a1OIK
              x18_a1OIL
              x19_a1OIO)
      (f_a1OIp x5_a1OIv)
{-# INLINE buttonConfig_inverted #-}
buttonConfig_labeledIcon ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 (Maybe Labeled))
buttonConfig_labeledIcon
  f_a1OIX
  (ButtonConfig x1_a1OIZ
                x2_a1OJ1
                x3_a1OJ2
                x4_a1OJ3
                x5_a1OJ4
                x6_a1OJ5
                x7_a1OJ7
                x8_a1OJ8
                x9_a1OJ9
                x10_a1OJa
                x11_a1OJb
                x12_a1OJc
                x13_a1OJf
                x14_a1OJg
                x15_a1OJh
                x16_a1OJi
                x17_a1OJj
                x18_a1OJk
                x19_a1OJl)
  = fmap
      (\ y1_a1OJn
         -> ButtonConfig
              x1_a1OIZ
              x2_a1OJ1
              x3_a1OJ2
              x4_a1OJ3
              x5_a1OJ4
              x6_a1OJ5
              x7_a1OJ7
              x8_a1OJ8
              x9_a1OJ9
              x10_a1OJa
              x11_a1OJb
              x12_a1OJc
              x13_a1OJf
              x14_a1OJg
              y1_a1OJn
              x16_a1OJi
              x17_a1OJj
              x18_a1OJk
              x19_a1OJl)
      (f_a1OIX x15_a1OJh)
{-# INLINE buttonConfig_labeledIcon #-}
buttonConfig_loading ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 Bool)
buttonConfig_loading
  f_a1OJw
  (ButtonConfig x1_a1OJx
                x2_a1OJy
                x3_a1OJz
                x4_a1OJA
                x5_a1OJB
                x6_a1OJC
                x7_a1OJD
                x8_a1OJE
                x9_a1OJF
                x10_a1OJG
                x11_a1OJH
                x12_a1OJK
                x13_a1OJL
                x14_a1OJM
                x15_a1OJN
                x16_a1OJO
                x17_a1OJP
                x18_a1OJQ
                x19_a1OJR)
  = fmap
      (\ y1_a1OJS
         -> ButtonConfig
              x1_a1OJx
              x2_a1OJy
              x3_a1OJz
              x4_a1OJA
              x5_a1OJB
              y1_a1OJS
              x7_a1OJD
              x8_a1OJE
              x9_a1OJF
              x10_a1OJG
              x11_a1OJH
              x12_a1OJK
              x13_a1OJL
              x14_a1OJM
              x15_a1OJN
              x16_a1OJO
              x17_a1OJP
              x18_a1OJQ
              x19_a1OJR)
      (f_a1OJw x6_a1OJC)
{-# INLINE buttonConfig_loading #-}
buttonConfig_positive ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 (Maybe Positive))
buttonConfig_positive
  f_a1OJY
  (ButtonConfig x1_a1OJZ
                x2_a1OK0
                x3_a1OK1
                x4_a1OK3
                x5_a1OK4
                x6_a1OK5
                x7_a1OK6
                x8_a1OK7
                x9_a1OK8
                x10_a1OKa
                x11_a1OKb
                x12_a1OKc
                x13_a1OKd
                x14_a1OKe
                x15_a1OKf
                x16_a1OKg
                x17_a1OKi
                x18_a1OKj
                x19_a1OKk)
  = fmap
      (\ y1_a1OKl
         -> ButtonConfig
              x1_a1OJZ
              x2_a1OK0
              x3_a1OK1
              x4_a1OK3
              x5_a1OK4
              x6_a1OK5
              x7_a1OK6
              x8_a1OK7
              x9_a1OK8
              x10_a1OKa
              x11_a1OKb
              y1_a1OKl
              x13_a1OKd
              x14_a1OKe
              x15_a1OKf
              x16_a1OKg
              x17_a1OKi
              x18_a1OKj
              x19_a1OKk)
      (f_a1OJY x12_a1OKc)
{-# INLINE buttonConfig_positive #-}
buttonConfig_size ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 (Maybe Size))
buttonConfig_size
  f_a1OKr
  (ButtonConfig x1_a1OKs
                x2_a1OKt
                x3_a1OKu
                x4_a1OKv
                x5_a1OKw
                x6_a1OKx
                x7_a1OKy
                x8_a1OKz
                x9_a1OKC
                x10_a1OKD
                x11_a1OKE
                x12_a1OKF
                x13_a1OKG
                x14_a1OKH
                x15_a1OKI
                x16_a1OKJ
                x17_a1OKK
                x18_a1OKL
                x19_a1OKM)
  = fmap
      (\ y1_a1OKO
         -> ButtonConfig
              x1_a1OKs
              x2_a1OKt
              x3_a1OKu
              x4_a1OKv
              x5_a1OKw
              x6_a1OKx
              x7_a1OKy
              x8_a1OKz
              x9_a1OKC
              y1_a1OKO
              x11_a1OKE
              x12_a1OKF
              x13_a1OKG
              x14_a1OKH
              x15_a1OKI
              x16_a1OKJ
              x17_a1OKK
              x18_a1OKL
              x19_a1OKM)
      (f_a1OKr x10_a1OKD)
{-# INLINE buttonConfig_size #-}
buttonConfig_social ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) (Active t_a1NM1 (Maybe Social))
buttonConfig_social
  f_a1OKT
  (ButtonConfig x1_a1OKV
                x2_a1OKW
                x3_a1OKX
                x4_a1OKY
                x5_a1OKZ
                x6_a1OL1
                x7_a1OL2
                x8_a1OL3
                x9_a1OL4
                x10_a1OL5
                x11_a1OL6
                x12_a1OL7
                x13_a1OL9
                x14_a1OLa
                x15_a1OLb
                x16_a1OLe
                x17_a1OLg
                x18_a1OLi
                x19_a1OLk)
  = fmap
      (\ y1_a1OLm
         -> ButtonConfig
              x1_a1OKV
              x2_a1OKW
              x3_a1OKX
              x4_a1OKY
              x5_a1OKZ
              x6_a1OL1
              x7_a1OL2
              x8_a1OL3
              x9_a1OL4
              x10_a1OL5
              x11_a1OL6
              x12_a1OL7
              y1_a1OLm
              x14_a1OLa
              x15_a1OLb
              x16_a1OLe
              x17_a1OLg
              x18_a1OLi
              x19_a1OLk)
      (f_a1OKT x13_a1OL9)
{-# INLINE buttonConfig_social #-}
buttonConfig_type ::
  forall t_a1NM1 m_a1NM5.
  Lens' (ButtonConfig t_a1NM1 m_a1NM5) ButtonType
buttonConfig_type
  f_a1OLS
  (ButtonConfig x1_a1OLU
                x2_a1OLX
                x3_a1OM0
                x4_a1OM2
                x5_a1OM4
                x6_a1OM6
                x7_a1OM8
                x8_a1OMa
                x9_a1OMc
                x10_a1OMe
                x11_a1OMg
                x12_a1OMj
                x13_a1OMl
                x14_a1OMm
                x15_a1OMp
                x16_a1OMs
                x17_a1OMt
                x18_a1OMv
                x19_a1OMx)
  = fmap
      (\ y1_a1OMA
         -> ButtonConfig
              x1_a1OLU
              x2_a1OLX
              x3_a1OM0
              x4_a1OM2
              x5_a1OM4
              x6_a1OM6
              x7_a1OM8
              x8_a1OMa
              x9_a1OMc
              x10_a1OMe
              x11_a1OMg
              x12_a1OMj
              x13_a1OMl
              x14_a1OMm
              x15_a1OMp
              x16_a1OMs
              x17_a1OMt
              y1_a1OMA
              x19_a1OMx)
      (f_a1OLS x18_a1OMv)
{-# INLINE buttonConfig_type #-}
