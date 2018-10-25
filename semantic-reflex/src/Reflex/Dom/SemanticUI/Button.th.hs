-- src/Reflex/Dom/SemanticUI/Button.hs:96:1-65: Splicing declarations
buttonsConfig_attached ::
  forall t_aTlw.
  Lens' (ButtonsConfig t_aTlw) (Active t_aTlw (Maybe VerticalAttached))
buttonsConfig_attached
  f_aTwM
  (ButtonsConfig x1_aTwN
                 x2_aTwO
                 x3_aTwP
                 x4_aTwQ
                 x5_aTwR
                 x6_aTwS
                 x7_aTwT
                 x8_aTwU
                 x9_aTwV
                 x10_aTwX
                 x11_aTwY)
  = fmap
      (\ y1_aTx0
         -> ButtonsConfig
              x1_aTwN
              x2_aTwO
              x3_aTwP
              x4_aTwQ
              x5_aTwR
              x6_aTwS
              x7_aTwT
              y1_aTx0
              x9_aTwV
              x10_aTwX
              x11_aTwY)
      (f_aTwM x8_aTwU)
{-# INLINE buttonsConfig_attached #-}
buttonsConfig_basic ::
  forall t_aTlw. Lens' (ButtonsConfig t_aTlw) (Active t_aTlw Bool)
buttonsConfig_basic
  f_aTx3
  (ButtonsConfig x1_aTx4
                 x2_aTx5
                 x3_aTx6
                 x4_aTx7
                 x5_aTx8
                 x6_aTxb
                 x7_aTxc
                 x8_aTxd
                 x9_aTxe
                 x10_aTxf
                 x11_aTxg)
  = fmap
      (\ y1_aTxh
         -> ButtonsConfig
              y1_aTxh
              x2_aTx5
              x3_aTx6
              x4_aTx7
              x5_aTx8
              x6_aTxb
              x7_aTxc
              x8_aTxd
              x9_aTxe
              x10_aTxf
              x11_aTxg)
      (f_aTx3 x1_aTx4)
{-# INLINE buttonsConfig_basic #-}
buttonsConfig_color ::
  forall t_aTlw.
  Lens' (ButtonsConfig t_aTlw) (Active t_aTlw (Maybe Color))
buttonsConfig_color
  f_aTxk
  (ButtonsConfig x1_aTxl
                 x2_aTxm
                 x3_aTxn
                 x4_aTxo
                 x5_aTxp
                 x6_aTxq
                 x7_aTxr
                 x8_aTxs
                 x9_aTxt
                 x10_aTxu
                 x11_aTxv)
  = fmap
      (\ y1_aTxx
         -> ButtonsConfig
              x1_aTxl
              x2_aTxm
              x3_aTxn
              x4_aTxo
              x5_aTxp
              y1_aTxx
              x7_aTxr
              x8_aTxs
              x9_aTxt
              x10_aTxu
              x11_aTxv)
      (f_aTxk x6_aTxq)
{-# INLINE buttonsConfig_color #-}
buttonsConfig_compact ::
  forall t_aTlw. Lens' (ButtonsConfig t_aTlw) (Active t_aTlw Bool)
buttonsConfig_compact
  f_aTxD
  (ButtonsConfig x1_aTxE
                 x2_aTxF
                 x3_aTxG
                 x4_aTxH
                 x5_aTxI
                 x6_aTxJ
                 x7_aTxK
                 x8_aTxL
                 x9_aTxM
                 x10_aTxN
                 x11_aTxO)
  = fmap
      (\ y1_aTxP
         -> ButtonsConfig
              x1_aTxE
              x2_aTxF
              x3_aTxG
              x4_aTxH
              y1_aTxP
              x6_aTxJ
              x7_aTxK
              x8_aTxL
              x9_aTxM
              x10_aTxN
              x11_aTxO)
      (f_aTxD x5_aTxI)
{-# INLINE buttonsConfig_compact #-}
buttonsConfig_elConfig ::
  forall t_aTlw. Lens' (ButtonsConfig t_aTlw) (ActiveElConfig t_aTlw)
buttonsConfig_elConfig
  f_aTxS
  (ButtonsConfig x1_aTxT
                 x2_aTxU
                 x3_aTxV
                 x4_aTxW
                 x5_aTxZ
                 x6_aTy0
                 x7_aTy1
                 x8_aTy2
                 x9_aTy3
                 x10_aTy4
                 x11_aTy5)
  = fmap
      (\ y1_aTy6
         -> ButtonsConfig
              x1_aTxT
              x2_aTxU
              x3_aTxV
              x4_aTxW
              x5_aTxZ
              x6_aTy0
              x7_aTy1
              x8_aTy2
              x9_aTy3
              x10_aTy4
              y1_aTy6)
      (f_aTxS x11_aTy5)
{-# INLINE buttonsConfig_elConfig #-}
buttonsConfig_floated ::
  forall t_aTlw.
  Lens' (ButtonsConfig t_aTlw) (Active t_aTlw (Maybe Floated))
buttonsConfig_floated
  f_aTy9
  (ButtonsConfig x1_aTya
                 x2_aTyb
                 x3_aTyc
                 x4_aTyd
                 x5_aTye
                 x6_aTyf
                 x7_aTyg
                 x8_aTyh
                 x9_aTyj
                 x10_aTyk
                 x11_aTym)
  = fmap
      (\ y1_aTyn
         -> ButtonsConfig
              x1_aTya
              x2_aTyb
              x3_aTyc
              x4_aTyd
              x5_aTye
              x6_aTyf
              x7_aTyg
              x8_aTyh
              x9_aTyj
              y1_aTyn
              x11_aTym)
      (f_aTy9 x10_aTyk)
{-# INLINE buttonsConfig_floated #-}
buttonsConfig_icon ::
  forall t_aTlw. Lens' (ButtonsConfig t_aTlw) (Active t_aTlw Bool)
buttonsConfig_icon
  f_aTyp
  (ButtonsConfig x1_aTyq
                 x2_aTyr
                 x3_aTys
                 x4_aTyt
                 x5_aTyu
                 x6_aTyv
                 x7_aTyw
                 x8_aTyx
                 x9_aTyy
                 x10_aTyz
                 x11_aTyA)
  = fmap
      (\ y1_aTyB
         -> ButtonsConfig
              x1_aTyq
              y1_aTyB
              x3_aTys
              x4_aTyt
              x5_aTyu
              x6_aTyv
              x7_aTyw
              x8_aTyx
              x9_aTyy
              x10_aTyz
              x11_aTyA)
      (f_aTyp x2_aTyr)
{-# INLINE buttonsConfig_icon #-}
buttonsConfig_labeledIcon ::
  forall t_aTlw. Lens' (ButtonsConfig t_aTlw) (Active t_aTlw Bool)
buttonsConfig_labeledIcon
  f_aTyE
  (ButtonsConfig x1_aTyF
                 x2_aTyG
                 x3_aTyH
                 x4_aTyI
                 x5_aTyJ
                 x6_aTyK
                 x7_aTyL
                 x8_aTyM
                 x9_aTyN
                 x10_aTyO
                 x11_aTyP)
  = fmap
      (\ y1_aTyQ
         -> ButtonsConfig
              x1_aTyF
              x2_aTyG
              y1_aTyQ
              x4_aTyI
              x5_aTyJ
              x6_aTyK
              x7_aTyL
              x8_aTyM
              x9_aTyN
              x10_aTyO
              x11_aTyP)
      (f_aTyE x3_aTyH)
{-# INLINE buttonsConfig_labeledIcon #-}
buttonsConfig_size ::
  forall t_aTlw.
  Lens' (ButtonsConfig t_aTlw) (Active t_aTlw (Maybe Size))
buttonsConfig_size
  f_aTyU
  (ButtonsConfig x1_aTyV
                 x2_aTyW
                 x3_aTyX
                 x4_aTyY
                 x5_aTyZ
                 x6_aTz0
                 x7_aTz1
                 x8_aTz2
                 x9_aTz3
                 x10_aTz4
                 x11_aTz5)
  = fmap
      (\ y1_aTz6
         -> ButtonsConfig
              x1_aTyV
              x2_aTyW
              x3_aTyX
              x4_aTyY
              x5_aTyZ
              x6_aTz0
              y1_aTz6
              x8_aTz2
              x9_aTz3
              x10_aTz4
              x11_aTz5)
      (f_aTyU x7_aTz1)
{-# INLINE buttonsConfig_size #-}
buttonsConfig_vertical ::
  forall t_aTlw. Lens' (ButtonsConfig t_aTlw) (Active t_aTlw Bool)
buttonsConfig_vertical
  f_aTz8
  (ButtonsConfig x1_aTz9
                 x2_aTza
                 x3_aTzb
                 x4_aTzc
                 x5_aTzd
                 x6_aTze
                 x7_aTzf
                 x8_aTzg
                 x9_aTzh
                 x10_aTzi
                 x11_aTzj)
  = fmap
      (\ y1_aTzk
         -> ButtonsConfig
              x1_aTz9
              x2_aTza
              x3_aTzb
              y1_aTzk
              x5_aTzd
              x6_aTze
              x7_aTzf
              x8_aTzg
              x9_aTzh
              x10_aTzi
              x11_aTzj)
      (f_aTz8 x4_aTzc)
{-# INLINE buttonsConfig_vertical #-}
buttonsConfig_width ::
  forall t_aTlw.
  Lens' (ButtonsConfig t_aTlw) (Active t_aTlw (Maybe Width))
buttonsConfig_width
  f_aTzo
  (ButtonsConfig x1_aTzp
                 x2_aTzq
                 x3_aTzr
                 x4_aTzs
                 x5_aTzt
                 x6_aTzu
                 x7_aTzv
                 x8_aTzw
                 x9_aTzx
                 x10_aTzy
                 x11_aTzz)
  = fmap
      (\ y1_aTzA
         -> ButtonsConfig
              x1_aTzp
              x2_aTzq
              x3_aTzr
              x4_aTzs
              x5_aTzt
              x6_aTzu
              x7_aTzv
              x8_aTzw
              y1_aTzA
              x10_aTzy
              x11_aTzz)
      (f_aTzo x9_aTzx)
{-# INLINE buttonsConfig_width #-}
-- src/Reflex/Dom/SemanticUI/Button.hs:142:1-71: Splicing declarations
labeledButtonConfig_elConfig ::
  forall t_aTNd.
  Lens' (LabeledButtonConfig t_aTNd) (ActiveElConfig t_aTNd)
labeledButtonConfig_elConfig
  f_aUuX
  (LabeledButtonConfig x1_aUuY x2_aUuZ)
  = fmap
      (\ y1_aUv0 -> LabeledButtonConfig x1_aUuY y1_aUv0) (f_aUuX x2_aUuZ)
{-# INLINE labeledButtonConfig_elConfig #-}
labeledButtonConfig_side ::
  forall t_aTNd.
  Lens' (LabeledButtonConfig t_aTNd) (Active t_aTNd Labeled)
labeledButtonConfig_side
  f_aUv4
  (LabeledButtonConfig x1_aUv5 x2_aUv6)
  = fmap
      (\ y1_aUv7 -> LabeledButtonConfig y1_aUv7 x2_aUv6) (f_aUv4 x1_aUv5)
{-# INLINE labeledButtonConfig_side #-}
-- src/Reflex/Dom/SemanticUI/Button.hs:175:1-66: Splicing declarations
animatedButton_hiddenContent ::
  forall t_aUvT m_aUvU.
  Lens' (AnimatedButton t_aUvT m_aUvU) (m_aUvU ())
animatedButton_hiddenContent
  f_aV2y
  (AnimatedButton x1_aV2z x2_aV2A)
  = fmap
      (\ y1_aV2B -> AnimatedButton x1_aV2z y1_aV2B) (f_aV2y x2_aV2A)
{-# INLINE animatedButton_hiddenContent #-}
animatedButton_type ::
  forall t_aUvT m_aUvU.
  Lens' (AnimatedButton t_aUvT m_aUvU) (Active t_aUvT AnimatedButtonType)
animatedButton_type f_aV2C (AnimatedButton x1_aV2D x2_aV2E)
  = fmap
      (\ y1_aV2F -> AnimatedButton y1_aV2F x2_aV2E) (f_aV2C x1_aV2D)
{-# INLINE animatedButton_type #-}
-- src/Reflex/Dom/SemanticUI/Button.hs:226:1-64: Splicing declarations
buttonConfig_animated ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Maybe (AnimatedButton t_aVus m_aVut))
buttonConfig_animated
  f_aVI4
  (ButtonConfig x1_aVI5
                x2_aVI6
                x3_aVI7
                x4_aVI8
                x5_aVI9
                x6_aVIa
                x7_aVIb
                x8_aVIc
                x9_aVId
                x10_aVIe
                x11_aVIf
                x12_aVIg
                x13_aVIh
                x14_aVIi
                x15_aVIj
                x16_aVIk
                x17_aVIl
                x18_aVIm
                x19_aVIn)
  = fmap
      (\ y1_aVIo
         -> ButtonConfig
              x1_aVI5
              x2_aVI6
              x3_aVI7
              x4_aVI8
              x5_aVI9
              x6_aVIa
              x7_aVIb
              x8_aVIc
              x9_aVId
              x10_aVIe
              x11_aVIf
              x12_aVIg
              x13_aVIh
              x14_aVIi
              x15_aVIj
              x16_aVIk
              y1_aVIo
              x18_aVIm
              x19_aVIn)
      (f_aVI4 x17_aVIl)
{-# INLINE buttonConfig_animated #-}
buttonConfig_attached ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus (Maybe ExclusiveAttached))
buttonConfig_attached
  f_aVIp
  (ButtonConfig x1_aVIq
                x2_aVIr
                x3_aVIs
                x4_aVIt
                x5_aVIu
                x6_aVIv
                x7_aVIw
                x8_aVIx
                x9_aVIy
                x10_aVIz
                x11_aVIA
                x12_aVIB
                x13_aVIC
                x14_aVID
                x15_aVIE
                x16_aVIF
                x17_aVIG
                x18_aVIH
                x19_aVII)
  = fmap
      (\ y1_aVIJ
         -> ButtonConfig
              x1_aVIq
              x2_aVIr
              x3_aVIs
              x4_aVIt
              x5_aVIu
              x6_aVIv
              x7_aVIw
              x8_aVIx
              x9_aVIy
              x10_aVIz
              x11_aVIA
              x12_aVIB
              x13_aVIC
              x14_aVID
              x15_aVIE
              y1_aVIJ
              x17_aVIG
              x18_aVIH
              x19_aVII)
      (f_aVIp x16_aVIF)
{-# INLINE buttonConfig_attached #-}
buttonConfig_basic ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus Bool)
buttonConfig_basic
  f_aVIK
  (ButtonConfig x1_aVIL
                x2_aVIM
                x3_aVIN
                x4_aVIO
                x5_aVIP
                x6_aVIQ
                x7_aVIR
                x8_aVIS
                x9_aVIT
                x10_aVIU
                x11_aVIV
                x12_aVIW
                x13_aVIX
                x14_aVIY
                x15_aVIZ
                x16_aVJ0
                x17_aVJ1
                x18_aVJ2
                x19_aVJ3)
  = fmap
      (\ y1_aVJ4
         -> ButtonConfig
              x1_aVIL
              x2_aVIM
              y1_aVJ4
              x4_aVIO
              x5_aVIP
              x6_aVIQ
              x7_aVIR
              x8_aVIS
              x9_aVIT
              x10_aVIU
              x11_aVIV
              x12_aVIW
              x13_aVIX
              x14_aVIY
              x15_aVIZ
              x16_aVJ0
              x17_aVJ1
              x18_aVJ2
              x19_aVJ3)
      (f_aVIK x3_aVIN)
{-# INLINE buttonConfig_basic #-}
buttonConfig_circular ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus Bool)
buttonConfig_circular
  f_aVJ5
  (ButtonConfig x1_aVJ6
                x2_aVJ7
                x3_aVJ8
                x4_aVJ9
                x5_aVJa
                x6_aVJb
                x7_aVJc
                x8_aVJd
                x9_aVJe
                x10_aVJf
                x11_aVJg
                x12_aVJh
                x13_aVJi
                x14_aVJj
                x15_aVJk
                x16_aVJl
                x17_aVJm
                x18_aVJn
                x19_aVJo)
  = fmap
      (\ y1_aVJp
         -> ButtonConfig
              x1_aVJ6
              x2_aVJ7
              x3_aVJ8
              x4_aVJ9
              x5_aVJa
              x6_aVJb
              x7_aVJc
              y1_aVJp
              x9_aVJe
              x10_aVJf
              x11_aVJg
              x12_aVJh
              x13_aVJi
              x14_aVJj
              x15_aVJk
              x16_aVJl
              x17_aVJm
              x18_aVJn
              x19_aVJo)
      (f_aVJ5 x8_aVJd)
{-# INLINE buttonConfig_circular #-}
buttonConfig_color ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus (Maybe Color))
buttonConfig_color
  f_aVJq
  (ButtonConfig x1_aVJr
                x2_aVJs
                x3_aVJt
                x4_aVJu
                x5_aVJv
                x6_aVJw
                x7_aVJx
                x8_aVJy
                x9_aVJz
                x10_aVJA
                x11_aVJB
                x12_aVJC
                x13_aVJD
                x14_aVJE
                x15_aVJF
                x16_aVJG
                x17_aVJH
                x18_aVJI
                x19_aVJJ)
  = fmap
      (\ y1_aVJK
         -> ButtonConfig
              x1_aVJr
              x2_aVJs
              x3_aVJt
              x4_aVJu
              x5_aVJv
              x6_aVJw
              x7_aVJx
              x8_aVJy
              y1_aVJK
              x10_aVJA
              x11_aVJB
              x12_aVJC
              x13_aVJD
              x14_aVJE
              x15_aVJF
              x16_aVJG
              x17_aVJH
              x18_aVJI
              x19_aVJJ)
      (f_aVJq x9_aVJz)
{-# INLINE buttonConfig_color #-}
buttonConfig_compact ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus Bool)
buttonConfig_compact
  f_aVJL
  (ButtonConfig x1_aVJM
                x2_aVJN
                x3_aVJO
                x4_aVJP
                x5_aVJQ
                x6_aVJR
                x7_aVJS
                x8_aVJT
                x9_aVJU
                x10_aVJV
                x11_aVJW
                x12_aVJX
                x13_aVJY
                x14_aVJZ
                x15_aVK0
                x16_aVK1
                x17_aVK2
                x18_aVK3
                x19_aVK4)
  = fmap
      (\ y1_aVK5
         -> ButtonConfig
              x1_aVJM
              y1_aVK5
              x3_aVJO
              x4_aVJP
              x5_aVJQ
              x6_aVJR
              x7_aVJS
              x8_aVJT
              x9_aVJU
              x10_aVJV
              x11_aVJW
              x12_aVJX
              x13_aVJY
              x14_aVJZ
              x15_aVK0
              x16_aVK1
              x17_aVK2
              x18_aVK3
              x19_aVK4)
      (f_aVJL x2_aVJN)
{-# INLINE buttonConfig_compact #-}
buttonConfig_disabled ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus Bool)
buttonConfig_disabled
  f_aVK6
  (ButtonConfig x1_aVK7
                x2_aVK8
                x3_aVK9
                x4_aVKa
                x5_aVKb
                x6_aVKc
                x7_aVKd
                x8_aVKe
                x9_aVKf
                x10_aVKg
                x11_aVKh
                x12_aVKi
                x13_aVKj
                x14_aVKk
                x15_aVKl
                x16_aVKm
                x17_aVKn
                x18_aVKo
                x19_aVKp)
  = fmap
      (\ y1_aVKq
         -> ButtonConfig
              y1_aVKq
              x2_aVK8
              x3_aVK9
              x4_aVKa
              x5_aVKb
              x6_aVKc
              x7_aVKd
              x8_aVKe
              x9_aVKf
              x10_aVKg
              x11_aVKh
              x12_aVKi
              x13_aVKj
              x14_aVKk
              x15_aVKl
              x16_aVKm
              x17_aVKn
              x18_aVKo
              x19_aVKp)
      (f_aVK6 x1_aVK7)
{-# INLINE buttonConfig_disabled #-}
buttonConfig_elConfig ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (ActiveElConfig t_aVus)
buttonConfig_elConfig
  f_aVKr
  (ButtonConfig x1_aVKs
                x2_aVKt
                x3_aVKu
                x4_aVKv
                x5_aVKw
                x6_aVKx
                x7_aVKy
                x8_aVKz
                x9_aVKA
                x10_aVKB
                x11_aVKC
                x12_aVKD
                x13_aVKE
                x14_aVKF
                x15_aVKG
                x16_aVKH
                x17_aVKJ
                x18_aVKL
                x19_aVKM)
  = fmap
      (\ y1_aVKN
         -> ButtonConfig
              x1_aVKs
              x2_aVKt
              x3_aVKu
              x4_aVKv
              x5_aVKw
              x6_aVKx
              x7_aVKy
              x8_aVKz
              x9_aVKA
              x10_aVKB
              x11_aVKC
              x12_aVKD
              x13_aVKE
              x14_aVKF
              x15_aVKG
              x16_aVKH
              x17_aVKJ
              x18_aVKL
              y1_aVKN)
      (f_aVKr x19_aVKM)
{-# INLINE buttonConfig_elConfig #-}
buttonConfig_emphasis ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus (Maybe Emphasis))
buttonConfig_emphasis
  f_aVKO
  (ButtonConfig x1_aVKP
                x2_aVKQ
                x3_aVKR
                x4_aVKS
                x5_aVKT
                x6_aVKU
                x7_aVKV
                x8_aVKW
                x9_aVKX
                x10_aVKY
                x11_aVKZ
                x12_aVL0
                x13_aVL1
                x14_aVL2
                x15_aVL3
                x16_aVL4
                x17_aVL5
                x18_aVL6
                x19_aVL7)
  = fmap
      (\ y1_aVL9
         -> ButtonConfig
              x1_aVKP
              x2_aVKQ
              x3_aVKR
              x4_aVKS
              x5_aVKT
              x6_aVKU
              x7_aVKV
              x8_aVKW
              x9_aVKX
              x10_aVKY
              y1_aVL9
              x12_aVL0
              x13_aVL1
              x14_aVL2
              x15_aVL3
              x16_aVL4
              x17_aVL5
              x18_aVL6
              x19_aVL7)
      (f_aVKO x11_aVKZ)
{-# INLINE buttonConfig_emphasis #-}
buttonConfig_floated ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus (Maybe Floated))
buttonConfig_floated
  f_aVLa
  (ButtonConfig x1_aVLb
                x2_aVLc
                x3_aVLd
                x4_aVLe
                x5_aVLf
                x6_aVLg
                x7_aVLh
                x8_aVLi
                x9_aVLj
                x10_aVLk
                x11_aVLl
                x12_aVLm
                x13_aVLn
                x14_aVLo
                x15_aVLp
                x16_aVLq
                x17_aVLr
                x18_aVLs
                x19_aVLt)
  = fmap
      (\ y1_aVLu
         -> ButtonConfig
              x1_aVLb
              x2_aVLc
              x3_aVLd
              x4_aVLe
              x5_aVLf
              x6_aVLg
              x7_aVLh
              x8_aVLi
              x9_aVLj
              x10_aVLk
              x11_aVLl
              x12_aVLm
              x13_aVLn
              y1_aVLu
              x15_aVLp
              x16_aVLq
              x17_aVLr
              x18_aVLs
              x19_aVLt)
      (f_aVLa x14_aVLo)
{-# INLINE buttonConfig_floated #-}
buttonConfig_fluid ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus Bool)
buttonConfig_fluid
  f_aVLw
  (ButtonConfig x1_aVLx
                x2_aVLy
                x3_aVLz
                x4_aVLA
                x5_aVLB
                x6_aVLC
                x7_aVLD
                x8_aVLE
                x9_aVLF
                x10_aVLG
                x11_aVLI
                x12_aVLJ
                x13_aVLK
                x14_aVLL
                x15_aVLM
                x16_aVLN
                x17_aVLO
                x18_aVLP
                x19_aVLQ)
  = fmap
      (\ y1_aVLR
         -> ButtonConfig
              x1_aVLx
              x2_aVLy
              x3_aVLz
              x4_aVLA
              x5_aVLB
              x6_aVLC
              y1_aVLR
              x8_aVLE
              x9_aVLF
              x10_aVLG
              x11_aVLI
              x12_aVLJ
              x13_aVLK
              x14_aVLL
              x15_aVLM
              x16_aVLN
              x17_aVLO
              x18_aVLP
              x19_aVLQ)
      (f_aVLw x7_aVLD)
{-# INLINE buttonConfig_fluid #-}
buttonConfig_icon ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus Bool)
buttonConfig_icon
  f_aVLT
  (ButtonConfig x1_aVLU
                x2_aVLV
                x3_aVLW
                x4_aVLX
                x5_aVLY
                x6_aVLZ
                x7_aVM1
                x8_aVM2
                x9_aVM3
                x10_aVM4
                x11_aVM5
                x12_aVM6
                x13_aVM7
                x14_aVM8
                x15_aVM9
                x16_aVMa
                x17_aVMb
                x18_aVMc
                x19_aVMd)
  = fmap
      (\ y1_aVMe
         -> ButtonConfig
              x1_aVLU
              x2_aVLV
              x3_aVLW
              y1_aVMe
              x5_aVLY
              x6_aVLZ
              x7_aVM1
              x8_aVM2
              x9_aVM3
              x10_aVM4
              x11_aVM5
              x12_aVM6
              x13_aVM7
              x14_aVM8
              x15_aVM9
              x16_aVMa
              x17_aVMb
              x18_aVMc
              x19_aVMd)
      (f_aVLT x4_aVLX)
{-# INLINE buttonConfig_icon #-}
buttonConfig_inverted ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus Bool)
buttonConfig_inverted
  f_aVMg
  (ButtonConfig x1_aVMh
                x2_aVMi
                x3_aVMj
                x4_aVMk
                x5_aVMl
                x6_aVMm
                x7_aVMn
                x8_aVMp
                x9_aVMr
                x10_aVMs
                x11_aVMt
                x12_aVMu
                x13_aVMv
                x14_aVMw
                x15_aVMx
                x16_aVMy
                x17_aVMz
                x18_aVMA
                x19_aVMB)
  = fmap
      (\ y1_aVMC
         -> ButtonConfig
              x1_aVMh
              x2_aVMi
              x3_aVMj
              x4_aVMk
              y1_aVMC
              x6_aVMm
              x7_aVMn
              x8_aVMp
              x9_aVMr
              x10_aVMs
              x11_aVMt
              x12_aVMu
              x13_aVMv
              x14_aVMw
              x15_aVMx
              x16_aVMy
              x17_aVMz
              x18_aVMA
              x19_aVMB)
      (f_aVMg x5_aVMl)
{-# INLINE buttonConfig_inverted #-}
buttonConfig_labeledIcon ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus (Maybe Labeled))
buttonConfig_labeledIcon
  f_aVMD
  (ButtonConfig x1_aVME
                x2_aVMF
                x3_aVMG
                x4_aVMH
                x5_aVMI
                x6_aVMJ
                x7_aVMK
                x8_aVML
                x9_aVMM
                x10_aVMN
                x11_aVMO
                x12_aVMP
                x13_aVMQ
                x14_aVMR
                x15_aVMS
                x16_aVMT
                x17_aVMU
                x18_aVMV
                x19_aVMW)
  = fmap
      (\ y1_aVMX
         -> ButtonConfig
              x1_aVME
              x2_aVMF
              x3_aVMG
              x4_aVMH
              x5_aVMI
              x6_aVMJ
              x7_aVMK
              x8_aVML
              x9_aVMM
              x10_aVMN
              x11_aVMO
              x12_aVMP
              x13_aVMQ
              x14_aVMR
              y1_aVMX
              x16_aVMT
              x17_aVMU
              x18_aVMV
              x19_aVMW)
      (f_aVMD x15_aVMS)
{-# INLINE buttonConfig_labeledIcon #-}
buttonConfig_loading ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus Bool)
buttonConfig_loading
  f_aVMZ
  (ButtonConfig x1_aVN0
                x2_aVN1
                x3_aVN2
                x4_aVN3
                x5_aVN4
                x6_aVN5
                x7_aVN6
                x8_aVN7
                x9_aVN8
                x10_aVN9
                x11_aVNa
                x12_aVNb
                x13_aVNc
                x14_aVNd
                x15_aVNe
                x16_aVNf
                x17_aVNg
                x18_aVNh
                x19_aVNi)
  = fmap
      (\ y1_aVNj
         -> ButtonConfig
              x1_aVN0
              x2_aVN1
              x3_aVN2
              x4_aVN3
              x5_aVN4
              y1_aVNj
              x7_aVN6
              x8_aVN7
              x9_aVN8
              x10_aVN9
              x11_aVNa
              x12_aVNb
              x13_aVNc
              x14_aVNd
              x15_aVNe
              x16_aVNf
              x17_aVNg
              x18_aVNh
              x19_aVNi)
      (f_aVMZ x6_aVN5)
{-# INLINE buttonConfig_loading #-}
buttonConfig_positive ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus (Maybe Positive))
buttonConfig_positive
  f_aVNl
  (ButtonConfig x1_aVNm
                x2_aVNn
                x3_aVNo
                x4_aVNp
                x5_aVNr
                x6_aVNs
                x7_aVNt
                x8_aVNu
                x9_aVNv
                x10_aVNw
                x11_aVNx
                x12_aVNy
                x13_aVNz
                x14_aVNA
                x15_aVNB
                x16_aVNC
                x17_aVND
                x18_aVNE
                x19_aVNF)
  = fmap
      (\ y1_aVNG
         -> ButtonConfig
              x1_aVNm
              x2_aVNn
              x3_aVNo
              x4_aVNp
              x5_aVNr
              x6_aVNs
              x7_aVNt
              x8_aVNu
              x9_aVNv
              x10_aVNw
              x11_aVNx
              y1_aVNG
              x13_aVNz
              x14_aVNA
              x15_aVNB
              x16_aVNC
              x17_aVND
              x18_aVNE
              x19_aVNF)
      (f_aVNl x12_aVNy)
{-# INLINE buttonConfig_positive #-}
buttonConfig_size ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus (Maybe Size))
buttonConfig_size
  f_aVNI
  (ButtonConfig x1_aVNJ
                x2_aVNK
                x3_aVNL
                x4_aVNM
                x5_aVNN
                x6_aVNO
                x7_aVNP
                x8_aVNQ
                x9_aVNR
                x10_aVNS
                x11_aVNU
                x12_aVNV
                x13_aVNW
                x14_aVNX
                x15_aVNY
                x16_aVNZ
                x17_aVO0
                x18_aVO1
                x19_aVO2)
  = fmap
      (\ y1_aVO3
         -> ButtonConfig
              x1_aVNJ
              x2_aVNK
              x3_aVNL
              x4_aVNM
              x5_aVNN
              x6_aVNO
              x7_aVNP
              x8_aVNQ
              x9_aVNR
              y1_aVO3
              x11_aVNU
              x12_aVNV
              x13_aVNW
              x14_aVNX
              x15_aVNY
              x16_aVNZ
              x17_aVO0
              x18_aVO1
              x19_aVO2)
      (f_aVNI x10_aVNS)
{-# INLINE buttonConfig_size #-}
buttonConfig_social ::
  forall t_aVus m_aVut.
  Lens' (ButtonConfig t_aVus m_aVut) (Active t_aVus (Maybe Social))
buttonConfig_social
  f_aVO5
  (ButtonConfig x1_aVO7
                x2_aVO8
                x3_aVO9
                x4_aVOa
                x5_aVOb
                x6_aVOc
                x7_aVOd
                x8_aVOe
                x9_aVOf
                x10_aVOg
                x11_aVOh
                x12_aVOi
                x13_aVOj
                x14_aVOk
                x15_aVOl
                x16_aVOm
                x17_aVOn
                x18_aVOo
                x19_aVOp)
  = fmap
      (\ y1_aVOq
         -> ButtonConfig
              x1_aVO7
              x2_aVO8
              x3_aVO9
              x4_aVOa
              x5_aVOb
              x6_aVOc
              x7_aVOd
              x8_aVOe
              x9_aVOf
              x10_aVOg
              x11_aVOh
              x12_aVOi
              y1_aVOq
              x14_aVOk
              x15_aVOl
              x16_aVOm
              x17_aVOn
              x18_aVOo
              x19_aVOp)
      (f_aVO5 x13_aVOj)
{-# INLINE buttonConfig_social #-}
buttonConfig_type ::
  forall t_aVus m_aVut. Lens' (ButtonConfig t_aVus m_aVut) ButtonType
buttonConfig_type
  f_aVOt
  (ButtonConfig x1_aVOu
                x2_aVOv
                x3_aVOw
                x4_aVOx
                x5_aVOy
                x6_aVOz
                x7_aVOA
                x8_aVOB
                x9_aVOC
                x10_aVOD
                x11_aVOE
                x12_aVOF
                x13_aVOG
                x14_aVOH
                x15_aVOI
                x16_aVOJ
                x17_aVOK
                x18_aVOL
                x19_aVOM)
  = fmap
      (\ y1_aVON
         -> ButtonConfig
              x1_aVOu
              x2_aVOv
              x3_aVOw
              x4_aVOx
              x5_aVOy
              x6_aVOz
              x7_aVOA
              x8_aVOB
              x9_aVOC
              x10_aVOD
              x11_aVOE
              x12_aVOF
              x13_aVOG
              x14_aVOH
              x15_aVOI
              x16_aVOJ
              x17_aVOK
              y1_aVON
              x19_aVOM)
      (f_aVOt x18_aVOL)
{-# INLINE buttonConfig_type #-}
