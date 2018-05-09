-- src/Reflex/Dom/SemanticUI/Button.hs:96:1-65: Splicing declarations
buttonsConfig_attached ::
  forall t_aUAj.
  Lens' (ButtonsConfig t_aUAj) (Active t_aUAj (Maybe VerticalAttached))
buttonsConfig_attached
  f_aUEZ
  (ButtonsConfig x1_aUF0
                 x2_aUF1
                 x3_aUF2
                 x4_aUF3
                 x5_aUF4
                 x6_aUF5
                 x7_aUF6
                 x8_aUF7
                 x9_aUF8
                 x10_aUF9
                 x11_aUFa)
  = fmap
      (\ y1_aUFb
         -> ButtonsConfig
              x1_aUF0
              x2_aUF1
              x3_aUF2
              x4_aUF3
              x5_aUF4
              x6_aUF5
              x7_aUF6
              y1_aUFb
              x9_aUF8
              x10_aUF9
              x11_aUFa)
      (f_aUEZ x8_aUF7)
{-# INLINE buttonsConfig_attached #-}
buttonsConfig_basic ::
  forall t_aUAj. Lens' (ButtonsConfig t_aUAj) (Active t_aUAj Bool)
buttonsConfig_basic
  f_aUFc
  (ButtonsConfig x1_aUFd
                 x2_aUFe
                 x3_aUFf
                 x4_aUFg
                 x5_aUFh
                 x6_aUFi
                 x7_aUFj
                 x8_aUFk
                 x9_aUFl
                 x10_aUFm
                 x11_aUFn)
  = fmap
      (\ y1_aUFo
         -> ButtonsConfig
              y1_aUFo
              x2_aUFe
              x3_aUFf
              x4_aUFg
              x5_aUFh
              x6_aUFi
              x7_aUFj
              x8_aUFk
              x9_aUFl
              x10_aUFm
              x11_aUFn)
      (f_aUFc x1_aUFd)
{-# INLINE buttonsConfig_basic #-}
buttonsConfig_color ::
  forall t_aUAj.
  Lens' (ButtonsConfig t_aUAj) (Active t_aUAj (Maybe Color))
buttonsConfig_color
  f_aUFp
  (ButtonsConfig x1_aUFq
                 x2_aUFr
                 x3_aUFs
                 x4_aUFt
                 x5_aUFu
                 x6_aUFv
                 x7_aUFw
                 x8_aUFx
                 x9_aUFy
                 x10_aUFz
                 x11_aUFA)
  = fmap
      (\ y1_aUFB
         -> ButtonsConfig
              x1_aUFq
              x2_aUFr
              x3_aUFs
              x4_aUFt
              x5_aUFu
              y1_aUFB
              x7_aUFw
              x8_aUFx
              x9_aUFy
              x10_aUFz
              x11_aUFA)
      (f_aUFp x6_aUFv)
{-# INLINE buttonsConfig_color #-}
buttonsConfig_compact ::
  forall t_aUAj. Lens' (ButtonsConfig t_aUAj) (Active t_aUAj Bool)
buttonsConfig_compact
  f_aUFC
  (ButtonsConfig x1_aUFD
                 x2_aUFE
                 x3_aUFF
                 x4_aUFG
                 x5_aUFH
                 x6_aUFI
                 x7_aUFJ
                 x8_aUFK
                 x9_aUFL
                 x10_aUFM
                 x11_aUFN)
  = fmap
      (\ y1_aUFO
         -> ButtonsConfig
              x1_aUFD
              x2_aUFE
              x3_aUFF
              x4_aUFG
              y1_aUFO
              x6_aUFI
              x7_aUFJ
              x8_aUFK
              x9_aUFL
              x10_aUFM
              x11_aUFN)
      (f_aUFC x5_aUFH)
{-# INLINE buttonsConfig_compact #-}
buttonsConfig_elConfig ::
  forall t_aUAj. Lens' (ButtonsConfig t_aUAj) (ActiveElConfig t_aUAj)
buttonsConfig_elConfig
  f_aUFP
  (ButtonsConfig x1_aUFQ
                 x2_aUFR
                 x3_aUFT
                 x4_aUFV
                 x5_aUFW
                 x6_aUFX
                 x7_aUFY
                 x8_aUFZ
                 x9_aUG0
                 x10_aUG1
                 x11_aUG2)
  = fmap
      (\ y1_aUG3
         -> ButtonsConfig
              x1_aUFQ
              x2_aUFR
              x3_aUFT
              x4_aUFV
              x5_aUFW
              x6_aUFX
              x7_aUFY
              x8_aUFZ
              x9_aUG0
              x10_aUG1
              y1_aUG3)
      (f_aUFP x11_aUG2)
{-# INLINE buttonsConfig_elConfig #-}
buttonsConfig_floated ::
  forall t_aUAj.
  Lens' (ButtonsConfig t_aUAj) (Active t_aUAj (Maybe Floated))
buttonsConfig_floated
  f_aUG4
  (ButtonsConfig x1_aUG5
                 x2_aUG6
                 x3_aUG7
                 x4_aUG8
                 x5_aUG9
                 x6_aUGa
                 x7_aUGb
                 x8_aUGc
                 x9_aUGd
                 x10_aUGe
                 x11_aUGf)
  = fmap
      (\ y1_aUGg
         -> ButtonsConfig
              x1_aUG5
              x2_aUG6
              x3_aUG7
              x4_aUG8
              x5_aUG9
              x6_aUGa
              x7_aUGb
              x8_aUGc
              x9_aUGd
              y1_aUGg
              x11_aUGf)
      (f_aUG4 x10_aUGe)
{-# INLINE buttonsConfig_floated #-}
buttonsConfig_icon ::
  forall t_aUAj. Lens' (ButtonsConfig t_aUAj) (Active t_aUAj Bool)
buttonsConfig_icon
  f_aUGh
  (ButtonsConfig x1_aUGi
                 x2_aUGk
                 x3_aUGl
                 x4_aUGm
                 x5_aUGn
                 x6_aUGo
                 x7_aUGp
                 x8_aUGq
                 x9_aUGr
                 x10_aUGs
                 x11_aUGt)
  = fmap
      (\ y1_aUGu
         -> ButtonsConfig
              x1_aUGi
              y1_aUGu
              x3_aUGl
              x4_aUGm
              x5_aUGn
              x6_aUGo
              x7_aUGp
              x8_aUGq
              x9_aUGr
              x10_aUGs
              x11_aUGt)
      (f_aUGh x2_aUGk)
{-# INLINE buttonsConfig_icon #-}
buttonsConfig_labeledIcon ::
  forall t_aUAj. Lens' (ButtonsConfig t_aUAj) (Active t_aUAj Bool)
buttonsConfig_labeledIcon
  f_aUGv
  (ButtonsConfig x1_aUGw
                 x2_aUGx
                 x3_aUGy
                 x4_aUGz
                 x5_aUGA
                 x6_aUGB
                 x7_aUGC
                 x8_aUGD
                 x9_aUGE
                 x10_aUGF
                 x11_aUGG)
  = fmap
      (\ y1_aUGH
         -> ButtonsConfig
              x1_aUGw
              x2_aUGx
              y1_aUGH
              x4_aUGz
              x5_aUGA
              x6_aUGB
              x7_aUGC
              x8_aUGD
              x9_aUGE
              x10_aUGF
              x11_aUGG)
      (f_aUGv x3_aUGy)
{-# INLINE buttonsConfig_labeledIcon #-}
buttonsConfig_size ::
  forall t_aUAj.
  Lens' (ButtonsConfig t_aUAj) (Active t_aUAj (Maybe Size))
buttonsConfig_size
  f_aUGI
  (ButtonsConfig x1_aUGJ
                 x2_aUGK
                 x3_aUGL
                 x4_aUGM
                 x5_aUGN
                 x6_aUGP
                 x7_aUGQ
                 x8_aUGR
                 x9_aUGS
                 x10_aUGT
                 x11_aUGU)
  = fmap
      (\ y1_aUGV
         -> ButtonsConfig
              x1_aUGJ
              x2_aUGK
              x3_aUGL
              x4_aUGM
              x5_aUGN
              x6_aUGP
              y1_aUGV
              x8_aUGR
              x9_aUGS
              x10_aUGT
              x11_aUGU)
      (f_aUGI x7_aUGQ)
{-# INLINE buttonsConfig_size #-}
buttonsConfig_vertical ::
  forall t_aUAj. Lens' (ButtonsConfig t_aUAj) (Active t_aUAj Bool)
buttonsConfig_vertical
  f_aUGW
  (ButtonsConfig x1_aUGY
                 x2_aUGZ
                 x3_aUH0
                 x4_aUH1
                 x5_aUH2
                 x6_aUH3
                 x7_aUH4
                 x8_aUH5
                 x9_aUH6
                 x10_aUH7
                 x11_aUH8)
  = fmap
      (\ y1_aUH9
         -> ButtonsConfig
              x1_aUGY
              x2_aUGZ
              x3_aUH0
              y1_aUH9
              x5_aUH2
              x6_aUH3
              x7_aUH4
              x8_aUH5
              x9_aUH6
              x10_aUH7
              x11_aUH8)
      (f_aUGW x4_aUH1)
{-# INLINE buttonsConfig_vertical #-}
buttonsConfig_width ::
  forall t_aUAj.
  Lens' (ButtonsConfig t_aUAj) (Active t_aUAj (Maybe Width))
buttonsConfig_width
  f_aUHb
  (ButtonsConfig x1_aUHc
                 x2_aUHd
                 x3_aUHe
                 x4_aUHf
                 x5_aUHg
                 x6_aUHh
                 x7_aUHi
                 x8_aUHj
                 x9_aUHk
                 x10_aUHl
                 x11_aUHm)
  = fmap
      (\ y1_aUHn
         -> ButtonsConfig
              x1_aUHc
              x2_aUHd
              x3_aUHe
              x4_aUHf
              x5_aUHg
              x6_aUHh
              x7_aUHi
              x8_aUHj
              y1_aUHn
              x10_aUHl
              x11_aUHm)
      (f_aUHb x9_aUHk)
{-# INLINE buttonsConfig_width #-}
-- src/Reflex/Dom/SemanticUI/Button.hs:142:1-71: Splicing declarations
labeledButtonConfig_elConfig ::
  forall t_aUM0.
  Lens' (LabeledButtonConfig t_aUM0) (ActiveElConfig t_aUM0)
labeledButtonConfig_elConfig
  f_aV6F
  (LabeledButtonConfig x1_aV6G x2_aV6H)
  = fmap
      (\ y1_aV6I -> LabeledButtonConfig x1_aV6G y1_aV6I) (f_aV6F x2_aV6H)
{-# INLINE labeledButtonConfig_elConfig #-}
labeledButtonConfig_side ::
  forall t_aUM0.
  Lens' (LabeledButtonConfig t_aUM0) (Active t_aUM0 Labeled)
labeledButtonConfig_side
  f_aV6J
  (LabeledButtonConfig x1_aV6K x2_aV6L)
  = fmap
      (\ y1_aV6M -> LabeledButtonConfig y1_aV6M x2_aV6L) (f_aV6J x1_aV6K)
{-# INLINE labeledButtonConfig_side #-}
-- src/Reflex/Dom/SemanticUI/Button.hs:175:1-66: Splicing declarations
animatedButton_hiddenContent ::
  forall t_aV83 m_aV84.
  Lens' (AnimatedButton t_aV83 m_aV84) (m_aV84 ())
animatedButton_hiddenContent
  f_aVDc
  (AnimatedButton x1_aVDd x2_aVDe)
  = fmap
      (\ y1_aVDf -> AnimatedButton x1_aVDd y1_aVDf) (f_aVDc x2_aVDe)
{-# INLINE animatedButton_hiddenContent #-}
animatedButton_type ::
  forall t_aV83 m_aV84.
  Lens' (AnimatedButton t_aV83 m_aV84) (Active t_aV83 AnimatedButtonType)
animatedButton_type f_aVDg (AnimatedButton x1_aVDh x2_aVDi)
  = fmap
      (\ y1_aVDj -> AnimatedButton y1_aVDj x2_aVDi) (f_aVDg x1_aVDh)
{-# INLINE animatedButton_type #-}
-- src/Reflex/Dom/SemanticUI/Button.hs:226:1-64: Splicing declarations
buttonConfig_animated ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Maybe (AnimatedButton t_aVDl m_aVDm))
buttonConfig_animated
  f_aW5S
  (ButtonConfig x1_aW5T
                x2_aW5U
                x3_aW5V
                x4_aW5W
                x5_aW5X
                x6_aW5Y
                x7_aW5Z
                x8_aW60
                x9_aW61
                x10_aW62
                x11_aW63
                x12_aW64
                x13_aW65
                x14_aW66
                x15_aW67
                x16_aW68
                x17_aW69
                x18_aW6a
                x19_aW6b)
  = fmap
      (\ y1_aW6c
         -> ButtonConfig
              x1_aW5T
              x2_aW5U
              x3_aW5V
              x4_aW5W
              x5_aW5X
              x6_aW5Y
              x7_aW5Z
              x8_aW60
              x9_aW61
              x10_aW62
              x11_aW63
              x12_aW64
              x13_aW65
              x14_aW66
              x15_aW67
              x16_aW68
              y1_aW6c
              x18_aW6a
              x19_aW6b)
      (f_aW5S x17_aW69)
{-# INLINE buttonConfig_animated #-}
buttonConfig_attached ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl (Maybe ExclusiveAttached))
buttonConfig_attached
  f_aW6d
  (ButtonConfig x1_aW6e
                x2_aW6f
                x3_aW6g
                x4_aW6h
                x5_aW6i
                x6_aW6j
                x7_aW6k
                x8_aW6l
                x9_aW6m
                x10_aW6n
                x11_aW6o
                x12_aW6p
                x13_aW6q
                x14_aW6r
                x15_aW6s
                x16_aW6t
                x17_aW6u
                x18_aW6v
                x19_aW6w)
  = fmap
      (\ y1_aW6x
         -> ButtonConfig
              x1_aW6e
              x2_aW6f
              x3_aW6g
              x4_aW6h
              x5_aW6i
              x6_aW6j
              x7_aW6k
              x8_aW6l
              x9_aW6m
              x10_aW6n
              x11_aW6o
              x12_aW6p
              x13_aW6q
              x14_aW6r
              x15_aW6s
              y1_aW6x
              x17_aW6u
              x18_aW6v
              x19_aW6w)
      (f_aW6d x16_aW6t)
{-# INLINE buttonConfig_attached #-}
buttonConfig_basic ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl Bool)
buttonConfig_basic
  f_aW6y
  (ButtonConfig x1_aW6z
                x2_aW6A
                x3_aW6B
                x4_aW6C
                x5_aW6D
                x6_aW6E
                x7_aW6F
                x8_aW6G
                x9_aW6H
                x10_aW6I
                x11_aW6J
                x12_aW6K
                x13_aW6L
                x14_aW6M
                x15_aW6N
                x16_aW6O
                x17_aW6P
                x18_aW6Q
                x19_aW6R)
  = fmap
      (\ y1_aW6S
         -> ButtonConfig
              x1_aW6z
              x2_aW6A
              y1_aW6S
              x4_aW6C
              x5_aW6D
              x6_aW6E
              x7_aW6F
              x8_aW6G
              x9_aW6H
              x10_aW6I
              x11_aW6J
              x12_aW6K
              x13_aW6L
              x14_aW6M
              x15_aW6N
              x16_aW6O
              x17_aW6P
              x18_aW6Q
              x19_aW6R)
      (f_aW6y x3_aW6B)
{-# INLINE buttonConfig_basic #-}
buttonConfig_circular ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl Bool)
buttonConfig_circular
  f_aW6T
  (ButtonConfig x1_aW6U
                x2_aW6V
                x3_aW6W
                x4_aW6X
                x5_aW6Y
                x6_aW6Z
                x7_aW70
                x8_aW71
                x9_aW72
                x10_aW73
                x11_aW74
                x12_aW75
                x13_aW76
                x14_aW77
                x15_aW78
                x16_aW79
                x17_aW7a
                x18_aW7b
                x19_aW7c)
  = fmap
      (\ y1_aW7d
         -> ButtonConfig
              x1_aW6U
              x2_aW6V
              x3_aW6W
              x4_aW6X
              x5_aW6Y
              x6_aW6Z
              x7_aW70
              y1_aW7d
              x9_aW72
              x10_aW73
              x11_aW74
              x12_aW75
              x13_aW76
              x14_aW77
              x15_aW78
              x16_aW79
              x17_aW7a
              x18_aW7b
              x19_aW7c)
      (f_aW6T x8_aW71)
{-# INLINE buttonConfig_circular #-}
buttonConfig_color ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl (Maybe Color))
buttonConfig_color
  f_aW7e
  (ButtonConfig x1_aW7f
                x2_aW7g
                x3_aW7h
                x4_aW7i
                x5_aW7j
                x6_aW7k
                x7_aW7l
                x8_aW7m
                x9_aW7n
                x10_aW7o
                x11_aW7p
                x12_aW7q
                x13_aW7r
                x14_aW7s
                x15_aW7t
                x16_aW7u
                x17_aW7v
                x18_aW7w
                x19_aW7x)
  = fmap
      (\ y1_aW7y
         -> ButtonConfig
              x1_aW7f
              x2_aW7g
              x3_aW7h
              x4_aW7i
              x5_aW7j
              x6_aW7k
              x7_aW7l
              x8_aW7m
              y1_aW7y
              x10_aW7o
              x11_aW7p
              x12_aW7q
              x13_aW7r
              x14_aW7s
              x15_aW7t
              x16_aW7u
              x17_aW7v
              x18_aW7w
              x19_aW7x)
      (f_aW7e x9_aW7n)
{-# INLINE buttonConfig_color #-}
buttonConfig_compact ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl Bool)
buttonConfig_compact
  f_aW7z
  (ButtonConfig x1_aW7A
                x2_aW7B
                x3_aW7C
                x4_aW7D
                x5_aW7E
                x6_aW7F
                x7_aW7G
                x8_aW7H
                x9_aW7I
                x10_aW7J
                x11_aW7K
                x12_aW7L
                x13_aW7M
                x14_aW7N
                x15_aW7O
                x16_aW7P
                x17_aW7Q
                x18_aW7R
                x19_aW7S)
  = fmap
      (\ y1_aW7T
         -> ButtonConfig
              x1_aW7A
              y1_aW7T
              x3_aW7C
              x4_aW7D
              x5_aW7E
              x6_aW7F
              x7_aW7G
              x8_aW7H
              x9_aW7I
              x10_aW7J
              x11_aW7K
              x12_aW7L
              x13_aW7M
              x14_aW7N
              x15_aW7O
              x16_aW7P
              x17_aW7Q
              x18_aW7R
              x19_aW7S)
      (f_aW7z x2_aW7B)
{-# INLINE buttonConfig_compact #-}
buttonConfig_disabled ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl Bool)
buttonConfig_disabled
  f_aW7U
  (ButtonConfig x1_aW7V
                x2_aW7W
                x3_aW7X
                x4_aW7Y
                x5_aW7Z
                x6_aW80
                x7_aW81
                x8_aW82
                x9_aW83
                x10_aW84
                x11_aW85
                x12_aW86
                x13_aW87
                x14_aW88
                x15_aW89
                x16_aW8a
                x17_aW8b
                x18_aW8c
                x19_aW8d)
  = fmap
      (\ y1_aW8e
         -> ButtonConfig
              y1_aW8e
              x2_aW7W
              x3_aW7X
              x4_aW7Y
              x5_aW7Z
              x6_aW80
              x7_aW81
              x8_aW82
              x9_aW83
              x10_aW84
              x11_aW85
              x12_aW86
              x13_aW87
              x14_aW88
              x15_aW89
              x16_aW8a
              x17_aW8b
              x18_aW8c
              x19_aW8d)
      (f_aW7U x1_aW7V)
{-# INLINE buttonConfig_disabled #-}
buttonConfig_elConfig ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (ActiveElConfig t_aVDl)
buttonConfig_elConfig
  f_aW8f
  (ButtonConfig x1_aW8g
                x2_aW8h
                x3_aW8i
                x4_aW8j
                x5_aW8k
                x6_aW8l
                x7_aW8m
                x8_aW8n
                x9_aW8o
                x10_aW8p
                x11_aW8q
                x12_aW8r
                x13_aW8s
                x14_aW8t
                x15_aW8u
                x16_aW8v
                x17_aW8w
                x18_aW8x
                x19_aW8y)
  = fmap
      (\ y1_aW8z
         -> ButtonConfig
              x1_aW8g
              x2_aW8h
              x3_aW8i
              x4_aW8j
              x5_aW8k
              x6_aW8l
              x7_aW8m
              x8_aW8n
              x9_aW8o
              x10_aW8p
              x11_aW8q
              x12_aW8r
              x13_aW8s
              x14_aW8t
              x15_aW8u
              x16_aW8v
              x17_aW8w
              x18_aW8x
              y1_aW8z)
      (f_aW8f x19_aW8y)
{-# INLINE buttonConfig_elConfig #-}
buttonConfig_emphasis ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl (Maybe Emphasis))
buttonConfig_emphasis
  f_aW8A
  (ButtonConfig x1_aW8B
                x2_aW8C
                x3_aW8D
                x4_aW8E
                x5_aW8F
                x6_aW8G
                x7_aW8H
                x8_aW8I
                x9_aW8J
                x10_aW8K
                x11_aW8L
                x12_aW8M
                x13_aW8N
                x14_aW8O
                x15_aW8P
                x16_aW8Q
                x17_aW8R
                x18_aW8S
                x19_aW8T)
  = fmap
      (\ y1_aW8U
         -> ButtonConfig
              x1_aW8B
              x2_aW8C
              x3_aW8D
              x4_aW8E
              x5_aW8F
              x6_aW8G
              x7_aW8H
              x8_aW8I
              x9_aW8J
              x10_aW8K
              y1_aW8U
              x12_aW8M
              x13_aW8N
              x14_aW8O
              x15_aW8P
              x16_aW8Q
              x17_aW8R
              x18_aW8S
              x19_aW8T)
      (f_aW8A x11_aW8L)
{-# INLINE buttonConfig_emphasis #-}
buttonConfig_floated ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl (Maybe Floated))
buttonConfig_floated
  f_aW8V
  (ButtonConfig x1_aW8W
                x2_aW8X
                x3_aW8Y
                x4_aW8Z
                x5_aW90
                x6_aW91
                x7_aW92
                x8_aW93
                x9_aW94
                x10_aW95
                x11_aW96
                x12_aW97
                x13_aW98
                x14_aW99
                x15_aW9a
                x16_aW9b
                x17_aW9c
                x18_aW9d
                x19_aW9e)
  = fmap
      (\ y1_aW9f
         -> ButtonConfig
              x1_aW8W
              x2_aW8X
              x3_aW8Y
              x4_aW8Z
              x5_aW90
              x6_aW91
              x7_aW92
              x8_aW93
              x9_aW94
              x10_aW95
              x11_aW96
              x12_aW97
              x13_aW98
              y1_aW9f
              x15_aW9a
              x16_aW9b
              x17_aW9c
              x18_aW9d
              x19_aW9e)
      (f_aW8V x14_aW99)
{-# INLINE buttonConfig_floated #-}
buttonConfig_fluid ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl Bool)
buttonConfig_fluid
  f_aW9g
  (ButtonConfig x1_aW9h
                x2_aW9i
                x3_aW9j
                x4_aW9k
                x5_aW9l
                x6_aW9m
                x7_aW9n
                x8_aW9o
                x9_aW9p
                x10_aW9q
                x11_aW9r
                x12_aW9s
                x13_aW9t
                x14_aW9u
                x15_aW9v
                x16_aW9w
                x17_aW9x
                x18_aW9y
                x19_aW9z)
  = fmap
      (\ y1_aW9A
         -> ButtonConfig
              x1_aW9h
              x2_aW9i
              x3_aW9j
              x4_aW9k
              x5_aW9l
              x6_aW9m
              y1_aW9A
              x8_aW9o
              x9_aW9p
              x10_aW9q
              x11_aW9r
              x12_aW9s
              x13_aW9t
              x14_aW9u
              x15_aW9v
              x16_aW9w
              x17_aW9x
              x18_aW9y
              x19_aW9z)
      (f_aW9g x7_aW9n)
{-# INLINE buttonConfig_fluid #-}
buttonConfig_icon ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl Bool)
buttonConfig_icon
  f_aW9B
  (ButtonConfig x1_aW9C
                x2_aW9D
                x3_aW9E
                x4_aW9F
                x5_aW9G
                x6_aW9H
                x7_aW9I
                x8_aW9J
                x9_aW9K
                x10_aW9L
                x11_aW9M
                x12_aW9N
                x13_aW9O
                x14_aW9P
                x15_aW9Q
                x16_aW9R
                x17_aW9S
                x18_aW9T
                x19_aW9U)
  = fmap
      (\ y1_aW9V
         -> ButtonConfig
              x1_aW9C
              x2_aW9D
              x3_aW9E
              y1_aW9V
              x5_aW9G
              x6_aW9H
              x7_aW9I
              x8_aW9J
              x9_aW9K
              x10_aW9L
              x11_aW9M
              x12_aW9N
              x13_aW9O
              x14_aW9P
              x15_aW9Q
              x16_aW9R
              x17_aW9S
              x18_aW9T
              x19_aW9U)
      (f_aW9B x4_aW9F)
{-# INLINE buttonConfig_icon #-}
buttonConfig_inverted ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl Bool)
buttonConfig_inverted
  f_aW9W
  (ButtonConfig x1_aW9X
                x2_aW9Y
                x3_aW9Z
                x4_aWa0
                x5_aWa1
                x6_aWa2
                x7_aWa3
                x8_aWa4
                x9_aWa5
                x10_aWa6
                x11_aWa7
                x12_aWa8
                x13_aWa9
                x14_aWaa
                x15_aWab
                x16_aWac
                x17_aWad
                x18_aWae
                x19_aWaf)
  = fmap
      (\ y1_aWag
         -> ButtonConfig
              x1_aW9X
              x2_aW9Y
              x3_aW9Z
              x4_aWa0
              y1_aWag
              x6_aWa2
              x7_aWa3
              x8_aWa4
              x9_aWa5
              x10_aWa6
              x11_aWa7
              x12_aWa8
              x13_aWa9
              x14_aWaa
              x15_aWab
              x16_aWac
              x17_aWad
              x18_aWae
              x19_aWaf)
      (f_aW9W x5_aWa1)
{-# INLINE buttonConfig_inverted #-}
buttonConfig_labeledIcon ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl (Maybe Labeled))
buttonConfig_labeledIcon
  f_aWah
  (ButtonConfig x1_aWai
                x2_aWaj
                x3_aWak
                x4_aWal
                x5_aWam
                x6_aWan
                x7_aWao
                x8_aWap
                x9_aWaq
                x10_aWar
                x11_aWas
                x12_aWat
                x13_aWau
                x14_aWav
                x15_aWaw
                x16_aWax
                x17_aWay
                x18_aWaz
                x19_aWaA)
  = fmap
      (\ y1_aWaB
         -> ButtonConfig
              x1_aWai
              x2_aWaj
              x3_aWak
              x4_aWal
              x5_aWam
              x6_aWan
              x7_aWao
              x8_aWap
              x9_aWaq
              x10_aWar
              x11_aWas
              x12_aWat
              x13_aWau
              x14_aWav
              y1_aWaB
              x16_aWax
              x17_aWay
              x18_aWaz
              x19_aWaA)
      (f_aWah x15_aWaw)
{-# INLINE buttonConfig_labeledIcon #-}
buttonConfig_loading ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl Bool)
buttonConfig_loading
  f_aWaC
  (ButtonConfig x1_aWaD
                x2_aWaE
                x3_aWaF
                x4_aWaG
                x5_aWaH
                x6_aWaI
                x7_aWaJ
                x8_aWaK
                x9_aWaL
                x10_aWaM
                x11_aWaN
                x12_aWaO
                x13_aWaP
                x14_aWaQ
                x15_aWaR
                x16_aWaS
                x17_aWaT
                x18_aWaU
                x19_aWaV)
  = fmap
      (\ y1_aWaW
         -> ButtonConfig
              x1_aWaD
              x2_aWaE
              x3_aWaF
              x4_aWaG
              x5_aWaH
              y1_aWaW
              x7_aWaJ
              x8_aWaK
              x9_aWaL
              x10_aWaM
              x11_aWaN
              x12_aWaO
              x13_aWaP
              x14_aWaQ
              x15_aWaR
              x16_aWaS
              x17_aWaT
              x18_aWaU
              x19_aWaV)
      (f_aWaC x6_aWaI)
{-# INLINE buttonConfig_loading #-}
buttonConfig_positive ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl (Maybe Positive))
buttonConfig_positive
  f_aWaX
  (ButtonConfig x1_aWaY
                x2_aWaZ
                x3_aWb0
                x4_aWb1
                x5_aWb2
                x6_aWb3
                x7_aWb4
                x8_aWb5
                x9_aWb6
                x10_aWb7
                x11_aWb8
                x12_aWb9
                x13_aWba
                x14_aWbb
                x15_aWbc
                x16_aWbd
                x17_aWbe
                x18_aWbf
                x19_aWbg)
  = fmap
      (\ y1_aWbh
         -> ButtonConfig
              x1_aWaY
              x2_aWaZ
              x3_aWb0
              x4_aWb1
              x5_aWb2
              x6_aWb3
              x7_aWb4
              x8_aWb5
              x9_aWb6
              x10_aWb7
              x11_aWb8
              y1_aWbh
              x13_aWba
              x14_aWbb
              x15_aWbc
              x16_aWbd
              x17_aWbe
              x18_aWbf
              x19_aWbg)
      (f_aWaX x12_aWb9)
{-# INLINE buttonConfig_positive #-}
buttonConfig_size ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl (Maybe Size))
buttonConfig_size
  f_aWbi
  (ButtonConfig x1_aWbj
                x2_aWbk
                x3_aWbl
                x4_aWbm
                x5_aWbn
                x6_aWbo
                x7_aWbp
                x8_aWbq
                x9_aWbr
                x10_aWbs
                x11_aWbt
                x12_aWbu
                x13_aWbv
                x14_aWbw
                x15_aWbx
                x16_aWby
                x17_aWbz
                x18_aWbA
                x19_aWbB)
  = fmap
      (\ y1_aWbC
         -> ButtonConfig
              x1_aWbj
              x2_aWbk
              x3_aWbl
              x4_aWbm
              x5_aWbn
              x6_aWbo
              x7_aWbp
              x8_aWbq
              x9_aWbr
              y1_aWbC
              x11_aWbt
              x12_aWbu
              x13_aWbv
              x14_aWbw
              x15_aWbx
              x16_aWby
              x17_aWbz
              x18_aWbA
              x19_aWbB)
      (f_aWbi x10_aWbs)
{-# INLINE buttonConfig_size #-}
buttonConfig_social ::
  forall t_aVDl m_aVDm.
  Lens' (ButtonConfig t_aVDl m_aVDm) (Active t_aVDl (Maybe Social))
buttonConfig_social
  f_aWbD
  (ButtonConfig x1_aWbE
                x2_aWbF
                x3_aWbG
                x4_aWbH
                x5_aWbI
                x6_aWbJ
                x7_aWbK
                x8_aWbL
                x9_aWbM
                x10_aWbN
                x11_aWbO
                x12_aWbP
                x13_aWbQ
                x14_aWbR
                x15_aWbS
                x16_aWbT
                x17_aWbU
                x18_aWbV
                x19_aWbW)
  = fmap
      (\ y1_aWbX
         -> ButtonConfig
              x1_aWbE
              x2_aWbF
              x3_aWbG
              x4_aWbH
              x5_aWbI
              x6_aWbJ
              x7_aWbK
              x8_aWbL
              x9_aWbM
              x10_aWbN
              x11_aWbO
              x12_aWbP
              y1_aWbX
              x14_aWbR
              x15_aWbS
              x16_aWbT
              x17_aWbU
              x18_aWbV
              x19_aWbW)
      (f_aWbD x13_aWbQ)
{-# INLINE buttonConfig_social #-}
buttonConfig_type ::
  forall t_aVDl m_aVDm. Lens' (ButtonConfig t_aVDl m_aVDm) ButtonType
buttonConfig_type
  f_aWbY
  (ButtonConfig x1_aWbZ
                x2_aWc0
                x3_aWc1
                x4_aWc2
                x5_aWc3
                x6_aWc4
                x7_aWc5
                x8_aWc6
                x9_aWc7
                x10_aWc8
                x11_aWc9
                x12_aWca
                x13_aWcb
                x14_aWcc
                x15_aWcd
                x16_aWce
                x17_aWcf
                x18_aWcg
                x19_aWch)
  = fmap
      (\ y1_aWci
         -> ButtonConfig
              x1_aWbZ
              x2_aWc0
              x3_aWc1
              x4_aWc2
              x5_aWc3
              x6_aWc4
              x7_aWc5
              x8_aWc6
              x9_aWc7
              x10_aWc8
              x11_aWc9
              x12_aWca
              x13_aWcb
              x14_aWcc
              x15_aWcd
              x16_aWce
              x17_aWcf
              y1_aWci
              x19_aWch)
      (f_aWbY x18_aWcg)
{-# INLINE buttonConfig_type #-}
