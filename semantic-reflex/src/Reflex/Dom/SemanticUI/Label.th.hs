-- src/Reflex/Dom/SemanticUI/Label.hs:83:1-26: Splicing declarations
labelAttached_horizontally ::
  Lens' LabelAttached (Maybe HorizontalAttached)
labelAttached_horizontally
  f_a1XGa
  (LabelAttached x1_a1XGb x2_a1XGc)
  = fmap
      (\ y1_a1XGd -> LabelAttached x1_a1XGb y1_a1XGd) (f_a1XGa x2_a1XGc)
{-# INLINE labelAttached_horizontally #-}
labelAttached_vertically :: Lens' LabelAttached VerticalAttached
labelAttached_vertically f_a1XGe (LabelAttached x1_a1XGf x2_a1XGg)
  = fmap
      (\ y1_a1XGh -> LabelAttached y1_a1XGh x2_a1XGg) (f_a1XGe x1_a1XGf)
{-# INLINE labelAttached_vertically #-}
-- src/Reflex/Dom/SemanticUI/Label.hs:119:1-63: Splicing declarations
labelConfig_attached ::
  forall t_a1XHd.
  Lens' (LabelConfig t_a1XHd) (Active t_a1XHd (Maybe LabelAttached))
labelConfig_attached
  f_a1XXK
  (LabelConfig x1_a1XXP
               x2_a1XXQ
               x3_a1XXR
               x4_a1XXS
               x5_a1XXT
               x6_a1XXU
               x7_a1XXV
               x8_a1XXW
               x9_a1XXX
               x10_a1XXY
               x11_a1XXZ
               x12_a1XY0
               x13_a1XY1)
  = fmap
      (\ y1_a1XY2
         -> LabelConfig
              x1_a1XXP
              x2_a1XXQ
              x3_a1XXR
              x4_a1XXS
              x5_a1XXT
              x6_a1XXU
              y1_a1XY2
              x8_a1XXW
              x9_a1XXX
              x10_a1XXY
              x11_a1XXZ
              x12_a1XY0
              x13_a1XY1)
      (f_a1XXK x7_a1XXV)
{-# INLINE labelConfig_attached #-}
labelConfig_basic ::
  forall t_a1XHd. Lens' (LabelConfig t_a1XHd) (Active t_a1XHd Bool)
labelConfig_basic
  f_a1XY9
  (LabelConfig x1_a1XYa
               x2_a1XYb
               x3_a1XYc
               x4_a1XYd
               x5_a1XYe
               x6_a1XYf
               x7_a1XYg
               x8_a1XYh
               x9_a1XYi
               x10_a1XYj
               x11_a1XYk
               x12_a1XYl
               x13_a1XYm)
  = fmap
      (\ y1_a1XYn
         -> LabelConfig
              x1_a1XYa
              x2_a1XYb
              y1_a1XYn
              x4_a1XYd
              x5_a1XYe
              x6_a1XYf
              x7_a1XYg
              x8_a1XYh
              x9_a1XYi
              x10_a1XYj
              x11_a1XYk
              x12_a1XYl
              x13_a1XYm)
      (f_a1XY9 x3_a1XYc)
{-# INLINE labelConfig_basic #-}
labelConfig_color ::
  forall t_a1XHd.
  Lens' (LabelConfig t_a1XHd) (Active t_a1XHd (Maybe Color))
labelConfig_color
  f_a1XYs
  (LabelConfig x1_a1XYt
               x2_a1XYu
               x3_a1XYv
               x4_a1XYw
               x5_a1XYx
               x6_a1XYy
               x7_a1XYz
               x8_a1XYA
               x9_a1XYB
               x10_a1XYC
               x11_a1XYD
               x12_a1XYE
               x13_a1XYF)
  = fmap
      (\ y1_a1XYH
         -> LabelConfig
              x1_a1XYt
              x2_a1XYu
              x3_a1XYv
              x4_a1XYw
              x5_a1XYx
              x6_a1XYy
              x7_a1XYz
              y1_a1XYH
              x9_a1XYB
              x10_a1XYC
              x11_a1XYD
              x12_a1XYE
              x13_a1XYF)
      (f_a1XYs x8_a1XYA)
{-# INLINE labelConfig_color #-}
labelConfig_corner ::
  forall t_a1XHd.
  Lens' (LabelConfig t_a1XHd) (Active t_a1XHd (Maybe TopCorner))
labelConfig_corner
  f_a1XYJ
  (LabelConfig x1_a1XYK
               x2_a1XYL
               x3_a1XYO
               x4_a1XYR
               x5_a1XYS
               x6_a1XYT
               x7_a1XYU
               x8_a1XYW
               x9_a1XZ2
               x10_a1XZ5
               x11_a1XZ6
               x12_a1XZ7
               x13_a1XZ8)
  = fmap
      (\ y1_a1XZ9
         -> LabelConfig
              x1_a1XYK
              x2_a1XYL
              x3_a1XYO
              x4_a1XYR
              x5_a1XYS
              x6_a1XYT
              x7_a1XYU
              x8_a1XYW
              x9_a1XZ2
              x10_a1XZ5
              y1_a1XZ9
              x12_a1XZ7
              x13_a1XZ8)
      (f_a1XYJ x11_a1XZ6)
{-# INLINE labelConfig_corner #-}
labelConfig_elConfig ::
  forall t_a1XHd.
  Lens' (LabelConfig t_a1XHd) (ActiveElConfig t_a1XHd)
labelConfig_elConfig
  f_a1XZa
  (LabelConfig x1_a1XZb
               x2_a1XZc
               x3_a1XZf
               x4_a1XZg
               x5_a1XZh
               x6_a1XZi
               x7_a1XZj
               x8_a1XZk
               x9_a1XZl
               x10_a1XZm
               x11_a1XZn
               x12_a1XZo
               x13_a1XZp)
  = fmap
      (\ y1_a1XZt
         -> LabelConfig
              x1_a1XZb
              x2_a1XZc
              x3_a1XZf
              x4_a1XZg
              x5_a1XZh
              x6_a1XZi
              x7_a1XZj
              x8_a1XZk
              x9_a1XZl
              x10_a1XZm
              x11_a1XZn
              x12_a1XZo
              y1_a1XZt)
      (f_a1XZa x13_a1XZp)
{-# INLINE labelConfig_elConfig #-}
labelConfig_floating ::
  forall t_a1XHd. Lens' (LabelConfig t_a1XHd) (Active t_a1XHd Bool)
labelConfig_floating
  f_a1XZv
  (LabelConfig x1_a1XZw
               x2_a1XZx
               x3_a1XZy
               x4_a1XZz
               x5_a1XZA
               x6_a1XZB
               x7_a1XZC
               x8_a1XZD
               x9_a1XZE
               x10_a1XZF
               x11_a1XZG
               x12_a1XZI
               x13_a1XZK)
  = fmap
      (\ y1_a1XZN
         -> LabelConfig
              x1_a1XZw
              x2_a1XZx
              x3_a1XZy
              x4_a1XZz
              y1_a1XZN
              x6_a1XZB
              x7_a1XZC
              x8_a1XZD
              x9_a1XZE
              x10_a1XZF
              x11_a1XZG
              x12_a1XZI
              x13_a1XZK)
      (f_a1XZv x5_a1XZA)
{-# INLINE labelConfig_floating #-}
labelConfig_hidden ::
  forall t_a1XHd. Lens' (LabelConfig t_a1XHd) (Active t_a1XHd Bool)
labelConfig_hidden
  f_a1XZO
  (LabelConfig x1_a1XZP
               x2_a1XZQ
               x3_a1XZR
               x4_a1XZS
               x5_a1XZT
               x6_a1XZU
               x7_a1XZV
               x8_a1XZW
               x9_a1XZX
               x10_a1XZY
               x11_a1XZZ
               x12_a1Y00
               x13_a1Y01)
  = fmap
      (\ y1_a1Y02
         -> LabelConfig
              x1_a1XZP
              y1_a1Y02
              x3_a1XZR
              x4_a1XZS
              x5_a1XZT
              x6_a1XZU
              x7_a1XZV
              x8_a1XZW
              x9_a1XZX
              x10_a1XZY
              x11_a1XZZ
              x12_a1Y00
              x13_a1Y01)
      (f_a1XZO x2_a1XZQ)
{-# INLINE labelConfig_hidden #-}
labelConfig_horizontal ::
  forall t_a1XHd. Lens' (LabelConfig t_a1XHd) (Active t_a1XHd Bool)
labelConfig_horizontal
  f_a1Y03
  (LabelConfig x1_a1Y04
               x2_a1Y05
               x3_a1Y06
               x4_a1Y07
               x5_a1Y08
               x6_a1Y09
               x7_a1Y0a
               x8_a1Y0b
               x9_a1Y0c
               x10_a1Y0d
               x11_a1Y0e
               x12_a1Y0f
               x13_a1Y0g)
  = fmap
      (\ y1_a1Y0h
         -> LabelConfig
              x1_a1Y04
              x2_a1Y05
              x3_a1Y06
              x4_a1Y07
              x5_a1Y08
              y1_a1Y0h
              x7_a1Y0a
              x8_a1Y0b
              x9_a1Y0c
              x10_a1Y0d
              x11_a1Y0e
              x12_a1Y0f
              x13_a1Y0g)
      (f_a1Y03 x6_a1Y09)
{-# INLINE labelConfig_horizontal #-}
labelConfig_image ::
  forall t_a1XHd. Lens' (LabelConfig t_a1XHd) (Active t_a1XHd Bool)
labelConfig_image
  f_a1Y0j
  (LabelConfig x1_a1Y0k
               x2_a1Y0l
               x3_a1Y0m
               x4_a1Y0n
               x5_a1Y0o
               x6_a1Y0p
               x7_a1Y0q
               x8_a1Y0r
               x9_a1Y0s
               x10_a1Y0t
               x11_a1Y0u
               x12_a1Y0v
               x13_a1Y0w)
  = fmap
      (\ y1_a1Y0x
         -> LabelConfig
              y1_a1Y0x
              x2_a1Y0l
              x3_a1Y0m
              x4_a1Y0n
              x5_a1Y0o
              x6_a1Y0p
              x7_a1Y0q
              x8_a1Y0r
              x9_a1Y0s
              x10_a1Y0t
              x11_a1Y0u
              x12_a1Y0v
              x13_a1Y0w)
      (f_a1Y0j x1_a1Y0k)
{-# INLINE labelConfig_image #-}
labelConfig_link ::
  forall t_a1XHd. Lens' (LabelConfig t_a1XHd) Bool
labelConfig_link
  f_a1Y0z
  (LabelConfig x1_a1Y0A
               x2_a1Y0B
               x3_a1Y0C
               x4_a1Y0D
               x5_a1Y0E
               x6_a1Y0F
               x7_a1Y0G
               x8_a1Y0H
               x9_a1Y0I
               x10_a1Y0J
               x11_a1Y0K
               x12_a1Y0L
               x13_a1Y0M)
  = fmap
      (\ y1_a1Y0N
         -> LabelConfig
              x1_a1Y0A
              x2_a1Y0B
              x3_a1Y0C
              x4_a1Y0D
              x5_a1Y0E
              x6_a1Y0F
              x7_a1Y0G
              x8_a1Y0H
              x9_a1Y0I
              x10_a1Y0J
              x11_a1Y0K
              y1_a1Y0N
              x13_a1Y0M)
      (f_a1Y0z x12_a1Y0L)
{-# INLINE labelConfig_link #-}
labelConfig_pointing ::
  forall t_a1XHd.
  Lens' (LabelConfig t_a1XHd) (Active t_a1XHd (Maybe Pointing))
labelConfig_pointing
  f_a1Y0O
  (LabelConfig x1_a1Y0P
               x2_a1Y0Q
               x3_a1Y0R
               x4_a1Y0S
               x5_a1Y0T
               x6_a1Y0U
               x7_a1Y0V
               x8_a1Y0W
               x9_a1Y0X
               x10_a1Y0Y
               x11_a1Y0Z
               x12_a1Y10
               x13_a1Y11)
  = fmap
      (\ y1_a1Y12
         -> LabelConfig
              x1_a1Y0P
              x2_a1Y0Q
              x3_a1Y0R
              x4_a1Y0S
              x5_a1Y0T
              x6_a1Y0U
              x7_a1Y0V
              x8_a1Y0W
              y1_a1Y12
              x10_a1Y0Y
              x11_a1Y0Z
              x12_a1Y10
              x13_a1Y11)
      (f_a1Y0O x9_a1Y0X)
{-# INLINE labelConfig_pointing #-}
labelConfig_ribbon ::
  forall t_a1XHd.
  Lens' (LabelConfig t_a1XHd) (Active t_a1XHd (Maybe Ribbon))
labelConfig_ribbon
  f_a1Y14
  (LabelConfig x1_a1Y15
               x2_a1Y16
               x3_a1Y17
               x4_a1Y18
               x5_a1Y19
               x6_a1Y1a
               x7_a1Y1b
               x8_a1Y1c
               x9_a1Y1d
               x10_a1Y1e
               x11_a1Y1f
               x12_a1Y1g
               x13_a1Y1h)
  = fmap
      (\ y1_a1Y1i
         -> LabelConfig
              x1_a1Y15
              x2_a1Y16
              x3_a1Y17
              x4_a1Y18
              x5_a1Y19
              x6_a1Y1a
              x7_a1Y1b
              x8_a1Y1c
              x9_a1Y1d
              y1_a1Y1i
              x11_a1Y1f
              x12_a1Y1g
              x13_a1Y1h)
      (f_a1Y14 x10_a1Y1e)
{-# INLINE labelConfig_ribbon #-}
labelConfig_tag ::
  forall t_a1XHd. Lens' (LabelConfig t_a1XHd) (Active t_a1XHd Bool)
labelConfig_tag
  f_a1Y1B
  (LabelConfig x1_a1Y1C
               x2_a1Y1D
               x3_a1Y1E
               x4_a1Y1F
               x5_a1Y1G
               x6_a1Y1H
               x7_a1Y1I
               x8_a1Y1K
               x9_a1Y1L
               x10_a1Y1M
               x11_a1Y1N
               x12_a1Y1P
               x13_a1Y1R)
  = fmap
      (\ y1_a1Y1S
         -> LabelConfig
              x1_a1Y1C
              x2_a1Y1D
              x3_a1Y1E
              y1_a1Y1S
              x5_a1Y1G
              x6_a1Y1H
              x7_a1Y1I
              x8_a1Y1K
              x9_a1Y1L
              x10_a1Y1M
              x11_a1Y1N
              x12_a1Y1P
              x13_a1Y1R)
      (f_a1Y1B x4_a1Y1F)
{-# INLINE labelConfig_tag #-}
