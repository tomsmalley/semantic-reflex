-- src/Reflex/Dom/SemanticUI/Icon.hs:79:1-62: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''IconConfig
--   ======>
iconConfig_bordered ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX Bool)
iconConfig_bordered
  f_a3k7s
  (IconConfig x1_a3k7t
              x2_a3k7u
              x3_a3k7v
              x4_a3k7w
              x5_a3k7x
              x6_a3k7y
              x7_a3k7z
              x8_a3k7A
              x9_a3k7B
              x10_a3k7C
              x11_a3k7D
              x12_a3k7E
              x13_a3k7F
              x14_a3k7G)
  = fmap
      (\ y1_a3k7H
         -> IconConfig
              x1_a3k7t
              x2_a3k7u
              x3_a3k7v
              x4_a3k7w
              x5_a3k7x
              y1_a3k7H
              x7_a3k7z
              x8_a3k7A
              x9_a3k7B
              x10_a3k7C
              x11_a3k7D
              x12_a3k7E
              x13_a3k7F
              x14_a3k7G)
      (f_a3k7s x6_a3k7y)
{-# INLINE iconConfig_bordered #-}
iconConfig_circular ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX Bool)
iconConfig_circular
  f_a3k7I
  (IconConfig x1_a3k7J
              x2_a3k7K
              x3_a3k7L
              x4_a3k7M
              x5_a3k7N
              x6_a3k7O
              x7_a3k7P
              x8_a3k7Q
              x9_a3k7R
              x10_a3k7S
              x11_a3k7T
              x12_a3k7U
              x13_a3k7V
              x14_a3k7W)
  = fmap
      (\ y1_a3k7X
         -> IconConfig
              x1_a3k7J
              x2_a3k7K
              x3_a3k7L
              x4_a3k7M
              y1_a3k7X
              x6_a3k7O
              x7_a3k7P
              x8_a3k7Q
              x9_a3k7R
              x10_a3k7S
              x11_a3k7T
              x12_a3k7U
              x13_a3k7V
              x14_a3k7W)
      (f_a3k7I x5_a3k7N)
{-# INLINE iconConfig_circular #-}
iconConfig_color ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX (Maybe Color))
iconConfig_color
  f_a3k7Y
  (IconConfig x1_a3k7Z
              x2_a3k80
              x3_a3k81
              x4_a3k82
              x5_a3k83
              x6_a3k84
              x7_a3k85
              x8_a3k86
              x9_a3k87
              x10_a3k88
              x11_a3k89
              x12_a3k8a
              x13_a3k8b
              x14_a3k8c)
  = fmap
      (\ y1_a3k8d
         -> IconConfig
              x1_a3k7Z
              x2_a3k80
              x3_a3k81
              x4_a3k82
              x5_a3k83
              x6_a3k84
              x7_a3k85
              x8_a3k86
              x9_a3k87
              x10_a3k88
              y1_a3k8d
              x12_a3k8a
              x13_a3k8b
              x14_a3k8c)
      (f_a3k7Y x11_a3k89)
{-# INLINE iconConfig_color #-}
iconConfig_corner ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX (Maybe Corner))
iconConfig_corner
  f_a3k8e
  (IconConfig x1_a3k8f
              x2_a3k8g
              x3_a3k8h
              x4_a3k8i
              x5_a3k8j
              x6_a3k8k
              x7_a3k8l
              x8_a3k8m
              x9_a3k8n
              x10_a3k8o
              x11_a3k8p
              x12_a3k8q
              x13_a3k8r
              x14_a3k8s)
  = fmap
      (\ y1_a3k8t
         -> IconConfig
              x1_a3k8f
              x2_a3k8g
              x3_a3k8h
              x4_a3k8i
              x5_a3k8j
              x6_a3k8k
              x7_a3k8l
              x8_a3k8m
              x9_a3k8n
              x10_a3k8o
              x11_a3k8p
              y1_a3k8t
              x13_a3k8r
              x14_a3k8s)
      (f_a3k8e x12_a3k8q)
{-# INLINE iconConfig_corner #-}
iconConfig_disabled ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX Bool)
iconConfig_disabled
  f_a3k8u
  (IconConfig x1_a3k8v
              x2_a3k8w
              x3_a3k8x
              x4_a3k8y
              x5_a3k8z
              x6_a3k8A
              x7_a3k8B
              x8_a3k8C
              x9_a3k8D
              x10_a3k8E
              x11_a3k8F
              x12_a3k8G
              x13_a3k8H
              x14_a3k8I)
  = fmap
      (\ y1_a3k8J
         -> IconConfig
              y1_a3k8J
              x2_a3k8w
              x3_a3k8x
              x4_a3k8y
              x5_a3k8z
              x6_a3k8A
              x7_a3k8B
              x8_a3k8C
              x9_a3k8D
              x10_a3k8E
              x11_a3k8F
              x12_a3k8G
              x13_a3k8H
              x14_a3k8I)
      (f_a3k8u x1_a3k8v)
{-# INLINE iconConfig_disabled #-}
iconConfig_elConfig ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (ActiveElConfig t_a3jQX)
iconConfig_elConfig
  f_a3k8K
  (IconConfig x1_a3k8L
              x2_a3k8M
              x3_a3k8N
              x4_a3k8O
              x5_a3k8P
              x6_a3k8Q
              x7_a3k8R
              x8_a3k8S
              x9_a3k8T
              x10_a3k8U
              x11_a3k8V
              x12_a3k8W
              x13_a3k8X
              x14_a3k8Y)
  = fmap
      (\ y1_a3k8Z
         -> IconConfig
              x1_a3k8L
              x2_a3k8M
              x3_a3k8N
              x4_a3k8O
              x5_a3k8P
              x6_a3k8Q
              x7_a3k8R
              x8_a3k8S
              x9_a3k8T
              x10_a3k8U
              x11_a3k8V
              x12_a3k8W
              x13_a3k8X
              y1_a3k8Z)
      (f_a3k8K x14_a3k8Y)
{-# INLINE iconConfig_elConfig #-}
iconConfig_fitted ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX Bool)
iconConfig_fitted
  f_a3k90
  (IconConfig x1_a3k91
              x2_a3k92
              x3_a3k93
              x4_a3k94
              x5_a3k95
              x6_a3k96
              x7_a3k97
              x8_a3k98
              x9_a3k99
              x10_a3k9a
              x11_a3k9b
              x12_a3k9c
              x13_a3k9d
              x14_a3k9e)
  = fmap
      (\ y1_a3k9f
         -> IconConfig
              x1_a3k91
              x2_a3k92
              y1_a3k9f
              x4_a3k94
              x5_a3k95
              x6_a3k96
              x7_a3k97
              x8_a3k98
              x9_a3k99
              x10_a3k9a
              x11_a3k9b
              x12_a3k9c
              x13_a3k9d
              x14_a3k9e)
      (f_a3k90 x3_a3k93)
{-# INLINE iconConfig_fitted #-}
iconConfig_flipped ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX (Maybe Flipped))
iconConfig_flipped
  f_a3k9g
  (IconConfig x1_a3k9h
              x2_a3k9i
              x3_a3k9j
              x4_a3k9k
              x5_a3k9l
              x6_a3k9m
              x7_a3k9n
              x8_a3k9o
              x9_a3k9p
              x10_a3k9q
              x11_a3k9r
              x12_a3k9s
              x13_a3k9t
              x14_a3k9u)
  = fmap
      (\ y1_a3k9v
         -> IconConfig
              x1_a3k9h
              x2_a3k9i
              x3_a3k9j
              x4_a3k9k
              x5_a3k9l
              x6_a3k9m
              x7_a3k9n
              x8_a3k9o
              y1_a3k9v
              x10_a3k9q
              x11_a3k9r
              x12_a3k9s
              x13_a3k9t
              x14_a3k9u)
      (f_a3k9g x9_a3k9p)
{-# INLINE iconConfig_flipped #-}
iconConfig_inverted ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX Bool)
iconConfig_inverted
  f_a3k9w
  (IconConfig x1_a3k9x
              x2_a3k9y
              x3_a3k9z
              x4_a3k9A
              x5_a3k9B
              x6_a3k9C
              x7_a3k9D
              x8_a3k9E
              x9_a3k9F
              x10_a3k9G
              x11_a3k9H
              x12_a3k9I
              x13_a3k9J
              x14_a3k9K)
  = fmap
      (\ y1_a3k9L
         -> IconConfig
              x1_a3k9x
              x2_a3k9y
              x3_a3k9z
              x4_a3k9A
              x5_a3k9B
              x6_a3k9C
              y1_a3k9L
              x8_a3k9E
              x9_a3k9F
              x10_a3k9G
              x11_a3k9H
              x12_a3k9I
              x13_a3k9J
              x14_a3k9K)
      (f_a3k9w x7_a3k9D)
{-# INLINE iconConfig_inverted #-}
iconConfig_link ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX Bool)
iconConfig_link
  f_a3k9M
  (IconConfig x1_a3k9N
              x2_a3k9O
              x3_a3k9P
              x4_a3k9Q
              x5_a3k9R
              x6_a3k9S
              x7_a3k9T
              x8_a3k9U
              x9_a3k9V
              x10_a3k9W
              x11_a3k9X
              x12_a3k9Y
              x13_a3k9Z
              x14_a3ka0)
  = fmap
      (\ y1_a3ka1
         -> IconConfig
              x1_a3k9N
              x2_a3k9O
              x3_a3k9P
              y1_a3ka1
              x5_a3k9R
              x6_a3k9S
              x7_a3k9T
              x8_a3k9U
              x9_a3k9V
              x10_a3k9W
              x11_a3k9X
              x12_a3k9Y
              x13_a3k9Z
              x14_a3ka0)
      (f_a3k9M x4_a3k9Q)
{-# INLINE iconConfig_link #-}
iconConfig_loading ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX Bool)
iconConfig_loading
  f_a3ka2
  (IconConfig x1_a3ka3
              x2_a3ka4
              x3_a3ka5
              x4_a3ka6
              x5_a3ka7
              x6_a3ka8
              x7_a3ka9
              x8_a3kaa
              x9_a3kab
              x10_a3kac
              x11_a3kad
              x12_a3kae
              x13_a3kaf
              x14_a3kag)
  = fmap
      (\ y1_a3kah
         -> IconConfig
              x1_a3ka3
              y1_a3kah
              x3_a3ka5
              x4_a3ka6
              x5_a3ka7
              x6_a3ka8
              x7_a3ka9
              x8_a3kaa
              x9_a3kab
              x10_a3kac
              x11_a3kad
              x12_a3kae
              x13_a3kaf
              x14_a3kag)
      (f_a3ka2 x2_a3ka4)
{-# INLINE iconConfig_loading #-}
iconConfig_rotated ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX (Maybe Rotated))
iconConfig_rotated
  f_a3kai
  (IconConfig x1_a3kaj
              x2_a3kak
              x3_a3kal
              x4_a3kam
              x5_a3kan
              x6_a3kao
              x7_a3kap
              x8_a3kaq
              x9_a3kar
              x10_a3kas
              x11_a3kat
              x12_a3kau
              x13_a3kav
              x14_a3kaw)
  = fmap
      (\ y1_a3kax
         -> IconConfig
              x1_a3kaj
              x2_a3kak
              x3_a3kal
              x4_a3kam
              x5_a3kan
              x6_a3kao
              x7_a3kap
              x8_a3kaq
              x9_a3kar
              y1_a3kax
              x11_a3kat
              x12_a3kau
              x13_a3kav
              x14_a3kaw)
      (f_a3kai x10_a3kas)
{-# INLINE iconConfig_rotated #-}
iconConfig_size ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX (Maybe Size))
iconConfig_size
  f_a3kay
  (IconConfig x1_a3kaz
              x2_a3kaA
              x3_a3kaB
              x4_a3kaC
              x5_a3kaD
              x6_a3kaE
              x7_a3kaF
              x8_a3kaG
              x9_a3kaH
              x10_a3kaI
              x11_a3kaJ
              x12_a3kaK
              x13_a3kaL
              x14_a3kaM)
  = fmap
      (\ y1_a3kaN
         -> IconConfig
              x1_a3kaz
              x2_a3kaA
              x3_a3kaB
              x4_a3kaC
              x5_a3kaD
              x6_a3kaE
              x7_a3kaF
              y1_a3kaN
              x9_a3kaH
              x10_a3kaI
              x11_a3kaJ
              x12_a3kaK
              x13_a3kaL
              x14_a3kaM)
      (f_a3kay x8_a3kaG)
{-# INLINE iconConfig_size #-}
iconConfig_title ::
  forall t_a3jQX.
  Control.Lens.Type.Lens' (IconConfig t_a3jQX) (Active t_a3jQX (Maybe Text))
iconConfig_title
  f_a3kaO
  (IconConfig x1_a3kaP
              x2_a3kaQ
              x3_a3kaR
              x4_a3kaS
              x5_a3kaT
              x6_a3kaU
              x7_a3kaV
              x8_a3kaW
              x9_a3kaX
              x10_a3kaY
              x11_a3kaZ
              x12_a3kb0
              x13_a3kb1
              x14_a3kb2)
  = fmap
      (\ y1_a3kb3
         -> IconConfig
              x1_a3kaP
              x2_a3kaQ
              x3_a3kaR
              x4_a3kaS
              x5_a3kaT
              x6_a3kaU
              x7_a3kaV
              x8_a3kaW
              x9_a3kaX
              x10_a3kaY
              x11_a3kaZ
              x12_a3kb0
              y1_a3kb3
              x14_a3kb2)
      (f_a3kaO x13_a3kb1)
{-# INLINE iconConfig_title #-}
-- src/Reflex/Dom/SemanticUI/Icon.hs:156:1-63: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''IconsConfig
--   ======>
iconsConfig_elConfig ::
  forall t_a3kb4.
  Control.Lens.Type.Lens' (IconsConfig t_a3kb4) (ActiveElConfig t_a3kb4)
iconsConfig_elConfig f_a3kvu (IconsConfig x1_a3kvv x2_a3kvw)
  = fmap
      (\ y1_a3kvx -> IconsConfig x1_a3kvv y1_a3kvx) (f_a3kvu x2_a3kvw)
{-# INLINE iconsConfig_elConfig #-}
iconsConfig_size ::
  forall t_a3kb4.
  Control.Lens.Type.Lens' (IconsConfig t_a3kb4) (Active t_a3kb4 (Maybe Size))
iconsConfig_size f_a3kvy (IconsConfig x1_a3kvz x2_a3kvA)
  = fmap
      (\ y1_a3kvB -> IconsConfig y1_a3kvB x2_a3kvA) (f_a3kvy x1_a3kvz)
{-# INLINE iconsConfig_size #-}
