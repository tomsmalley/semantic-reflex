-- src/Reflex/Dom/SemanticUI/Input.hs:88:1-63: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''InputConfig
--   ======>
inputConfig_action ::
  forall t_a3A0L.
  Control.Lens.Type.Lens' (InputConfig t_a3A0L) (Active t_a3A0L (Maybe InputAction))
inputConfig_action
  f_a3A2s
  (InputConfig x1_a3A2t
               x2_a3A2u
               x3_a3A2v
               x4_a3A2w
               x5_a3A2x
               x6_a3A2y
               x7_a3A2z
               x8_a3A2A
               x9_a3A2B
               x10_a3A2C
               x11_a3A2D)
  = fmap
      (\ y1_a3A2E
         -> InputConfig
              x1_a3A2t
              x2_a3A2u
              x3_a3A2v
              x4_a3A2w
              x5_a3A2x
              x6_a3A2y
              x7_a3A2z
              x8_a3A2A
              y1_a3A2E
              x10_a3A2C
              x11_a3A2D)
      (f_a3A2s x9_a3A2B)
{-# INLINE inputConfig_action #-}
inputConfig_disabled ::
  forall t_a3A0L.
  Control.Lens.Type.Lens' (InputConfig t_a3A0L) (Active t_a3A0L Bool)
inputConfig_disabled
  f_a3A2F
  (InputConfig x1_a3A2G
               x2_a3A2H
               x3_a3A2I
               x4_a3A2J
               x5_a3A2K
               x6_a3A2L
               x7_a3A2M
               x8_a3A2N
               x9_a3A2O
               x10_a3A2P
               x11_a3A2Q)
  = fmap
      (\ y1_a3A2R
         -> InputConfig
              x1_a3A2G
              y1_a3A2R
              x3_a3A2I
              x4_a3A2J
              x5_a3A2K
              x6_a3A2L
              x7_a3A2M
              x8_a3A2N
              x9_a3A2O
              x10_a3A2P
              x11_a3A2Q)
      (f_a3A2F x2_a3A2H)
{-# INLINE inputConfig_disabled #-}
inputConfig_elConfig ::
  forall t_a3A0L.
  Control.Lens.Type.Lens' (InputConfig t_a3A0L) (ActiveElConfig t_a3A0L)
inputConfig_elConfig
  f_a3A2S
  (InputConfig x1_a3A2T
               x2_a3A2U
               x3_a3A2V
               x4_a3A2W
               x5_a3A2X
               x6_a3A2Y
               x7_a3A2Z
               x8_a3A30
               x9_a3A31
               x10_a3A32
               x11_a3A33)
  = fmap
      (\ y1_a3A34
         -> InputConfig
              x1_a3A2T
              x2_a3A2U
              x3_a3A2V
              x4_a3A2W
              x5_a3A2X
              x6_a3A2Y
              x7_a3A2Z
              x8_a3A30
              x9_a3A31
              x10_a3A32
              y1_a3A34)
      (f_a3A2S x11_a3A33)
{-# INLINE inputConfig_elConfig #-}
inputConfig_error ::
  forall t_a3A0L.
  Control.Lens.Type.Lens' (InputConfig t_a3A0L) (Active t_a3A0L Bool)
inputConfig_error
  f_a3A35
  (InputConfig x1_a3A36
               x2_a3A37
               x3_a3A38
               x4_a3A39
               x5_a3A3a
               x6_a3A3b
               x7_a3A3c
               x8_a3A3d
               x9_a3A3e
               x10_a3A3f
               x11_a3A3g)
  = fmap
      (\ y1_a3A3h
         -> InputConfig
              x1_a3A36
              x2_a3A37
              y1_a3A3h
              x4_a3A39
              x5_a3A3a
              x6_a3A3b
              x7_a3A3c
              x8_a3A3d
              x9_a3A3e
              x10_a3A3f
              x11_a3A3g)
      (f_a3A35 x3_a3A38)
{-# INLINE inputConfig_error #-}
inputConfig_fluid ::
  forall t_a3A0L.
  Control.Lens.Type.Lens' (InputConfig t_a3A0L) (Active t_a3A0L Bool)
inputConfig_fluid
  f_a3A3i
  (InputConfig x1_a3A3j
               x2_a3A3k
               x3_a3A3l
               x4_a3A3m
               x5_a3A3n
               x6_a3A3o
               x7_a3A3p
               x8_a3A3q
               x9_a3A3r
               x10_a3A3s
               x11_a3A3t)
  = fmap
      (\ y1_a3A3u
         -> InputConfig
              x1_a3A3j
              x2_a3A3k
              x3_a3A3l
              x4_a3A3m
              x5_a3A3n
              y1_a3A3u
              x7_a3A3p
              x8_a3A3q
              x9_a3A3r
              x10_a3A3s
              x11_a3A3t)
      (f_a3A3i x6_a3A3o)
{-# INLINE inputConfig_fluid #-}
inputConfig_icon ::
  forall t_a3A0L.
  Control.Lens.Type.Lens' (InputConfig t_a3A0L) (Active t_a3A0L (Maybe InputIcon))
inputConfig_icon
  f_a3A3v
  (InputConfig x1_a3A3w
               x2_a3A3x
               x3_a3A3y
               x4_a3A3z
               x5_a3A3A
               x6_a3A3B
               x7_a3A3C
               x8_a3A3D
               x9_a3A3E
               x10_a3A3F
               x11_a3A3G)
  = fmap
      (\ y1_a3A3H
         -> InputConfig
              x1_a3A3w
              x2_a3A3x
              x3_a3A3y
              x4_a3A3z
              x5_a3A3A
              x6_a3A3B
              y1_a3A3H
              x8_a3A3D
              x9_a3A3E
              x10_a3A3F
              x11_a3A3G)
      (f_a3A3v x7_a3A3C)
{-# INLINE inputConfig_icon #-}
inputConfig_inverted ::
  forall t_a3A0L.
  Control.Lens.Type.Lens' (InputConfig t_a3A0L) (Active t_a3A0L Bool)
inputConfig_inverted
  f_a3A3I
  (InputConfig x1_a3A3J
               x2_a3A3K
               x3_a3A3L
               x4_a3A3M
               x5_a3A3N
               x6_a3A3O
               x7_a3A3P
               x8_a3A3Q
               x9_a3A3R
               x10_a3A3S
               x11_a3A3T)
  = fmap
      (\ y1_a3A3U
         -> InputConfig
              x1_a3A3J
              x2_a3A3K
              x3_a3A3L
              x4_a3A3M
              y1_a3A3U
              x6_a3A3O
              x7_a3A3P
              x8_a3A3Q
              x9_a3A3R
              x10_a3A3S
              x11_a3A3T)
      (f_a3A3I x5_a3A3N)
{-# INLINE inputConfig_inverted #-}
inputConfig_labeled ::
  forall t_a3A0L.
  Control.Lens.Type.Lens' (InputConfig t_a3A0L) (Active t_a3A0L (Maybe Labeled))
inputConfig_labeled
  f_a3A3V
  (InputConfig x1_a3A3W
               x2_a3A3X
               x3_a3A3Y
               x4_a3A3Z
               x5_a3A40
               x6_a3A41
               x7_a3A42
               x8_a3A43
               x9_a3A44
               x10_a3A45
               x11_a3A46)
  = fmap
      (\ y1_a3A47
         -> InputConfig
              x1_a3A3W
              x2_a3A3X
              x3_a3A3Y
              x4_a3A3Z
              x5_a3A40
              x6_a3A41
              x7_a3A42
              y1_a3A47
              x9_a3A44
              x10_a3A45
              x11_a3A46)
      (f_a3A3V x8_a3A43)
{-# INLINE inputConfig_labeled #-}
inputConfig_loading ::
  forall t_a3A0L.
  Control.Lens.Type.Lens' (InputConfig t_a3A0L) (Active t_a3A0L Bool)
inputConfig_loading
  f_a3A48
  (InputConfig x1_a3A49
               x2_a3A4a
               x3_a3A4b
               x4_a3A4c
               x5_a3A4d
               x6_a3A4e
               x7_a3A4f
               x8_a3A4g
               x9_a3A4h
               x10_a3A4i
               x11_a3A4j)
  = fmap
      (\ y1_a3A4k
         -> InputConfig
              y1_a3A4k
              x2_a3A4a
              x3_a3A4b
              x4_a3A4c
              x5_a3A4d
              x6_a3A4e
              x7_a3A4f
              x8_a3A4g
              x9_a3A4h
              x10_a3A4i
              x11_a3A4j)
      (f_a3A48 x1_a3A49)
{-# INLINE inputConfig_loading #-}
inputConfig_size ::
  forall t_a3A0L.
  Control.Lens.Type.Lens' (InputConfig t_a3A0L) (Active t_a3A0L (Maybe Size))
inputConfig_size
  f_a3A4l
  (InputConfig x1_a3A4m
               x2_a3A4n
               x3_a3A4o
               x4_a3A4p
               x5_a3A4q
               x6_a3A4r
               x7_a3A4s
               x8_a3A4t
               x9_a3A4u
               x10_a3A4v
               x11_a3A4w)
  = fmap
      (\ y1_a3A4x
         -> InputConfig
              x1_a3A4m
              x2_a3A4n
              x3_a3A4o
              x4_a3A4p
              x5_a3A4q
              x6_a3A4r
              x7_a3A4s
              x8_a3A4t
              x9_a3A4u
              y1_a3A4x
              x11_a3A4w)
      (f_a3A4l x10_a3A4v)
{-# INLINE inputConfig_size #-}
inputConfig_transparent ::
  forall t_a3A0L.
  Control.Lens.Type.Lens' (InputConfig t_a3A0L) (Active t_a3A0L Bool)
inputConfig_transparent
  f_a3A4y
  (InputConfig x1_a3A4z
               x2_a3A4A
               x3_a3A4B
               x4_a3A4C
               x5_a3A4D
               x6_a3A4E
               x7_a3A4F
               x8_a3A4G
               x9_a3A4H
               x10_a3A4I
               x11_a3A4J)
  = fmap
      (\ y1_a3A4K
         -> InputConfig
              x1_a3A4z
              x2_a3A4A
              x3_a3A4B
              y1_a3A4K
              x5_a3A4D
              x6_a3A4E
              x7_a3A4F
              x8_a3A4G
              x9_a3A4H
              x10_a3A4I
              x11_a3A4J)
      (f_a3A4y x4_a3A4C)
{-# INLINE inputConfig_transparent #-}
-- src/Reflex/Dom/SemanticUI/Input.hs:160:1-67: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''TextInputConfig
--   ======>
textInputConfig_attrs ::
  forall t_a3A4L.
  Control.Lens.Type.Lens' (TextInputConfig t_a3A4L) (Dynamic t_a3A4L (Map Text Text))
textInputConfig_attrs
  f_a3AnJ
  (TextInputConfig x1_a3AnK x2_a3AnL x3_a3AnM x4_a3AnN)
  = fmap
      (\ y1_a3AnO -> TextInputConfig x1_a3AnK x2_a3AnL x3_a3AnM y1_a3AnO)
      (f_a3AnJ x4_a3AnN)
{-# INLINE textInputConfig_attrs #-}
textInputConfig_placeholder ::
  forall t_a3A4L.
  Control.Lens.Type.Lens' (TextInputConfig t_a3A4L) (Dynamic t_a3A4L Text)
textInputConfig_placeholder
  f_a3AnP
  (TextInputConfig x1_a3AnQ x2_a3AnR x3_a3AnS x4_a3AnT)
  = fmap
      (\ y1_a3AnU -> TextInputConfig x1_a3AnQ y1_a3AnU x3_a3AnS x4_a3AnT)
      (f_a3AnP x2_a3AnR)
{-# INLINE textInputConfig_placeholder #-}
textInputConfig_type ::
  forall t_a3A4L.
  Control.Lens.Type.Lens' (TextInputConfig t_a3A4L) InputType
textInputConfig_type
  f_a3AnV
  (TextInputConfig x1_a3AnW x2_a3AnX x3_a3AnY x4_a3AnZ)
  = fmap
      (\ y1_a3Ao0 -> TextInputConfig x1_a3AnW x2_a3AnX y1_a3Ao0 x4_a3AnZ)
      (f_a3AnV x3_a3AnY)
{-# INLINE textInputConfig_type #-}
textInputConfig_value ::
  forall t_a3A4L.
  Control.Lens.Type.Lens' (TextInputConfig t_a3A4L) (SetValue t_a3A4L Text)
textInputConfig_value
  f_a3Ao1
  (TextInputConfig x1_a3Ao2 x2_a3Ao3 x3_a3Ao4 x4_a3Ao5)
  = fmap
      (\ y1_a3Ao6 -> TextInputConfig y1_a3Ao6 x2_a3Ao3 x3_a3Ao4 x4_a3Ao5)
      (f_a3Ao1 x1_a3Ao2)
{-# INLINE textInputConfig_value #-}

