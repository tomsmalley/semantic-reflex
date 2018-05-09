-- src/Reflex/Dom/SemanticUI/Input.hs:93:1-63: Splicing declarations
inputConfig_action ::
  forall t_a1BV6.
  Lens' (InputConfig t_a1BV6) (Active t_a1BV6 (Maybe InputAction))
inputConfig_action
  f_a1C1o
  (InputConfig x1_a1C1p
               x2_a1C1q
               x3_a1C1r
               x4_a1C1s
               x5_a1C1t
               x6_a1C1u
               x7_a1C1v
               x8_a1C1w
               x9_a1C1x
               x10_a1C1y
               x11_a1C1z)
  = fmap
      (\ y1_a1C1A
         -> InputConfig
              x1_a1C1p
              x2_a1C1q
              x3_a1C1r
              x4_a1C1s
              x5_a1C1t
              x6_a1C1u
              x7_a1C1v
              x8_a1C1w
              y1_a1C1A
              x10_a1C1y
              x11_a1C1z)
      (f_a1C1o x9_a1C1x)
{-# INLINE inputConfig_action #-}
inputConfig_disabled ::
  forall t_a1BV6. Lens' (InputConfig t_a1BV6) (Active t_a1BV6 Bool)
inputConfig_disabled
  f_a1C1C
  (InputConfig x1_a1C1H
               x2_a1C1M
               x3_a1C1N
               x4_a1C1O
               x5_a1C1P
               x6_a1C1Q
               x7_a1C1R
               x8_a1C1S
               x9_a1C1T
               x10_a1C1U
               x11_a1C1V)
  = fmap
      (\ y1_a1C1W
         -> InputConfig
              x1_a1C1H
              y1_a1C1W
              x3_a1C1N
              x4_a1C1O
              x5_a1C1P
              x6_a1C1Q
              x7_a1C1R
              x8_a1C1S
              x9_a1C1T
              x10_a1C1U
              x11_a1C1V)
      (f_a1C1C x2_a1C1M)
{-# INLINE inputConfig_disabled #-}
inputConfig_elConfig ::
  forall t_a1BV6.
  Lens' (InputConfig t_a1BV6) (ActiveElConfig t_a1BV6)
inputConfig_elConfig
  f_a1C1Z
  (InputConfig x1_a1C20
               x2_a1C21
               x3_a1C22
               x4_a1C23
               x5_a1C24
               x6_a1C25
               x7_a1C26
               x8_a1C27
               x9_a1C28
               x10_a1C29
               x11_a1C2a)
  = fmap
      (\ y1_a1C2b
         -> InputConfig
              x1_a1C20
              x2_a1C21
              x3_a1C22
              x4_a1C23
              x5_a1C24
              x6_a1C25
              x7_a1C26
              x8_a1C27
              x9_a1C28
              x10_a1C29
              y1_a1C2b)
      (f_a1C1Z x11_a1C2a)
{-# INLINE inputConfig_elConfig #-}
inputConfig_error ::
  forall t_a1BV6. Lens' (InputConfig t_a1BV6) (Active t_a1BV6 Bool)
inputConfig_error
  f_a1C2c
  (InputConfig x1_a1C2d
               x2_a1C2e
               x3_a1C2f
               x4_a1C2g
               x5_a1C2h
               x6_a1C2k
               x7_a1C2l
               x8_a1C2m
               x9_a1C2n
               x10_a1C2o
               x11_a1C2p)
  = fmap
      (\ y1_a1C2q
         -> InputConfig
              x1_a1C2d
              x2_a1C2e
              y1_a1C2q
              x4_a1C2g
              x5_a1C2h
              x6_a1C2k
              x7_a1C2l
              x8_a1C2m
              x9_a1C2n
              x10_a1C2o
              x11_a1C2p)
      (f_a1C2c x3_a1C2f)
{-# INLINE inputConfig_error #-}
inputConfig_fluid ::
  forall t_a1BV6. Lens' (InputConfig t_a1BV6) (Active t_a1BV6 Bool)
inputConfig_fluid
  f_a1C2v
  (InputConfig x1_a1C2w
               x2_a1C2x
               x3_a1C2y
               x4_a1C2z
               x5_a1C2A
               x6_a1C2B
               x7_a1C2C
               x8_a1C2D
               x9_a1C2E
               x10_a1C2F
               x11_a1C2G)
  = fmap
      (\ y1_a1C2H
         -> InputConfig
              x1_a1C2w
              x2_a1C2x
              x3_a1C2y
              x4_a1C2z
              x5_a1C2A
              y1_a1C2H
              x7_a1C2C
              x8_a1C2D
              x9_a1C2E
              x10_a1C2F
              x11_a1C2G)
      (f_a1C2v x6_a1C2B)
{-# INLINE inputConfig_fluid #-}
inputConfig_icon ::
  forall t_a1BV6.
  Lens' (InputConfig t_a1BV6) (Active t_a1BV6 (Maybe InputIcon))
inputConfig_icon
  f_a1C2I
  (InputConfig x1_a1C2J
               x2_a1C2K
               x3_a1C2L
               x4_a1C2M
               x5_a1C2N
               x6_a1C2O
               x7_a1C2P
               x8_a1C2Q
               x9_a1C2R
               x10_a1C2S
               x11_a1C2T)
  = fmap
      (\ y1_a1C2U
         -> InputConfig
              x1_a1C2J
              x2_a1C2K
              x3_a1C2L
              x4_a1C2M
              x5_a1C2N
              x6_a1C2O
              y1_a1C2U
              x8_a1C2Q
              x9_a1C2R
              x10_a1C2S
              x11_a1C2T)
      (f_a1C2I x7_a1C2P)
{-# INLINE inputConfig_icon #-}
inputConfig_inverted ::
  forall t_a1BV6. Lens' (InputConfig t_a1BV6) (Active t_a1BV6 Bool)
inputConfig_inverted
  f_a1C2V
  (InputConfig x1_a1C2W
               x2_a1C2X
               x3_a1C2Y
               x4_a1C2Z
               x5_a1C30
               x6_a1C31
               x7_a1C32
               x8_a1C33
               x9_a1C34
               x10_a1C35
               x11_a1C36)
  = fmap
      (\ y1_a1C37
         -> InputConfig
              x1_a1C2W
              x2_a1C2X
              x3_a1C2Y
              x4_a1C2Z
              y1_a1C37
              x6_a1C31
              x7_a1C32
              x8_a1C33
              x9_a1C34
              x10_a1C35
              x11_a1C36)
      (f_a1C2V x5_a1C30)
{-# INLINE inputConfig_inverted #-}
inputConfig_labeled ::
  forall t_a1BV6.
  Lens' (InputConfig t_a1BV6) (Active t_a1BV6 (Maybe Labeled))
inputConfig_labeled
  f_a1C38
  (InputConfig x1_a1C39
               x2_a1C3a
               x3_a1C3b
               x4_a1C3c
               x5_a1C3d
               x6_a1C3e
               x7_a1C3f
               x8_a1C3g
               x9_a1C3h
               x10_a1C3j
               x11_a1C3k)
  = fmap
      (\ y1_a1C3l
         -> InputConfig
              x1_a1C39
              x2_a1C3a
              x3_a1C3b
              x4_a1C3c
              x5_a1C3d
              x6_a1C3e
              x7_a1C3f
              y1_a1C3l
              x9_a1C3h
              x10_a1C3j
              x11_a1C3k)
      (f_a1C38 x8_a1C3g)
{-# INLINE inputConfig_labeled #-}
inputConfig_loading ::
  forall t_a1BV6. Lens' (InputConfig t_a1BV6) (Active t_a1BV6 Bool)
inputConfig_loading
  f_a1C3m
  (InputConfig x1_a1C3n
               x2_a1C3o
               x3_a1C3p
               x4_a1C3q
               x5_a1C3r
               x6_a1C3s
               x7_a1C3t
               x8_a1C3u
               x9_a1C3v
               x10_a1C3w
               x11_a1C3x)
  = fmap
      (\ y1_a1C3y
         -> InputConfig
              y1_a1C3y
              x2_a1C3o
              x3_a1C3p
              x4_a1C3q
              x5_a1C3r
              x6_a1C3s
              x7_a1C3t
              x8_a1C3u
              x9_a1C3v
              x10_a1C3w
              x11_a1C3x)
      (f_a1C3m x1_a1C3n)
{-# INLINE inputConfig_loading #-}
inputConfig_size ::
  forall t_a1BV6.
  Lens' (InputConfig t_a1BV6) (Active t_a1BV6 (Maybe Size))
inputConfig_size
  f_a1C3A
  (InputConfig x1_a1C3B
               x2_a1C3C
               x3_a1C3D
               x4_a1C3E
               x5_a1C3F
               x6_a1C3G
               x7_a1C3H
               x8_a1C3I
               x9_a1C3J
               x10_a1C3K
               x11_a1C3L)
  = fmap
      (\ y1_a1C3M
         -> InputConfig
              x1_a1C3B
              x2_a1C3C
              x3_a1C3D
              x4_a1C3E
              x5_a1C3F
              x6_a1C3G
              x7_a1C3H
              x8_a1C3I
              x9_a1C3J
              y1_a1C3M
              x11_a1C3L)
      (f_a1C3A x10_a1C3K)
{-# INLINE inputConfig_size #-}
inputConfig_transparent ::
  forall t_a1BV6. Lens' (InputConfig t_a1BV6) (Active t_a1BV6 Bool)
inputConfig_transparent
  f_a1C3P
  (InputConfig x1_a1C3Q
               x2_a1C3R
               x3_a1C3S
               x4_a1C3T
               x5_a1C3U
               x6_a1C3V
               x7_a1C3W
               x8_a1C3X
               x9_a1C3Y
               x10_a1C3Z
               x11_a1C40)
  = fmap
      (\ y1_a1C41
         -> InputConfig
              x1_a1C3Q
              x2_a1C3R
              x3_a1C3S
              y1_a1C41
              x5_a1C3U
              x6_a1C3V
              x7_a1C3W
              x8_a1C3X
              x9_a1C3Y
              x10_a1C3Z
              x11_a1C40)
      (f_a1C3P x4_a1C3T)
{-# INLINE inputConfig_transparent #-}
-- src/Reflex/Dom/SemanticUI/Input.hs:167:1-67: Splicing declarations
textInputConfig_attrs ::
  forall t_a1Cc3.
  Lens' (TextInputConfig t_a1Cc3) (Dynamic t_a1Cc3 (Map Text Text))
textInputConfig_attrs
  f_a1FRj
  (TextInputConfig x1_a1FRk x2_a1FRl x3_a1FRm x4_a1FRn)
  = fmap
      (\ y1_a1FRo -> TextInputConfig x1_a1FRk x2_a1FRl x3_a1FRm y1_a1FRo)
      (f_a1FRj x4_a1FRn)
{-# INLINE textInputConfig_attrs #-}
textInputConfig_placeholder ::
  forall t_a1Cc3.
  Lens' (TextInputConfig t_a1Cc3) (Dynamic t_a1Cc3 Text)
textInputConfig_placeholder
  f_a1FRp
  (TextInputConfig x1_a1FRq x2_a1FRr x3_a1FRs x4_a1FRt)
  = fmap
      (\ y1_a1FRu -> TextInputConfig x1_a1FRq y1_a1FRu x3_a1FRs x4_a1FRt)
      (f_a1FRp x2_a1FRr)
{-# INLINE textInputConfig_placeholder #-}
textInputConfig_type ::
  forall t_a1Cc3. Lens' (TextInputConfig t_a1Cc3) InputType
textInputConfig_type
  f_a1FRx
  (TextInputConfig x1_a1FRy x2_a1FRz x3_a1FRA x4_a1FRB)
  = fmap
      (\ y1_a1FRC -> TextInputConfig x1_a1FRy x2_a1FRz y1_a1FRC x4_a1FRB)
      (f_a1FRx x3_a1FRA)
{-# INLINE textInputConfig_type #-}
textInputConfig_value ::
  forall t_a1Cc3.
  Lens' (TextInputConfig t_a1Cc3) (SetValue t_a1Cc3 Text)
textInputConfig_value
  f_a1FRD
  (TextInputConfig x1_a1FRE x2_a1FRF x3_a1FRG x4_a1FRH)
  = fmap
      (\ y1_a1FRI -> TextInputConfig y1_a1FRI x2_a1FRF x3_a1FRG x4_a1FRH)
      (f_a1FRD x1_a1FRE)
{-# INLINE textInputConfig_value #-}
