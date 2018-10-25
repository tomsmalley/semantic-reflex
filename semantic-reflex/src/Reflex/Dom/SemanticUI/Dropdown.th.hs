-- src/Reflex/Dom/SemanticUI/Dropdown.hs:86:1-66: Splicing declarations
dropdownConfig_as ::
  forall t_a2Usx.
  Lens' (DropdownConfig t_a2Usx) (Dynamic t_a2Usx (Maybe DropdownStyle))
dropdownConfig_as
  f_a2V0V
  (DropdownConfig x1_a2V0W
                  x2_a2V0X
                  x3_a2V0Y
                  x4_a2V0Z
                  x5_a2V10
                  x6_a2V11
                  x7_a2V12
                  x8_a2V13
                  x9_a2V14
                  x10_a2V15
                  x11_a2V16)
  = fmap
      (\ y1_a2V18
         -> DropdownConfig
              x1_a2V0W
              x2_a2V0X
              x3_a2V0Y
              x4_a2V0Z
              x5_a2V10
              x6_a2V11
              y1_a2V18
              x8_a2V13
              x9_a2V14
              x10_a2V15
              x11_a2V16)
      (f_a2V0V x7_a2V12)
{-# INLINE dropdownConfig_as #-}
dropdownConfig_closeOnClickSelection ::
  forall t_a2Usx. Lens' (DropdownConfig t_a2Usx) Bool
dropdownConfig_closeOnClickSelection
  f_a2V1e
  (DropdownConfig x1_a2V1f
                  x2_a2V1g
                  x3_a2V1h
                  x4_a2V1i
                  x5_a2V1j
                  x6_a2V1k
                  x7_a2V1l
                  x8_a2V1m
                  x9_a2V1n
                  x10_a2V1o
                  x11_a2V1p)
  = fmap
      (\ y1_a2V1q
         -> DropdownConfig
              x1_a2V1f
              x2_a2V1g
              x3_a2V1h
              x4_a2V1i
              x5_a2V1j
              x6_a2V1k
              x7_a2V1l
              x8_a2V1m
              y1_a2V1q
              x10_a2V1o
              x11_a2V1p)
      (f_a2V1e x9_a2V1n)
{-# INLINE dropdownConfig_closeOnClickSelection #-}
dropdownConfig_compact ::
  forall t_a2Usx.
  Lens' (DropdownConfig t_a2Usx) (Dynamic t_a2Usx Bool)
dropdownConfig_compact
  f_a2V1s
  (DropdownConfig x1_a2V1t
                  x2_a2V1u
                  x3_a2V1x
                  x4_a2V1y
                  x5_a2V1B
                  x6_a2V1C
                  x7_a2V1D
                  x8_a2V1E
                  x9_a2V1F
                  x10_a2V1G
                  x11_a2V1H)
  = fmap
      (\ y1_a2V1I
         -> DropdownConfig
              x1_a2V1t
              x2_a2V1u
              y1_a2V1I
              x4_a2V1y
              x5_a2V1B
              x6_a2V1C
              x7_a2V1D
              x8_a2V1E
              x9_a2V1F
              x10_a2V1G
              x11_a2V1H)
      (f_a2V1s x3_a2V1x)
{-# INLINE dropdownConfig_compact #-}
dropdownConfig_elConfig ::
  forall t_a2Usx.
  Lens' (DropdownConfig t_a2Usx) (ActiveElConfig t_a2Usx)
dropdownConfig_elConfig
  f_a2V1J
  (DropdownConfig x1_a2V1K
                  x2_a2V1L
                  x3_a2V1M
                  x4_a2V1O
                  x5_a2V1P
                  x6_a2V1Q
                  x7_a2V1R
                  x8_a2V1S
                  x9_a2V1T
                  x10_a2V1U
                  x11_a2V1V)
  = fmap
      (\ y1_a2V1W
         -> DropdownConfig
              x1_a2V1K
              x2_a2V1L
              x3_a2V1M
              x4_a2V1O
              x5_a2V1P
              x6_a2V1Q
              x7_a2V1R
              x8_a2V1S
              x9_a2V1T
              x10_a2V1U
              y1_a2V1W)
      (f_a2V1J x11_a2V1V)
{-# INLINE dropdownConfig_elConfig #-}
dropdownConfig_fluid ::
  forall t_a2Usx.
  Lens' (DropdownConfig t_a2Usx) (Dynamic t_a2Usx Bool)
dropdownConfig_fluid
  f_a2V21
  (DropdownConfig x1_a2V22
                  x2_a2V23
                  x3_a2V24
                  x4_a2V25
                  x5_a2V26
                  x6_a2V27
                  x7_a2V28
                  x8_a2V29
                  x9_a2V2a
                  x10_a2V2b
                  x11_a2V2c)
  = fmap
      (\ y1_a2V2d
         -> DropdownConfig
              x1_a2V22
              x2_a2V23
              x3_a2V24
              y1_a2V2d
              x5_a2V26
              x6_a2V27
              x7_a2V28
              x8_a2V29
              x9_a2V2a
              x10_a2V2b
              x11_a2V2c)
      (f_a2V21 x4_a2V25)
{-# INLINE dropdownConfig_fluid #-}
dropdownConfig_inline ::
  forall t_a2Usx.
  Lens' (DropdownConfig t_a2Usx) (Dynamic t_a2Usx Bool)
dropdownConfig_inline
  f_a2V2e
  (DropdownConfig x1_a2V2f
                  x2_a2V2g
                  x3_a2V2h
                  x4_a2V2i
                  x5_a2V2j
                  x6_a2V2k
                  x7_a2V2l
                  x8_a2V2m
                  x9_a2V2n
                  x10_a2V2o
                  x11_a2V2p)
  = fmap
      (\ y1_a2V2q
         -> DropdownConfig
              x1_a2V2f
              x2_a2V2g
              x3_a2V2h
              x4_a2V2i
              x5_a2V2j
              y1_a2V2q
              x7_a2V2l
              x8_a2V2m
              x9_a2V2n
              x10_a2V2o
              x11_a2V2p)
      (f_a2V2e x6_a2V2k)
{-# INLINE dropdownConfig_inline #-}
dropdownConfig_item ::
  forall t_a2Usx.
  Lens' (DropdownConfig t_a2Usx) (Dynamic t_a2Usx Bool)
dropdownConfig_item
  f_a2V2s
  (DropdownConfig x1_a2V2t
                  x2_a2V2u
                  x3_a2V2v
                  x4_a2V2w
                  x5_a2V2x
                  x6_a2V2y
                  x7_a2V2z
                  x8_a2V2A
                  x9_a2V2B
                  x10_a2V2C
                  x11_a2V2D)
  = fmap
      (\ y1_a2V2E
         -> DropdownConfig
              x1_a2V2t
              x2_a2V2u
              x3_a2V2v
              x4_a2V2w
              y1_a2V2E
              x6_a2V2y
              x7_a2V2z
              x8_a2V2A
              x9_a2V2B
              x10_a2V2C
              x11_a2V2D)
      (f_a2V2s x5_a2V2x)
{-# INLINE dropdownConfig_item #-}
dropdownConfig_placeholder ::
  forall t_a2Usx.
  Lens' (DropdownConfig t_a2Usx) (Dynamic t_a2Usx Text)
dropdownConfig_placeholder
  f_a2V2F
  (DropdownConfig x1_a2V2G
                  x2_a2V2K
                  x3_a2V2R
                  x4_a2V2X
                  x5_a2V2Z
                  x6_a2V30
                  x7_a2V31
                  x8_a2V32
                  x9_a2V33
                  x10_a2V35
                  x11_a2V36)
  = fmap
      (\ y1_a2V37
         -> DropdownConfig
              y1_a2V37
              x2_a2V2K
              x3_a2V2R
              x4_a2V2X
              x5_a2V2Z
              x6_a2V30
              x7_a2V31
              x8_a2V32
              x9_a2V33
              x10_a2V35
              x11_a2V36)
      (f_a2V2F x1_a2V2G)
{-# INLINE dropdownConfig_placeholder #-}
dropdownConfig_searchFunction ::
  forall t_a2Usx.
  Lens' (DropdownConfig t_a2Usx) (Text -> Text -> Bool)
dropdownConfig_searchFunction
  f_a2V3a
  (DropdownConfig x1_a2V3b
                  x2_a2V3c
                  x3_a2V3d
                  x4_a2V3e
                  x5_a2V3f
                  x6_a2V3g
                  x7_a2V3h
                  x8_a2V3i
                  x9_a2V3j
                  x10_a2V3k
                  x11_a2V3m)
  = fmap
      (\ y1_a2V3n
         -> DropdownConfig
              x1_a2V3b
              x2_a2V3c
              x3_a2V3d
              x4_a2V3e
              x5_a2V3f
              x6_a2V3g
              x7_a2V3h
              x8_a2V3i
              x9_a2V3j
              y1_a2V3n
              x11_a2V3m)
      (f_a2V3a x10_a2V3k)
{-# INLINE dropdownConfig_searchFunction #-}
dropdownConfig_selection ::
  forall t_a2Usx.
  Lens' (DropdownConfig t_a2Usx) (Dynamic t_a2Usx Bool)
dropdownConfig_selection
  f_a2V3s
  (DropdownConfig x1_a2V3t
                  x2_a2V3u
                  x3_a2V3v
                  x4_a2V3w
                  x5_a2V3x
                  x6_a2V3z
                  x7_a2V3A
                  x8_a2V3B
                  x9_a2V3C
                  x10_a2V3D
                  x11_a2V3E)
  = fmap
      (\ y1_a2V3F
         -> DropdownConfig
              x1_a2V3t
              y1_a2V3F
              x3_a2V3v
              x4_a2V3w
              x5_a2V3x
              x6_a2V3z
              x7_a2V3A
              x8_a2V3B
              x9_a2V3C
              x10_a2V3D
              x11_a2V3E)
      (f_a2V3s x2_a2V3u)
{-# INLINE dropdownConfig_selection #-}
dropdownConfig_unselectable ::
  forall t_a2Usx. Lens' (DropdownConfig t_a2Usx) Bool
dropdownConfig_unselectable
  f_a2V3H
  (DropdownConfig x1_a2V3I
                  x2_a2V3J
                  x3_a2V3K
                  x4_a2V3L
                  x5_a2V3M
                  x6_a2V3N
                  x7_a2V3O
                  x8_a2V3P
                  x9_a2V3Q
                  x10_a2V3R
                  x11_a2V3U)
  = fmap
      (\ y1_a2V3X
         -> DropdownConfig
              x1_a2V3I
              x2_a2V3J
              x3_a2V3K
              x4_a2V3L
              x5_a2V3M
              x6_a2V3N
              x7_a2V3O
              y1_a2V3X
              x9_a2V3Q
              x10_a2V3R
              x11_a2V3U)
      (f_a2V3H x8_a2V3P)
{-# INLINE dropdownConfig_unselectable #-}
-- src/Reflex/Dom/SemanticUI/Dropdown.hs:144:1-60: Splicing declarations
dropdown_blur ::
  forall t_a2Vaz a_a2VaD.
  Lens' (Dropdown t_a2Vaz a_a2VaD) (Event t_a2Vaz ())
dropdown_blur f_a2VCo (Dropdown x1_a2VCp x2_a2VCq x3_a2VCr)
  = fmap
      (\ y1_a2VCs -> Dropdown x1_a2VCp y1_a2VCs x3_a2VCr)
      (f_a2VCo x2_a2VCq)
{-# INLINE dropdown_blur #-}
dropdown_element ::
  forall t_a2Vaz a_a2VaD.
  Lens' (Dropdown t_a2Vaz a_a2VaD) (El t_a2Vaz)
dropdown_element f_a2VCu (Dropdown x1_a2VCv x2_a2VCw x3_a2VCx)
  = fmap
      (\ y1_a2VCy -> Dropdown x1_a2VCv x2_a2VCw y1_a2VCy)
      (f_a2VCu x3_a2VCx)
{-# INLINE dropdown_element #-}
dropdown_value ::
  forall t_a2Vaz a_a2VaD.
  Lens' (Dropdown t_a2Vaz a_a2VaD) (Dynamic t_a2Vaz a_a2VaD)
dropdown_value f_a2VCz (Dropdown x1_a2VCA x2_a2VCB x3_a2VCC)
  = fmap
      (\ y1_a2VCE -> Dropdown y1_a2VCE x2_a2VCB x3_a2VCC)
      (f_a2VCz x1_a2VCA)
{-# INLINE dropdown_value #-}
