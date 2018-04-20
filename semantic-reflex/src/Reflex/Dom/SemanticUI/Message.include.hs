-- src/Reflex/Dom/SemanticUI/Message.hs:68:1-65: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''MessageConfig
--   ======>
messageConfig_attached ::
  forall t_a3r7c.
  Control.Lens.Type.Lens' (MessageConfig t_a3r7c) (Active t_a3r7c (Maybe VerticalAttached))
messageConfig_attached
  f_a3r9p
  (MessageConfig x1_a3r9q
                 x2_a3r9r
                 x3_a3r9s
                 x4_a3r9t
                 x5_a3r9u
                 x6_a3r9v
                 x7_a3r9w
                 x8_a3r9x)
  = fmap
      (\ y1_a3r9y
         -> MessageConfig
              x1_a3r9q
              x2_a3r9r
              y1_a3r9y
              x4_a3r9t
              x5_a3r9u
              x6_a3r9v
              x7_a3r9w
              x8_a3r9x)
      (f_a3r9p x3_a3r9s)
{-# INLINE messageConfig_attached #-}
messageConfig_color ::
  forall t_a3r7c.
  Control.Lens.Type.Lens' (MessageConfig t_a3r7c) (Active t_a3r7c (Maybe Color))
messageConfig_color
  f_a3r9z
  (MessageConfig x1_a3r9A
                 x2_a3r9B
                 x3_a3r9C
                 x4_a3r9D
                 x5_a3r9E
                 x6_a3r9F
                 x7_a3r9G
                 x8_a3r9H)
  = fmap
      (\ y1_a3r9I
         -> MessageConfig
              x1_a3r9A
              x2_a3r9B
              x3_a3r9C
              x4_a3r9D
              y1_a3r9I
              x6_a3r9F
              x7_a3r9G
              x8_a3r9H)
      (f_a3r9z x5_a3r9E)
{-# INLINE messageConfig_color #-}
messageConfig_compact ::
  forall t_a3r7c.
  Control.Lens.Type.Lens' (MessageConfig t_a3r7c) (Active t_a3r7c Bool)
messageConfig_compact
  f_a3r9J
  (MessageConfig x1_a3r9K
                 x2_a3r9L
                 x3_a3r9M
                 x4_a3r9N
                 x5_a3r9O
                 x6_a3r9P
                 x7_a3r9Q
                 x8_a3r9R)
  = fmap
      (\ y1_a3r9S
         -> MessageConfig
              x1_a3r9K
              y1_a3r9S
              x3_a3r9M
              x4_a3r9N
              x5_a3r9O
              x6_a3r9P
              x7_a3r9Q
              x8_a3r9R)
      (f_a3r9J x2_a3r9L)
{-# INLINE messageConfig_compact #-}
messageConfig_elConfig ::
  forall t_a3r7c.
  Control.Lens.Type.Lens' (MessageConfig t_a3r7c) (ActiveElConfig t_a3r7c)
messageConfig_elConfig
  f_a3r9T
  (MessageConfig x1_a3r9U
                 x2_a3r9V
                 x3_a3r9W
                 x4_a3r9X
                 x5_a3r9Y
                 x6_a3r9Z
                 x7_a3ra0
                 x8_a3ra1)
  = fmap
      (\ y1_a3ra2
         -> MessageConfig
              x1_a3r9U
              x2_a3r9V
              x3_a3r9W
              x4_a3r9X
              x5_a3r9Y
              x6_a3r9Z
              x7_a3ra0
              y1_a3ra2)
      (f_a3r9T x8_a3ra1)
{-# INLINE messageConfig_elConfig #-}
messageConfig_floating ::
  forall t_a3r7c.
  Control.Lens.Type.Lens' (MessageConfig t_a3r7c) (Active t_a3r7c Bool)
messageConfig_floating
  f_a3ra3
  (MessageConfig x1_a3ra4
                 x2_a3ra5
                 x3_a3ra6
                 x4_a3ra7
                 x5_a3ra8
                 x6_a3ra9
                 x7_a3raa
                 x8_a3rab)
  = fmap
      (\ y1_a3rac
         -> MessageConfig
              y1_a3rac
              x2_a3ra5
              x3_a3ra6
              x4_a3ra7
              x5_a3ra8
              x6_a3ra9
              x7_a3raa
              x8_a3rab)
      (f_a3ra3 x1_a3ra4)
{-# INLINE messageConfig_floating #-}
messageConfig_icon ::
  forall t_a3r7c.
  Control.Lens.Type.Lens' (MessageConfig t_a3r7c) (Maybe (Icon t_a3r7c))
messageConfig_icon
  f_a3rad
  (MessageConfig x1_a3rae
                 x2_a3raf
                 x3_a3rag
                 x4_a3rah
                 x5_a3rai
                 x6_a3raj
                 x7_a3rak
                 x8_a3ral)
  = fmap
      (\ y1_a3ram
         -> MessageConfig
              x1_a3rae
              x2_a3raf
              x3_a3rag
              x4_a3rah
              x5_a3rai
              x6_a3raj
              y1_a3ram
              x8_a3ral)
      (f_a3rad x7_a3rak)
{-# INLINE messageConfig_icon #-}
messageConfig_size ::
  forall t_a3r7c.
  Control.Lens.Type.Lens' (MessageConfig t_a3r7c) (Active t_a3r7c (Maybe Size))
messageConfig_size
  f_a3ran
  (MessageConfig x1_a3rao
                 x2_a3rap
                 x3_a3raq
                 x4_a3rar
                 x5_a3ras
                 x6_a3rat
                 x7_a3rau
                 x8_a3rav)
  = fmap
      (\ y1_a3raw
         -> MessageConfig
              x1_a3rao
              x2_a3rap
              x3_a3raq
              x4_a3rar
              x5_a3ras
              y1_a3raw
              x7_a3rau
              x8_a3rav)
      (f_a3ran x6_a3rat)
{-# INLINE messageConfig_size #-}
messageConfig_type ::
  forall t_a3r7c.
  Control.Lens.Type.Lens' (MessageConfig t_a3r7c) (Active t_a3r7c (Maybe MessageType))
messageConfig_type
  f_a3rax
  (MessageConfig x1_a3ray
                 x2_a3raz
                 x3_a3raA
                 x4_a3raB
                 x5_a3raC
                 x6_a3raD
                 x7_a3raE
                 x8_a3raF)
  = fmap
      (\ y1_a3raG
         -> MessageConfig
              x1_a3ray
              x2_a3raz
              x3_a3raA
              y1_a3raG
              x5_a3raC
              x6_a3raD
              x7_a3raE
              x8_a3raF)
      (f_a3rax x4_a3raB)
{-# INLINE messageConfig_type #-}

