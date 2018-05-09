-- src/Reflex/Dom/SemanticUI/Message.hs:73:1-65: Splicing declarations
messageConfig_attached ::
  forall t_a2OSj.
  Lens' (MessageConfig t_a2OSj) (Active t_a2OSj (Maybe VerticalAttached))
messageConfig_attached
  f_a2OUJ
  (MessageConfig x1_a2OUK
                 x2_a2OUL
                 x3_a2OUM
                 x4_a2OUN
                 x5_a2OUO
                 x6_a2OUP
                 x7_a2OUQ
                 x8_a2OUR)
  = fmap
      (\ y1_a2OUS
         -> MessageConfig
              x1_a2OUK
              x2_a2OUL
              y1_a2OUS
              x4_a2OUN
              x5_a2OUO
              x6_a2OUP
              x7_a2OUQ
              x8_a2OUR)
      (f_a2OUJ x3_a2OUM)
{-# INLINE messageConfig_attached #-}
messageConfig_color ::
  forall t_a2OSj.
  Lens' (MessageConfig t_a2OSj) (Active t_a2OSj (Maybe Color))
messageConfig_color
  f_a2OUT
  (MessageConfig x1_a2OUU
                 x2_a2OUV
                 x3_a2OUW
                 x4_a2OUX
                 x5_a2OUY
                 x6_a2OUZ
                 x7_a2OV0
                 x8_a2OV1)
  = fmap
      (\ y1_a2OV2
         -> MessageConfig
              x1_a2OUU
              x2_a2OUV
              x3_a2OUW
              x4_a2OUX
              y1_a2OV2
              x6_a2OUZ
              x7_a2OV0
              x8_a2OV1)
      (f_a2OUT x5_a2OUY)
{-# INLINE messageConfig_color #-}
messageConfig_compact ::
  forall t_a2OSj. Lens' (MessageConfig t_a2OSj) (Active t_a2OSj Bool)
messageConfig_compact
  f_a2OV3
  (MessageConfig x1_a2OV4
                 x2_a2OV5
                 x3_a2OV6
                 x4_a2OV7
                 x5_a2OV8
                 x6_a2OV9
                 x7_a2OVa
                 x8_a2OVb)
  = fmap
      (\ y1_a2OVc
         -> MessageConfig
              x1_a2OV4
              y1_a2OVc
              x3_a2OV6
              x4_a2OV7
              x5_a2OV8
              x6_a2OV9
              x7_a2OVa
              x8_a2OVb)
      (f_a2OV3 x2_a2OV5)
{-# INLINE messageConfig_compact #-}
messageConfig_elConfig ::
  forall t_a2OSj.
  Lens' (MessageConfig t_a2OSj) (ActiveElConfig t_a2OSj)
messageConfig_elConfig
  f_a2OVd
  (MessageConfig x1_a2OVe
                 x2_a2OVf
                 x3_a2OVg
                 x4_a2OVh
                 x5_a2OVi
                 x6_a2OVj
                 x7_a2OVk
                 x8_a2OVl)
  = fmap
      (\ y1_a2OVm
         -> MessageConfig
              x1_a2OVe
              x2_a2OVf
              x3_a2OVg
              x4_a2OVh
              x5_a2OVi
              x6_a2OVj
              x7_a2OVk
              y1_a2OVm)
      (f_a2OVd x8_a2OVl)
{-# INLINE messageConfig_elConfig #-}
messageConfig_floating ::
  forall t_a2OSj. Lens' (MessageConfig t_a2OSj) (Active t_a2OSj Bool)
messageConfig_floating
  f_a2OVn
  (MessageConfig x1_a2OVo
                 x2_a2OVp
                 x3_a2OVq
                 x4_a2OVr
                 x5_a2OVs
                 x6_a2OVt
                 x7_a2OVu
                 x8_a2OVv)
  = fmap
      (\ y1_a2OVw
         -> MessageConfig
              y1_a2OVw
              x2_a2OVp
              x3_a2OVq
              x4_a2OVr
              x5_a2OVs
              x6_a2OVt
              x7_a2OVu
              x8_a2OVv)
      (f_a2OVn x1_a2OVo)
{-# INLINE messageConfig_floating #-}
messageConfig_icon ::
  forall t_a2OSj.
  Lens' (MessageConfig t_a2OSj) (Maybe (Icon t_a2OSj))
messageConfig_icon
  f_a2OVx
  (MessageConfig x1_a2OVy
                 x2_a2OVz
                 x3_a2OVA
                 x4_a2OVB
                 x5_a2OVC
                 x6_a2OVD
                 x7_a2OVE
                 x8_a2OVF)
  = fmap
      (\ y1_a2OVG
         -> MessageConfig
              x1_a2OVy
              x2_a2OVz
              x3_a2OVA
              x4_a2OVB
              x5_a2OVC
              x6_a2OVD
              y1_a2OVG
              x8_a2OVF)
      (f_a2OVx x7_a2OVE)
{-# INLINE messageConfig_icon #-}
messageConfig_size ::
  forall t_a2OSj.
  Lens' (MessageConfig t_a2OSj) (Active t_a2OSj (Maybe Size))
messageConfig_size
  f_a2OVH
  (MessageConfig x1_a2OVI
                 x2_a2OVJ
                 x3_a2OVK
                 x4_a2OVL
                 x5_a2OVM
                 x6_a2OVN
                 x7_a2OVO
                 x8_a2OVP)
  = fmap
      (\ y1_a2OVQ
         -> MessageConfig
              x1_a2OVI
              x2_a2OVJ
              x3_a2OVK
              x4_a2OVL
              x5_a2OVM
              y1_a2OVQ
              x7_a2OVO
              x8_a2OVP)
      (f_a2OVH x6_a2OVN)
{-# INLINE messageConfig_size #-}
messageConfig_type ::
  forall t_a2OSj.
  Lens' (MessageConfig t_a2OSj) (Active t_a2OSj (Maybe MessageType))
messageConfig_type
  f_a2OVR
  (MessageConfig x1_a2OVS
                 x2_a2OVT
                 x3_a2OVU
                 x4_a2OVV
                 x5_a2OVW
                 x6_a2OVX
                 x7_a2OVY
                 x8_a2OVZ)
  = fmap
      (\ y1_a2OW0
         -> MessageConfig
              x1_a2OVS
              x2_a2OVT
              x3_a2OVU
              y1_a2OW0
              x5_a2OVW
              x6_a2OVX
              x7_a2OVY
              x8_a2OVZ)
      (f_a2OVR x4_a2OVV)
{-# INLINE messageConfig_type #-}
