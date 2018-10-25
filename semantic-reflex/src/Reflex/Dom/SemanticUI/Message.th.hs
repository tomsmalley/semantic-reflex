-- src/Reflex/Dom/SemanticUI/Message.hs:72:1-65: Splicing declarations
messageConfig_attached ::
  forall t_a2RoC.
  Lens' (MessageConfig t_a2RoC) (Active t_a2RoC (Maybe VerticalAttached))
messageConfig_attached
  f_a2RBf
  (MessageConfig x1_a2RBg
                 x2_a2RBh
                 x3_a2RBi
                 x4_a2RBj
                 x5_a2RBk
                 x6_a2RBl
                 x7_a2RBm
                 x8_a2RBn)
  = fmap
      (\ y1_a2RBo
         -> MessageConfig
              x1_a2RBg
              x2_a2RBh
              y1_a2RBo
              x4_a2RBj
              x5_a2RBk
              x6_a2RBl
              x7_a2RBm
              x8_a2RBn)
      (f_a2RBf x3_a2RBi)
{-# INLINE messageConfig_attached #-}
messageConfig_color ::
  forall t_a2RoC.
  Lens' (MessageConfig t_a2RoC) (Active t_a2RoC (Maybe Color))
messageConfig_color
  f_a2RBs
  (MessageConfig x1_a2RBt
                 x2_a2RBu
                 x3_a2RBv
                 x4_a2RBw
                 x5_a2RBx
                 x6_a2RBz
                 x7_a2RBA
                 x8_a2RBB)
  = fmap
      (\ y1_a2RBC
         -> MessageConfig
              x1_a2RBt
              x2_a2RBu
              x3_a2RBv
              x4_a2RBw
              y1_a2RBC
              x6_a2RBz
              x7_a2RBA
              x8_a2RBB)
      (f_a2RBs x5_a2RBx)
{-# INLINE messageConfig_color #-}
messageConfig_compact ::
  forall t_a2RoC. Lens' (MessageConfig t_a2RoC) (Active t_a2RoC Bool)
messageConfig_compact
  f_a2RBH
  (MessageConfig x1_a2RBJ
                 x2_a2RBK
                 x3_a2RBL
                 x4_a2RBM
                 x5_a2RBN
                 x6_a2RBO
                 x7_a2RBP
                 x8_a2RBQ)
  = fmap
      (\ y1_a2RBS
         -> MessageConfig
              x1_a2RBJ
              y1_a2RBS
              x3_a2RBL
              x4_a2RBM
              x5_a2RBN
              x6_a2RBO
              x7_a2RBP
              x8_a2RBQ)
      (f_a2RBH x2_a2RBK)
{-# INLINE messageConfig_compact #-}
messageConfig_elConfig ::
  forall t_a2RoC.
  Lens' (MessageConfig t_a2RoC) (ActiveElConfig t_a2RoC)
messageConfig_elConfig
  f_a2RBW
  (MessageConfig x1_a2RBX
                 x2_a2RBZ
                 x3_a2RC0
                 x4_a2RC1
                 x5_a2RC3
                 x6_a2RC4
                 x7_a2RC5
                 x8_a2RC6)
  = fmap
      (\ y1_a2RC7
         -> MessageConfig
              x1_a2RBX
              x2_a2RBZ
              x3_a2RC0
              x4_a2RC1
              x5_a2RC3
              x6_a2RC4
              x7_a2RC5
              y1_a2RC7)
      (f_a2RBW x8_a2RC6)
{-# INLINE messageConfig_elConfig #-}
messageConfig_floating ::
  forall t_a2RoC. Lens' (MessageConfig t_a2RoC) (Active t_a2RoC Bool)
messageConfig_floating
  f_a2RCc
  (MessageConfig x1_a2RCd
                 x2_a2RCf
                 x3_a2RCg
                 x4_a2RCh
                 x5_a2RCi
                 x6_a2RCj
                 x7_a2RCk
                 x8_a2RCm)
  = fmap
      (\ y1_a2RCn
         -> MessageConfig
              y1_a2RCn
              x2_a2RCf
              x3_a2RCg
              x4_a2RCh
              x5_a2RCi
              x6_a2RCj
              x7_a2RCk
              x8_a2RCm)
      (f_a2RCc x1_a2RCd)
{-# INLINE messageConfig_floating #-}
messageConfig_icon ::
  forall t_a2RoC.
  Lens' (MessageConfig t_a2RoC) (Maybe (Icon t_a2RoC))
messageConfig_icon
  f_a2RCp
  (MessageConfig x1_a2RCq
                 x2_a2RCr
                 x3_a2RCs
                 x4_a2RCt
                 x5_a2RCu
                 x6_a2RCw
                 x7_a2RCx
                 x8_a2RCy)
  = fmap
      (\ y1_a2RCz
         -> MessageConfig
              x1_a2RCq
              x2_a2RCr
              x3_a2RCs
              x4_a2RCt
              x5_a2RCu
              x6_a2RCw
              y1_a2RCz
              x8_a2RCy)
      (f_a2RCp x7_a2RCx)
{-# INLINE messageConfig_icon #-}
messageConfig_size ::
  forall t_a2RoC.
  Lens' (MessageConfig t_a2RoC) (Active t_a2RoC (Maybe Size))
messageConfig_size
  f_a2RCA
  (MessageConfig x1_a2RCB
                 x2_a2RCC
                 x3_a2RCD
                 x4_a2RCE
                 x5_a2RCF
                 x6_a2RCG
                 x7_a2RCH
                 x8_a2RCI)
  = fmap
      (\ y1_a2RCJ
         -> MessageConfig
              x1_a2RCB
              x2_a2RCC
              x3_a2RCD
              x4_a2RCE
              x5_a2RCF
              y1_a2RCJ
              x7_a2RCH
              x8_a2RCI)
      (f_a2RCA x6_a2RCG)
{-# INLINE messageConfig_size #-}
messageConfig_type ::
  forall t_a2RoC.
  Lens' (MessageConfig t_a2RoC) (Active t_a2RoC (Maybe MessageType))
messageConfig_type
  f_a2RCK
  (MessageConfig x1_a2RCL
                 x2_a2RCM
                 x3_a2RCN
                 x4_a2RCO
                 x5_a2RCP
                 x6_a2RCQ
                 x7_a2RCR
                 x8_a2RCS)
  = fmap
      (\ y1_a2RCT
         -> MessageConfig
              x1_a2RCL
              x2_a2RCM
              x3_a2RCN
              y1_a2RCT
              x5_a2RCP
              x6_a2RCQ
              x7_a2RCR
              x8_a2RCS)
      (f_a2RCK x4_a2RCO)
{-# INLINE messageConfig_type #-}
