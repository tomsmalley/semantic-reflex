-- src/Reflex/Dom/SemanticUI/Label.hs:79:1-26: Splicing declarations
--     makeLenses ''LabelAttached
--   ======>
labelAttached_horizontally ::
  Control.Lens.Type.Lens' LabelAttached (Maybe HorizontalAttached)
labelAttached_horizontally
  f_a3MvH
  (LabelAttached x1_a3MvI x2_a3MvJ)
  = fmap
      (\ y1_a3MvK -> LabelAttached x1_a3MvI y1_a3MvK) (f_a3MvH x2_a3MvJ)
{-# INLINE labelAttached_horizontally #-}
labelAttached_vertically ::
  Control.Lens.Type.Lens' LabelAttached VerticalAttached
labelAttached_vertically f_a3MvL (LabelAttached x1_a3MvM x2_a3MvN)
  = fmap
      (\ y1_a3MvO -> LabelAttached y1_a3MvO x2_a3MvN) (f_a3MvL x1_a3MvM)
{-# INLINE labelAttached_vertically #-}
-- src/Reflex/Dom/SemanticUI/Label.hs:113:1-63: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''LabelConfig
--   ======>
labelConfig_attached ::
  forall t_a3MvP.
  Control.Lens.Type.Lens' (LabelConfig t_a3MvP) (Active t_a3MvP (Maybe LabelAttached))
labelConfig_attached
  f_a3MyY
  (LabelConfig x1_a3MyZ
               x2_a3Mz0
               x3_a3Mz1
               x4_a3Mz2
               x5_a3Mz3
               x6_a3Mz4
               x7_a3Mz5
               x8_a3Mz6
               x9_a3Mz7
               x10_a3Mz8
               x11_a3Mz9
               x12_a3Mza
               x13_a3Mzb)
  = fmap
      (\ y1_a3Mzc
         -> LabelConfig
              x1_a3MyZ
              x2_a3Mz0
              x3_a3Mz1
              x4_a3Mz2
              x5_a3Mz3
              x6_a3Mz4
              y1_a3Mzc
              x8_a3Mz6
              x9_a3Mz7
              x10_a3Mz8
              x11_a3Mz9
              x12_a3Mza
              x13_a3Mzb)
      (f_a3MyY x7_a3Mz5)
{-# INLINE labelConfig_attached #-}
labelConfig_basic ::
  forall t_a3MvP.
  Control.Lens.Type.Lens' (LabelConfig t_a3MvP) (Active t_a3MvP Bool)
labelConfig_basic
  f_a3Mzd
  (LabelConfig x1_a3Mze
               x2_a3Mzf
               x3_a3Mzg
               x4_a3Mzh
               x5_a3Mzi
               x6_a3Mzj
               x7_a3Mzk
               x8_a3Mzl
               x9_a3Mzm
               x10_a3Mzn
               x11_a3Mzo
               x12_a3Mzp
               x13_a3Mzq)
  = fmap
      (\ y1_a3Mzr
         -> LabelConfig
              x1_a3Mze
              x2_a3Mzf
              y1_a3Mzr
              x4_a3Mzh
              x5_a3Mzi
              x6_a3Mzj
              x7_a3Mzk
              x8_a3Mzl
              x9_a3Mzm
              x10_a3Mzn
              x11_a3Mzo
              x12_a3Mzp
              x13_a3Mzq)
      (f_a3Mzd x3_a3Mzg)
{-# INLINE labelConfig_basic #-}
labelConfig_color ::
  forall t_a3MvP.
  Control.Lens.Type.Lens' (LabelConfig t_a3MvP) (Active t_a3MvP (Maybe Color))
labelConfig_color
  f_a3Mzs
  (LabelConfig x1_a3Mzt
               x2_a3Mzu
               x3_a3Mzv
               x4_a3Mzw
               x5_a3Mzx
               x6_a3Mzy
               x7_a3Mzz
               x8_a3MzA
               x9_a3MzB
               x10_a3MzC
               x11_a3MzD
               x12_a3MzE
               x13_a3MzF)
  = fmap
      (\ y1_a3MzG
         -> LabelConfig
              x1_a3Mzt
              x2_a3Mzu
              x3_a3Mzv
              x4_a3Mzw
              x5_a3Mzx
              x6_a3Mzy
              x7_a3Mzz
              y1_a3MzG
              x9_a3MzB
              x10_a3MzC
              x11_a3MzD
              x12_a3MzE
              x13_a3MzF)
      (f_a3Mzs x8_a3MzA)
{-# INLINE labelConfig_color #-}
labelConfig_corner ::
  forall t_a3MvP.
  Control.Lens.Type.Lens' (LabelConfig t_a3MvP) (Active t_a3MvP (Maybe TopCorner))
labelConfig_corner
  f_a3MzH
  (LabelConfig x1_a3MzI
               x2_a3MzJ
               x3_a3MzK
               x4_a3MzL
               x5_a3MzM
               x6_a3MzN
               x7_a3MzO
               x8_a3MzP
               x9_a3MzQ
               x10_a3MzR
               x11_a3MzS
               x12_a3MzT
               x13_a3MzU)
  = fmap
      (\ y1_a3MzV
         -> LabelConfig
              x1_a3MzI
              x2_a3MzJ
              x3_a3MzK
              x4_a3MzL
              x5_a3MzM
              x6_a3MzN
              x7_a3MzO
              x8_a3MzP
              x9_a3MzQ
              x10_a3MzR
              y1_a3MzV
              x12_a3MzT
              x13_a3MzU)
      (f_a3MzH x11_a3MzS)
{-# INLINE labelConfig_corner #-}
labelConfig_elConfig ::
  forall t_a3MvP.
  Control.Lens.Type.Lens' (LabelConfig t_a3MvP) (ActiveElConfig t_a3MvP)
labelConfig_elConfig
  f_a3MzW
  (LabelConfig x1_a3MzX
               x2_a3MzY
               x3_a3MzZ
               x4_a3MA0
               x5_a3MA1
               x6_a3MA2
               x7_a3MA3
               x8_a3MA4
               x9_a3MA5
               x10_a3MA6
               x11_a3MA7
               x12_a3MA8
               x13_a3MA9)
  = fmap
      (\ y1_a3MAa
         -> LabelConfig
              x1_a3MzX
              x2_a3MzY
              x3_a3MzZ
              x4_a3MA0
              x5_a3MA1
              x6_a3MA2
              x7_a3MA3
              x8_a3MA4
              x9_a3MA5
              x10_a3MA6
              x11_a3MA7
              x12_a3MA8
              y1_a3MAa)
      (f_a3MzW x13_a3MA9)
{-# INLINE labelConfig_elConfig #-}
labelConfig_floating ::
  forall t_a3MvP.
  Control.Lens.Type.Lens' (LabelConfig t_a3MvP) (Active t_a3MvP Bool)
labelConfig_floating
  f_a3MAb
  (LabelConfig x1_a3MAc
               x2_a3MAd
               x3_a3MAe
               x4_a3MAf
               x5_a3MAg
               x6_a3MAh
               x7_a3MAi
               x8_a3MAj
               x9_a3MAk
               x10_a3MAl
               x11_a3MAm
               x12_a3MAn
               x13_a3MAo)
  = fmap
      (\ y1_a3MAp
         -> LabelConfig
              x1_a3MAc
              x2_a3MAd
              x3_a3MAe
              x4_a3MAf
              y1_a3MAp
              x6_a3MAh
              x7_a3MAi
              x8_a3MAj
              x9_a3MAk
              x10_a3MAl
              x11_a3MAm
              x12_a3MAn
              x13_a3MAo)
      (f_a3MAb x5_a3MAg)
{-# INLINE labelConfig_floating #-}
labelConfig_hidden ::
  forall t_a3MvP.
  Control.Lens.Type.Lens' (LabelConfig t_a3MvP) (Active t_a3MvP Bool)
labelConfig_hidden
  f_a3MAq
  (LabelConfig x1_a3MAr
               x2_a3MAs
               x3_a3MAt
               x4_a3MAu
               x5_a3MAv
               x6_a3MAw
               x7_a3MAx
               x8_a3MAy
               x9_a3MAz
               x10_a3MAA
               x11_a3MAB
               x12_a3MAC
               x13_a3MAD)
  = fmap
      (\ y1_a3MAE
         -> LabelConfig
              x1_a3MAr
              y1_a3MAE
              x3_a3MAt
              x4_a3MAu
              x5_a3MAv
              x6_a3MAw
              x7_a3MAx
              x8_a3MAy
              x9_a3MAz
              x10_a3MAA
              x11_a3MAB
              x12_a3MAC
              x13_a3MAD)
      (f_a3MAq x2_a3MAs)
{-# INLINE labelConfig_hidden #-}
labelConfig_horizontal ::
  forall t_a3MvP.
  Control.Lens.Type.Lens' (LabelConfig t_a3MvP) (Active t_a3MvP Bool)
labelConfig_horizontal
  f_a3MAF
  (LabelConfig x1_a3MAG
               x2_a3MAH
               x3_a3MAI
               x4_a3MAJ
               x5_a3MAK
               x6_a3MAL
               x7_a3MAM
               x8_a3MAN
               x9_a3MAO
               x10_a3MAP
               x11_a3MAQ
               x12_a3MAR
               x13_a3MAS)
  = fmap
      (\ y1_a3MAT
         -> LabelConfig
              x1_a3MAG
              x2_a3MAH
              x3_a3MAI
              x4_a3MAJ
              x5_a3MAK
              y1_a3MAT
              x7_a3MAM
              x8_a3MAN
              x9_a3MAO
              x10_a3MAP
              x11_a3MAQ
              x12_a3MAR
              x13_a3MAS)
      (f_a3MAF x6_a3MAL)
{-# INLINE labelConfig_horizontal #-}
labelConfig_image ::
  forall t_a3MvP.
  Control.Lens.Type.Lens' (LabelConfig t_a3MvP) (Active t_a3MvP Bool)
labelConfig_image
  f_a3MAU
  (LabelConfig x1_a3MAV
               x2_a3MAW
               x3_a3MAX
               x4_a3MAY
               x5_a3MAZ
               x6_a3MB0
               x7_a3MB1
               x8_a3MB2
               x9_a3MB3
               x10_a3MB4
               x11_a3MB5
               x12_a3MB6
               x13_a3MB7)
  = fmap
      (\ y1_a3MB8
         -> LabelConfig
              y1_a3MB8
              x2_a3MAW
              x3_a3MAX
              x4_a3MAY
              x5_a3MAZ
              x6_a3MB0
              x7_a3MB1
              x8_a3MB2
              x9_a3MB3
              x10_a3MB4
              x11_a3MB5
              x12_a3MB6
              x13_a3MB7)
      (f_a3MAU x1_a3MAV)
{-# INLINE labelConfig_image #-}
labelConfig_link ::
  forall t_a3MvP. Control.Lens.Type.Lens' (LabelConfig t_a3MvP) Bool
labelConfig_link
  f_a3MB9
  (LabelConfig x1_a3MBa
               x2_a3MBb
               x3_a3MBc
               x4_a3MBd
               x5_a3MBe
               x6_a3MBf
               x7_a3MBg
               x8_a3MBh
               x9_a3MBi
               x10_a3MBj
               x11_a3MBk
               x12_a3MBl
               x13_a3MBm)
  = fmap
      (\ y1_a3MBn
         -> LabelConfig
              x1_a3MBa
              x2_a3MBb
              x3_a3MBc
              x4_a3MBd
              x5_a3MBe
              x6_a3MBf
              x7_a3MBg
              x8_a3MBh
              x9_a3MBi
              x10_a3MBj
              x11_a3MBk
              y1_a3MBn
              x13_a3MBm)
      (f_a3MB9 x12_a3MBl)
{-# INLINE labelConfig_link #-}
labelConfig_pointing ::
  forall t_a3MvP.
  Control.Lens.Type.Lens' (LabelConfig t_a3MvP) (Active t_a3MvP (Maybe Pointing))
labelConfig_pointing
  f_a3MBo
  (LabelConfig x1_a3MBp
               x2_a3MBq
               x3_a3MBr
               x4_a3MBs
               x5_a3MBt
               x6_a3MBu
               x7_a3MBv
               x8_a3MBw
               x9_a3MBx
               x10_a3MBy
               x11_a3MBz
               x12_a3MBA
               x13_a3MBB)
  = fmap
      (\ y1_a3MBC
         -> LabelConfig
              x1_a3MBp
              x2_a3MBq
              x3_a3MBr
              x4_a3MBs
              x5_a3MBt
              x6_a3MBu
              x7_a3MBv
              x8_a3MBw
              y1_a3MBC
              x10_a3MBy
              x11_a3MBz
              x12_a3MBA
              x13_a3MBB)
      (f_a3MBo x9_a3MBx)
{-# INLINE labelConfig_pointing #-}
labelConfig_ribbon ::
  forall t_a3MvP.
  Control.Lens.Type.Lens' (LabelConfig t_a3MvP) (Active t_a3MvP (Maybe Ribbon))
labelConfig_ribbon
  f_a3MBD
  (LabelConfig x1_a3MBE
               x2_a3MBF
               x3_a3MBG
               x4_a3MBH
               x5_a3MBI
               x6_a3MBJ
               x7_a3MBK
               x8_a3MBL
               x9_a3MBM
               x10_a3MBN
               x11_a3MBO
               x12_a3MBP
               x13_a3MBQ)
  = fmap
      (\ y1_a3MBR
         -> LabelConfig
              x1_a3MBE
              x2_a3MBF
              x3_a3MBG
              x4_a3MBH
              x5_a3MBI
              x6_a3MBJ
              x7_a3MBK
              x8_a3MBL
              x9_a3MBM
              y1_a3MBR
              x11_a3MBO
              x12_a3MBP
              x13_a3MBQ)
      (f_a3MBD x10_a3MBN)
{-# INLINE labelConfig_ribbon #-}
labelConfig_tag ::
  forall t_a3MvP.
  Control.Lens.Type.Lens' (LabelConfig t_a3MvP) (Active t_a3MvP Bool)
labelConfig_tag
  f_a3MBS
  (LabelConfig x1_a3MBT
               x2_a3MBU
               x3_a3MBV
               x4_a3MBW
               x5_a3MBX
               x6_a3MBY
               x7_a3MBZ
               x8_a3MC0
               x9_a3MC1
               x10_a3MC2
               x11_a3MC3
               x12_a3MC4
               x13_a3MC5)
  = fmap
      (\ y1_a3MC6
         -> LabelConfig
              x1_a3MBT
              x2_a3MBU
              x3_a3MBV
              y1_a3MC6
              x5_a3MBX
              x6_a3MBY
              x7_a3MBZ
              x8_a3MC0
              x9_a3MC1
              x10_a3MC2
              x11_a3MC3
              x12_a3MC4
              x13_a3MC5)
      (f_a3MBS x4_a3MBW)
{-# INLINE labelConfig_tag #-}

