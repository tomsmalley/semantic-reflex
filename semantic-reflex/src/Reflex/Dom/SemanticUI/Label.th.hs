-- src/Reflex/Dom/SemanticUI/Label.hs:83:1-26: Splicing declarations
labelAttached_horizontally ::
  Lens' LabelAttached (Maybe HorizontalAttached)
labelAttached_horizontally
  f_a1UeQ
  (LabelAttached x1_a1UeS x2_a1UeT)
  = fmap
      (\ y1_a1UeU -> LabelAttached x1_a1UeS y1_a1UeU) (f_a1UeQ x2_a1UeT)
{-# INLINE labelAttached_horizontally #-}
labelAttached_vertically :: Lens' LabelAttached VerticalAttached
labelAttached_vertically f_a1Uf1 (LabelAttached x1_a1Uf3 x2_a1Uf4)
  = fmap
      (\ y1_a1Uf5 -> LabelAttached y1_a1Uf5 x2_a1Uf4) (f_a1Uf1 x1_a1Uf3)
{-# INLINE labelAttached_vertically #-}
-- src/Reflex/Dom/SemanticUI/Label.hs:119:1-63: Splicing declarations
labelConfig_attached ::
  forall t_a1Uk0.
  Lens' (LabelConfig t_a1Uk0) (Active t_a1Uk0 (Maybe LabelAttached))
labelConfig_attached
  f_a1UQT
  (LabelConfig x1_a1UQW
               x2_a1UQX
               x3_a1UQY
               x4_a1UQZ
               x5_a1UR0
               x6_a1UR1
               x7_a1UR2
               x8_a1UR3
               x9_a1UR4
               x10_a1UR5
               x11_a1UR7
               x12_a1UR8
               x13_a1UR9)
  = fmap
      (\ y1_a1URb
         -> LabelConfig
              x1_a1UQW
              x2_a1UQX
              x3_a1UQY
              x4_a1UQZ
              x5_a1UR0
              x6_a1UR1
              y1_a1URb
              x8_a1UR3
              x9_a1UR4
              x10_a1UR5
              x11_a1UR7
              x12_a1UR8
              x13_a1UR9)
      (f_a1UQT x7_a1UR2)
{-# INLINE labelConfig_attached #-}
labelConfig_basic ::
  forall t_a1Uk0. Lens' (LabelConfig t_a1Uk0) (Active t_a1Uk0 Bool)
labelConfig_basic
  f_a1URi
  (LabelConfig x1_a1URk
               x2_a1URm
               x3_a1URn
               x4_a1URo
               x5_a1URp
               x6_a1URq
               x7_a1URr
               x8_a1URs
               x9_a1URt
               x10_a1URv
               x11_a1URw
               x12_a1URx
               x13_a1URy)
  = fmap
      (\ y1_a1URz
         -> LabelConfig
              x1_a1URk
              x2_a1URm
              y1_a1URz
              x4_a1URo
              x5_a1URp
              x6_a1URq
              x7_a1URr
              x8_a1URs
              x9_a1URt
              x10_a1URv
              x11_a1URw
              x12_a1URx
              x13_a1URy)
      (f_a1URi x3_a1URn)
{-# INLINE labelConfig_basic #-}
labelConfig_color ::
  forall t_a1Uk0.
  Lens' (LabelConfig t_a1Uk0) (Active t_a1Uk0 (Maybe Color))
labelConfig_color
  f_a1URF
  (LabelConfig x1_a1URG
               x2_a1URH
               x3_a1URJ
               x4_a1URK
               x5_a1URM
               x6_a1URN
               x7_a1URO
               x8_a1URP
               x9_a1URQ
               x10_a1URR
               x11_a1URS
               x12_a1URT
               x13_a1URU)
  = fmap
      (\ y1_a1URW
         -> LabelConfig
              x1_a1URG
              x2_a1URH
              x3_a1URJ
              x4_a1URK
              x5_a1URM
              x6_a1URN
              x7_a1URO
              y1_a1URW
              x9_a1URQ
              x10_a1URR
              x11_a1URS
              x12_a1URT
              x13_a1URU)
      (f_a1URF x8_a1URP)
{-# INLINE labelConfig_color #-}
labelConfig_corner ::
  forall t_a1Uk0.
  Lens' (LabelConfig t_a1Uk0) (Active t_a1Uk0 (Maybe TopCorner))
labelConfig_corner
  f_a1US1
  (LabelConfig x1_a1US2
               x2_a1US3
               x3_a1US5
               x4_a1US6
               x5_a1US7
               x6_a1US9
               x7_a1USa
               x8_a1USb
               x9_a1USc
               x10_a1USd
               x11_a1USf
               x12_a1USg
               x13_a1USh)
  = fmap
      (\ y1_a1USi
         -> LabelConfig
              x1_a1US2
              x2_a1US3
              x3_a1US5
              x4_a1US6
              x5_a1US7
              x6_a1US9
              x7_a1USa
              x8_a1USb
              x9_a1USc
              x10_a1USd
              y1_a1USi
              x12_a1USg
              x13_a1USh)
      (f_a1US1 x11_a1USf)
{-# INLINE labelConfig_corner #-}
labelConfig_elConfig ::
  forall t_a1Uk0.
  Lens' (LabelConfig t_a1Uk0) (ActiveElConfig t_a1Uk0)
labelConfig_elConfig
  f_a1USo
  (LabelConfig x1_a1USp
               x2_a1USq
               x3_a1USr
               x4_a1USs
               x5_a1USt
               x6_a1USu
               x7_a1USv
               x8_a1USx
               x9_a1USz
               x10_a1USA
               x11_a1USB
               x12_a1USC
               x13_a1USD)
  = fmap
      (\ y1_a1USE
         -> LabelConfig
              x1_a1USp
              x2_a1USq
              x3_a1USr
              x4_a1USs
              x5_a1USt
              x6_a1USu
              x7_a1USv
              x8_a1USx
              x9_a1USz
              x10_a1USA
              x11_a1USB
              x12_a1USC
              y1_a1USE)
      (f_a1USo x13_a1USD)
{-# INLINE labelConfig_elConfig #-}
labelConfig_floating ::
  forall t_a1Uk0. Lens' (LabelConfig t_a1Uk0) (Active t_a1Uk0 Bool)
labelConfig_floating
  f_a1USK
  (LabelConfig x1_a1USL
               x2_a1USM
               x3_a1USN
               x4_a1USO
               x5_a1USP
               x6_a1USQ
               x7_a1USR
               x8_a1UST
               x9_a1USU
               x10_a1USV
               x11_a1USW
               x12_a1USY
               x13_a1USZ)
  = fmap
      (\ y1_a1UT0
         -> LabelConfig
              x1_a1USL
              x2_a1USM
              x3_a1USN
              x4_a1USO
              y1_a1UT0
              x6_a1USQ
              x7_a1USR
              x8_a1UST
              x9_a1USU
              x10_a1USV
              x11_a1USW
              x12_a1USY
              x13_a1USZ)
      (f_a1USK x5_a1USP)
{-# INLINE labelConfig_floating #-}
labelConfig_hidden ::
  forall t_a1Uk0. Lens' (LabelConfig t_a1Uk0) (Active t_a1Uk0 Bool)
labelConfig_hidden
  f_a1UT5
  (LabelConfig x1_a1UT7
               x2_a1UT8
               x3_a1UTa
               x4_a1UTb
               x5_a1UTc
               x6_a1UTd
               x7_a1UTe
               x8_a1UTf
               x9_a1UTg
               x10_a1UTh
               x11_a1UTi
               x12_a1UTj
               x13_a1UTl)
  = fmap
      (\ y1_a1UTn
         -> LabelConfig
              x1_a1UT7
              y1_a1UTn
              x3_a1UTa
              x4_a1UTb
              x5_a1UTc
              x6_a1UTd
              x7_a1UTe
              x8_a1UTf
              x9_a1UTg
              x10_a1UTh
              x11_a1UTi
              x12_a1UTj
              x13_a1UTl)
      (f_a1UT5 x2_a1UT8)
{-# INLINE labelConfig_hidden #-}
labelConfig_horizontal ::
  forall t_a1Uk0. Lens' (LabelConfig t_a1Uk0) (Active t_a1Uk0 Bool)
labelConfig_horizontal
  f_a1UTr
  (LabelConfig x1_a1UTt
               x2_a1UTv
               x3_a1UTw
               x4_a1UTx
               x5_a1UTy
               x6_a1UTz
               x7_a1UTA
               x8_a1UTB
               x9_a1UTC
               x10_a1UTE
               x11_a1UTF
               x12_a1UTG
               x13_a1UTH)
  = fmap
      (\ y1_a1UTI
         -> LabelConfig
              x1_a1UTt
              x2_a1UTv
              x3_a1UTw
              x4_a1UTx
              x5_a1UTy
              y1_a1UTI
              x7_a1UTA
              x8_a1UTB
              x9_a1UTC
              x10_a1UTE
              x11_a1UTF
              x12_a1UTG
              x13_a1UTH)
      (f_a1UTr x6_a1UTz)
{-# INLINE labelConfig_horizontal #-}
labelConfig_image ::
  forall t_a1Uk0. Lens' (LabelConfig t_a1Uk0) (Active t_a1Uk0 Bool)
labelConfig_image
  f_a1UTO
  (LabelConfig x1_a1UTP
               x2_a1UTQ
               x3_a1UTR
               x4_a1UTT
               x5_a1UTU
               x6_a1UTW
               x7_a1UTX
               x8_a1UTY
               x9_a1UTZ
               x10_a1UU0
               x11_a1UU1
               x12_a1UU2
               x13_a1UU3)
  = fmap
      (\ y1_a1UU5
         -> LabelConfig
              y1_a1UU5
              x2_a1UTQ
              x3_a1UTR
              x4_a1UTT
              x5_a1UTU
              x6_a1UTW
              x7_a1UTX
              x8_a1UTY
              x9_a1UTZ
              x10_a1UU0
              x11_a1UU1
              x12_a1UU2
              x13_a1UU3)
      (f_a1UTO x1_a1UTP)
{-# INLINE labelConfig_image #-}
labelConfig_link ::
  forall t_a1Uk0. Lens' (LabelConfig t_a1Uk0) Bool
labelConfig_link
  f_a1UUa
  (LabelConfig x1_a1UUb
               x2_a1UUc
               x3_a1UUe
               x4_a1UUf
               x5_a1UUg
               x6_a1UUi
               x7_a1UUj
               x8_a1UUk
               x9_a1UUl
               x10_a1UUm
               x11_a1UUn
               x12_a1UUp
               x13_a1UUq)
  = fmap
      (\ y1_a1UUr
         -> LabelConfig
              x1_a1UUb
              x2_a1UUc
              x3_a1UUe
              x4_a1UUf
              x5_a1UUg
              x6_a1UUi
              x7_a1UUj
              x8_a1UUk
              x9_a1UUl
              x10_a1UUm
              x11_a1UUn
              y1_a1UUr
              x13_a1UUq)
      (f_a1UUa x12_a1UUp)
{-# INLINE labelConfig_link #-}
labelConfig_pointing ::
  forall t_a1Uk0.
  Lens' (LabelConfig t_a1Uk0) (Active t_a1Uk0 (Maybe Pointing))
labelConfig_pointing
  f_a1UUw
  (LabelConfig x1_a1UUy
               x2_a1UUz
               x3_a1UUA
               x4_a1UUB
               x5_a1UUC
               x6_a1UUD
               x7_a1UUE
               x8_a1UUG
               x9_a1UUH
               x10_a1UUJ
               x11_a1UUK
               x12_a1UUL
               x13_a1UUM)
  = fmap
      (\ y1_a1UUN
         -> LabelConfig
              x1_a1UUy
              x2_a1UUz
              x3_a1UUA
              x4_a1UUB
              x5_a1UUC
              x6_a1UUD
              x7_a1UUE
              x8_a1UUG
              y1_a1UUN
              x10_a1UUJ
              x11_a1UUK
              x12_a1UUL
              x13_a1UUM)
      (f_a1UUw x9_a1UUH)
{-# INLINE labelConfig_pointing #-}
labelConfig_ribbon ::
  forall t_a1Uk0.
  Lens' (LabelConfig t_a1Uk0) (Active t_a1Uk0 (Maybe Ribbon))
labelConfig_ribbon
  f_a1UUT
  (LabelConfig x1_a1UUU
               x2_a1UUV
               x3_a1UUW
               x4_a1UUX
               x5_a1UUY
               x6_a1UUZ
               x7_a1UV0
               x8_a1UV2
               x9_a1UV3
               x10_a1UV5
               x11_a1UV6
               x12_a1UV7
               x13_a1UV8)
  = fmap
      (\ y1_a1UV9
         -> LabelConfig
              x1_a1UUU
              x2_a1UUV
              x3_a1UUW
              x4_a1UUX
              x5_a1UUY
              x6_a1UUZ
              x7_a1UV0
              x8_a1UV2
              x9_a1UV3
              y1_a1UV9
              x11_a1UV6
              x12_a1UV7
              x13_a1UV8)
      (f_a1UUT x10_a1UV5)
{-# INLINE labelConfig_ribbon #-}
labelConfig_tag ::
  forall t_a1Uk0. Lens' (LabelConfig t_a1Uk0) (Active t_a1Uk0 Bool)
labelConfig_tag
  f_a1UVf
  (LabelConfig x1_a1UVg
               x2_a1UVh
               x3_a1UVi
               x4_a1UVk
               x5_a1UVl
               x6_a1UVm
               x7_a1UVn
               x8_a1UVo
               x9_a1UVp
               x10_a1UVq
               x11_a1UVs
               x12_a1UVt
               x13_a1UVv)
  = fmap
      (\ y1_a1UVw
         -> LabelConfig
              x1_a1UVg
              x2_a1UVh
              x3_a1UVi
              y1_a1UVw
              x5_a1UVl
              x6_a1UVm
              x7_a1UVn
              x8_a1UVo
              x9_a1UVp
              x10_a1UVq
              x11_a1UVs
              x12_a1UVt
              x13_a1UVv)
      (f_a1UVf x4_a1UVk)
{-# INLINE labelConfig_tag #-}
