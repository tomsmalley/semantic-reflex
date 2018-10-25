-- src/Reflex/Dom/SemanticUI/Menu.hs:36:1-23: Splicing declarations
menuConfig_color ::
  forall t_aT74.
  Lens' (MenuConfig t_aT74) (Active t_aT74 (Maybe Color))
menuConfig_color
  f_aTdb
  (MenuConfig x1_aTdc
              x2_aTdd
              x3_aTde
              x4_aTdf
              x5_aTdg
              x6_aTdh
              x7_aTdi
              x8_aTdj
              x9_aTdk
              x10_aTdl
              x11_aTdm
              x12_aTdn)
  = fmap
      (\ y1_aTdo
         -> MenuConfig
              y1_aTdo
              x2_aTdd
              x3_aTde
              x4_aTdf
              x5_aTdg
              x6_aTdh
              x7_aTdi
              x8_aTdj
              x9_aTdk
              x10_aTdl
              x11_aTdm
              x12_aTdn)
      (f_aTdb x1_aTdc)
{-# INLINE menuConfig_color #-}
menuConfig_compact ::
  forall t_aT74. Lens' (MenuConfig t_aT74) (Active t_aT74 Bool)
menuConfig_compact
  f_aTdp
  (MenuConfig x1_aTdq
              x2_aTdr
              x3_aTds
              x4_aTdt
              x5_aTdu
              x6_aTdv
              x7_aTdw
              x8_aTdx
              x9_aTdy
              x10_aTdz
              x11_aTdA
              x12_aTdB)
  = fmap
      (\ y1_aTdC
         -> MenuConfig
              x1_aTdq
              x2_aTdr
              x3_aTds
              x4_aTdt
              x5_aTdu
              x6_aTdv
              x7_aTdw
              x8_aTdx
              x9_aTdy
              y1_aTdC
              x11_aTdA
              x12_aTdB)
      (f_aTdp x10_aTdz)
{-# INLINE menuConfig_compact #-}
menuConfig_elConfig ::
  forall t_aT74. Lens' (MenuConfig t_aT74) (ActiveElConfig t_aT74)
menuConfig_elConfig
  f_aTdD
  (MenuConfig x1_aTdE
              x2_aTdF
              x3_aTdG
              x4_aTdH
              x5_aTdI
              x6_aTdJ
              x7_aTdK
              x8_aTdL
              x9_aTdM
              x10_aTdN
              x11_aTdO
              x12_aTdP)
  = fmap
      (\ y1_aTdQ
         -> MenuConfig
              x1_aTdE
              x2_aTdF
              x3_aTdG
              x4_aTdH
              x5_aTdI
              x6_aTdJ
              x7_aTdK
              x8_aTdL
              x9_aTdM
              x10_aTdN
              x11_aTdO
              y1_aTdQ)
      (f_aTdD x12_aTdP)
{-# INLINE menuConfig_elConfig #-}
menuConfig_floated ::
  forall t_aT74.
  Lens' (MenuConfig t_aT74) (Active t_aT74 (Maybe Floated))
menuConfig_floated
  f_aTdR
  (MenuConfig x1_aTdS
              x2_aTdT
              x3_aTdU
              x4_aTdV
              x5_aTdW
              x6_aTdX
              x7_aTdY
              x8_aTdZ
              x9_aTe0
              x10_aTe1
              x11_aTe2
              x12_aTe3)
  = fmap
      (\ y1_aTe4
         -> MenuConfig
              x1_aTdS
              x2_aTdT
              x3_aTdU
              x4_aTdV
              x5_aTdW
              x6_aTdX
              x7_aTdY
              x8_aTdZ
              x9_aTe0
              x10_aTe1
              y1_aTe4
              x12_aTe3)
      (f_aTdR x11_aTe2)
{-# INLINE menuConfig_floated #-}
menuConfig_fluid ::
  forall t_aT74. Lens' (MenuConfig t_aT74) (Active t_aT74 Bool)
menuConfig_fluid
  f_aTe5
  (MenuConfig x1_aTe6
              x2_aTe7
              x3_aTe8
              x4_aTe9
              x5_aTea
              x6_aTeb
              x7_aTec
              x8_aTed
              x9_aTee
              x10_aTef
              x11_aTeg
              x12_aTeh)
  = fmap
      (\ y1_aTei
         -> MenuConfig
              x1_aTe6
              x2_aTe7
              x3_aTe8
              x4_aTe9
              x5_aTea
              x6_aTeb
              x7_aTec
              y1_aTei
              x9_aTee
              x10_aTef
              x11_aTeg
              x12_aTeh)
      (f_aTe5 x8_aTed)
{-# INLINE menuConfig_fluid #-}
menuConfig_inverted ::
  forall t_aT74. Lens' (MenuConfig t_aT74) (Active t_aT74 Bool)
menuConfig_inverted
  f_aTej
  (MenuConfig x1_aTek
              x2_aTel
              x3_aTem
              x4_aTen
              x5_aTeo
              x6_aTep
              x7_aTeq
              x8_aTer
              x9_aTes
              x10_aTet
              x11_aTeu
              x12_aTev)
  = fmap
      (\ y1_aTew
         -> MenuConfig
              x1_aTek
              y1_aTew
              x3_aTem
              x4_aTen
              x5_aTeo
              x6_aTep
              x7_aTeq
              x8_aTer
              x9_aTes
              x10_aTet
              x11_aTeu
              x12_aTev)
      (f_aTej x2_aTel)
{-# INLINE menuConfig_inverted #-}
menuConfig_pointing ::
  forall t_aT74. Lens' (MenuConfig t_aT74) (Active t_aT74 Bool)
menuConfig_pointing
  f_aTex
  (MenuConfig x1_aTey
              x2_aTez
              x3_aTeA
              x4_aTeB
              x5_aTeC
              x6_aTeD
              x7_aTeE
              x8_aTeF
              x9_aTeG
              x10_aTeH
              x11_aTeI
              x12_aTeJ)
  = fmap
      (\ y1_aTeK
         -> MenuConfig
              x1_aTey
              x2_aTez
              x3_aTeA
              x4_aTeB
              x5_aTeC
              x6_aTeD
              y1_aTeK
              x8_aTeF
              x9_aTeG
              x10_aTeH
              x11_aTeI
              x12_aTeJ)
      (f_aTex x7_aTeE)
{-# INLINE menuConfig_pointing #-}
menuConfig_right ::
  forall t_aT74. Lens' (MenuConfig t_aT74) (Active t_aT74 Bool)
menuConfig_right
  f_aTeL
  (MenuConfig x1_aTeM
              x2_aTeN
              x3_aTeO
              x4_aTeP
              x5_aTeQ
              x6_aTeR
              x7_aTeS
              x8_aTeT
              x9_aTeU
              x10_aTeV
              x11_aTeW
              x12_aTeX)
  = fmap
      (\ y1_aTeY
         -> MenuConfig
              x1_aTeM
              x2_aTeN
              x3_aTeO
              x4_aTeP
              x5_aTeQ
              y1_aTeY
              x7_aTeS
              x8_aTeT
              x9_aTeU
              x10_aTeV
              x11_aTeW
              x12_aTeX)
      (f_aTeL x6_aTeR)
{-# INLINE menuConfig_right #-}
menuConfig_secondary ::
  forall t_aT74. Lens' (MenuConfig t_aT74) (Active t_aT74 Bool)
menuConfig_secondary
  f_aTeZ
  (MenuConfig x1_aTf0
              x2_aTf1
              x3_aTf2
              x4_aTf3
              x5_aTf4
              x6_aTf5
              x7_aTf6
              x8_aTf7
              x9_aTf8
              x10_aTf9
              x11_aTfa
              x12_aTfb)
  = fmap
      (\ y1_aTfc
         -> MenuConfig
              x1_aTf0
              x2_aTf1
              x3_aTf2
              x4_aTf3
              y1_aTfc
              x6_aTf5
              x7_aTf6
              x8_aTf7
              x9_aTf8
              x10_aTf9
              x11_aTfa
              x12_aTfb)
      (f_aTeZ x5_aTf4)
{-# INLINE menuConfig_secondary #-}
menuConfig_size ::
  forall t_aT74.
  Lens' (MenuConfig t_aT74) (Active t_aT74 (Maybe Size))
menuConfig_size
  f_aTfd
  (MenuConfig x1_aTfe
              x2_aTff
              x3_aTfg
              x4_aTfh
              x5_aTfi
              x6_aTfj
              x7_aTfk
              x8_aTfl
              x9_aTfm
              x10_aTfn
              x11_aTfo
              x12_aTfp)
  = fmap
      (\ y1_aTfq
         -> MenuConfig
              x1_aTfe
              x2_aTff
              y1_aTfq
              x4_aTfh
              x5_aTfi
              x6_aTfj
              x7_aTfk
              x8_aTfl
              x9_aTfm
              x10_aTfn
              x11_aTfo
              x12_aTfp)
      (f_aTfd x3_aTfg)
{-# INLINE menuConfig_size #-}
menuConfig_text ::
  forall t_aT74. Lens' (MenuConfig t_aT74) (Active t_aT74 Bool)
menuConfig_text
  f_aTfr
  (MenuConfig x1_aTfs
              x2_aTft
              x3_aTfu
              x4_aTfv
              x5_aTfw
              x6_aTfx
              x7_aTfy
              x8_aTfz
              x9_aTfA
              x10_aTfB
              x11_aTfC
              x12_aTfD)
  = fmap
      (\ y1_aTfE
         -> MenuConfig
              x1_aTfs
              x2_aTft
              x3_aTfu
              x4_aTfv
              x5_aTfw
              x6_aTfx
              x7_aTfy
              x8_aTfz
              y1_aTfE
              x10_aTfB
              x11_aTfC
              x12_aTfD)
      (f_aTfr x9_aTfA)
{-# INLINE menuConfig_text #-}
menuConfig_vertical ::
  forall t_aT74. Lens' (MenuConfig t_aT74) (Active t_aT74 Bool)
menuConfig_vertical
  f_aTfF
  (MenuConfig x1_aTfG
              x2_aTfH
              x3_aTfI
              x4_aTfJ
              x5_aTfK
              x6_aTfL
              x7_aTfM
              x8_aTfN
              x9_aTfO
              x10_aTfP
              x11_aTfQ
              x12_aTfR)
  = fmap
      (\ y1_aTfS
         -> MenuConfig
              x1_aTfG
              x2_aTfH
              x3_aTfI
              y1_aTfS
              x5_aTfK
              x6_aTfL
              x7_aTfM
              x8_aTfN
              x9_aTfO
              x10_aTfP
              x11_aTfQ
              x12_aTfR)
      (f_aTfF x4_aTfJ)
{-# INLINE menuConfig_vertical #-}
-- src/Reflex/Dom/SemanticUI/Menu.hs:101:1-27: Splicing declarations
menuItemConfig_color ::
  forall t_aTlW.
  Lens' (MenuItemConfig t_aTlW) (Active t_aTlW (Maybe Color))
menuItemConfig_color
  f_aUvW
  (MenuItemConfig x1_aUvX x2_aUvY x3_aUvZ x4_aUw0)
  = fmap
      (\ y1_aUw1 -> MenuItemConfig y1_aUw1 x2_aUvY x3_aUvZ x4_aUw0)
      (f_aUvW x1_aUvX)
{-# INLINE menuItemConfig_color #-}
menuItemConfig_disabled ::
  forall t_aTlW. Lens' (MenuItemConfig t_aTlW) (Active t_aTlW Bool)
menuItemConfig_disabled
  f_aUw2
  (MenuItemConfig x1_aUw3 x2_aUw4 x3_aUw5 x4_aUw6)
  = fmap
      (\ y1_aUw7 -> MenuItemConfig x1_aUw3 y1_aUw7 x3_aUw5 x4_aUw6)
      (f_aUw2 x2_aUw4)
{-# INLINE menuItemConfig_disabled #-}
menuItemConfig_elConfig ::
  forall t_aTlW.
  Lens' (MenuItemConfig t_aTlW) (ActiveElConfig t_aTlW)
menuItemConfig_elConfig
  f_aUw8
  (MenuItemConfig x1_aUw9 x2_aUwa x3_aUwb x4_aUwc)
  = fmap
      (\ y1_aUwd -> MenuItemConfig x1_aUw9 x2_aUwa x3_aUwb y1_aUwd)
      (f_aUw8 x4_aUwc)
{-# INLINE menuItemConfig_elConfig #-}
menuItemConfig_link ::
  forall t_aTlW. Lens' (MenuItemConfig t_aTlW) MenuLink
menuItemConfig_link
  f_aUwe
  (MenuItemConfig x1_aUwf x2_aUwg x3_aUwh x4_aUwi)
  = fmap
      (\ y1_aUwj -> MenuItemConfig x1_aUwf x2_aUwg y1_aUwj x4_aUwi)
      (f_aUwe x3_aUwh)
{-# INLINE menuItemConfig_link #-}
