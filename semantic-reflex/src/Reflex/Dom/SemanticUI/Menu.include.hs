-- src/Reflex/Dom/SemanticUI/Menu.hs:31:1-23: Splicing declarations
--     makeLenses ''MenuConfig
--   ======>
menuConfig_color ::
  forall t_a3Xbx.
  Control.Lens.Type.Lens' (MenuConfig t_a3Xbx) (Active t_a3Xbx (Maybe Color))
menuConfig_color
  f_a3Xcz
  (MenuConfig x1_a3XcA
              x2_a3XcB
              x3_a3XcC
              x4_a3XcD
              x5_a3XcE
              x6_a3XcF
              x7_a3XcG
              x8_a3XcH
              x9_a3XcI
              x10_a3XcJ
              x11_a3XcK
              x12_a3XcL)
  = fmap
      (\ y1_a3XcM
         -> MenuConfig
              y1_a3XcM
              x2_a3XcB
              x3_a3XcC
              x4_a3XcD
              x5_a3XcE
              x6_a3XcF
              x7_a3XcG
              x8_a3XcH
              x9_a3XcI
              x10_a3XcJ
              x11_a3XcK
              x12_a3XcL)
      (f_a3Xcz x1_a3XcA)
{-# INLINE menuConfig_color #-}
menuConfig_compact ::
  forall t_a3Xbx.
  Control.Lens.Type.Lens' (MenuConfig t_a3Xbx) (Active t_a3Xbx Bool)
menuConfig_compact
  f_a3XcN
  (MenuConfig x1_a3XcO
              x2_a3XcP
              x3_a3XcQ
              x4_a3XcR
              x5_a3XcS
              x6_a3XcT
              x7_a3XcU
              x8_a3XcV
              x9_a3XcW
              x10_a3XcX
              x11_a3XcY
              x12_a3XcZ)
  = fmap
      (\ y1_a3Xd0
         -> MenuConfig
              x1_a3XcO
              x2_a3XcP
              x3_a3XcQ
              x4_a3XcR
              x5_a3XcS
              x6_a3XcT
              x7_a3XcU
              x8_a3XcV
              x9_a3XcW
              y1_a3Xd0
              x11_a3XcY
              x12_a3XcZ)
      (f_a3XcN x10_a3XcX)
{-# INLINE menuConfig_compact #-}
menuConfig_elConfig ::
  forall t_a3Xbx.
  Control.Lens.Type.Lens' (MenuConfig t_a3Xbx) (ActiveElConfig t_a3Xbx)
menuConfig_elConfig
  f_a3Xd1
  (MenuConfig x1_a3Xd2
              x2_a3Xd3
              x3_a3Xd4
              x4_a3Xd5
              x5_a3Xd6
              x6_a3Xd7
              x7_a3Xd8
              x8_a3Xd9
              x9_a3Xda
              x10_a3Xdb
              x11_a3Xdc
              x12_a3Xdd)
  = fmap
      (\ y1_a3Xde
         -> MenuConfig
              x1_a3Xd2
              x2_a3Xd3
              x3_a3Xd4
              x4_a3Xd5
              x5_a3Xd6
              x6_a3Xd7
              x7_a3Xd8
              x8_a3Xd9
              x9_a3Xda
              x10_a3Xdb
              x11_a3Xdc
              y1_a3Xde)
      (f_a3Xd1 x12_a3Xdd)
{-# INLINE menuConfig_elConfig #-}
menuConfig_floated ::
  forall t_a3Xbx.
  Control.Lens.Type.Lens' (MenuConfig t_a3Xbx) (Active t_a3Xbx (Maybe Floated))
menuConfig_floated
  f_a3Xdf
  (MenuConfig x1_a3Xdg
              x2_a3Xdh
              x3_a3Xdi
              x4_a3Xdj
              x5_a3Xdk
              x6_a3Xdl
              x7_a3Xdm
              x8_a3Xdn
              x9_a3Xdo
              x10_a3Xdp
              x11_a3Xdq
              x12_a3Xdr)
  = fmap
      (\ y1_a3Xds
         -> MenuConfig
              x1_a3Xdg
              x2_a3Xdh
              x3_a3Xdi
              x4_a3Xdj
              x5_a3Xdk
              x6_a3Xdl
              x7_a3Xdm
              x8_a3Xdn
              x9_a3Xdo
              x10_a3Xdp
              y1_a3Xds
              x12_a3Xdr)
      (f_a3Xdf x11_a3Xdq)
{-# INLINE menuConfig_floated #-}
menuConfig_fluid ::
  forall t_a3Xbx.
  Control.Lens.Type.Lens' (MenuConfig t_a3Xbx) (Active t_a3Xbx Bool)
menuConfig_fluid
  f_a3Xdt
  (MenuConfig x1_a3Xdu
              x2_a3Xdv
              x3_a3Xdw
              x4_a3Xdx
              x5_a3Xdy
              x6_a3Xdz
              x7_a3XdA
              x8_a3XdB
              x9_a3XdC
              x10_a3XdD
              x11_a3XdE
              x12_a3XdF)
  = fmap
      (\ y1_a3XdG
         -> MenuConfig
              x1_a3Xdu
              x2_a3Xdv
              x3_a3Xdw
              x4_a3Xdx
              x5_a3Xdy
              x6_a3Xdz
              x7_a3XdA
              y1_a3XdG
              x9_a3XdC
              x10_a3XdD
              x11_a3XdE
              x12_a3XdF)
      (f_a3Xdt x8_a3XdB)
{-# INLINE menuConfig_fluid #-}
menuConfig_inverted ::
  forall t_a3Xbx.
  Control.Lens.Type.Lens' (MenuConfig t_a3Xbx) (Active t_a3Xbx Bool)
menuConfig_inverted
  f_a3XdH
  (MenuConfig x1_a3XdI
              x2_a3XdJ
              x3_a3XdK
              x4_a3XdL
              x5_a3XdM
              x6_a3XdN
              x7_a3XdO
              x8_a3XdP
              x9_a3XdQ
              x10_a3XdR
              x11_a3XdS
              x12_a3XdT)
  = fmap
      (\ y1_a3XdU
         -> MenuConfig
              x1_a3XdI
              y1_a3XdU
              x3_a3XdK
              x4_a3XdL
              x5_a3XdM
              x6_a3XdN
              x7_a3XdO
              x8_a3XdP
              x9_a3XdQ
              x10_a3XdR
              x11_a3XdS
              x12_a3XdT)
      (f_a3XdH x2_a3XdJ)
{-# INLINE menuConfig_inverted #-}
menuConfig_pointing ::
  forall t_a3Xbx.
  Control.Lens.Type.Lens' (MenuConfig t_a3Xbx) (Active t_a3Xbx Bool)
menuConfig_pointing
  f_a3XdV
  (MenuConfig x1_a3XdW
              x2_a3XdX
              x3_a3XdY
              x4_a3XdZ
              x5_a3Xe0
              x6_a3Xe1
              x7_a3Xe2
              x8_a3Xe3
              x9_a3Xe4
              x10_a3Xe5
              x11_a3Xe6
              x12_a3Xe7)
  = fmap
      (\ y1_a3Xe8
         -> MenuConfig
              x1_a3XdW
              x2_a3XdX
              x3_a3XdY
              x4_a3XdZ
              x5_a3Xe0
              x6_a3Xe1
              y1_a3Xe8
              x8_a3Xe3
              x9_a3Xe4
              x10_a3Xe5
              x11_a3Xe6
              x12_a3Xe7)
      (f_a3XdV x7_a3Xe2)
{-# INLINE menuConfig_pointing #-}
menuConfig_right ::
  forall t_a3Xbx.
  Control.Lens.Type.Lens' (MenuConfig t_a3Xbx) (Active t_a3Xbx Bool)
menuConfig_right
  f_a3Xe9
  (MenuConfig x1_a3Xea
              x2_a3Xeb
              x3_a3Xec
              x4_a3Xed
              x5_a3Xee
              x6_a3Xef
              x7_a3Xeg
              x8_a3Xeh
              x9_a3Xei
              x10_a3Xej
              x11_a3Xek
              x12_a3Xel)
  = fmap
      (\ y1_a3Xem
         -> MenuConfig
              x1_a3Xea
              x2_a3Xeb
              x3_a3Xec
              x4_a3Xed
              x5_a3Xee
              y1_a3Xem
              x7_a3Xeg
              x8_a3Xeh
              x9_a3Xei
              x10_a3Xej
              x11_a3Xek
              x12_a3Xel)
      (f_a3Xe9 x6_a3Xef)
{-# INLINE menuConfig_right #-}
menuConfig_secondary ::
  forall t_a3Xbx.
  Control.Lens.Type.Lens' (MenuConfig t_a3Xbx) (Active t_a3Xbx Bool)
menuConfig_secondary
  f_a3Xen
  (MenuConfig x1_a3Xeo
              x2_a3Xep
              x3_a3Xeq
              x4_a3Xer
              x5_a3Xes
              x6_a3Xet
              x7_a3Xeu
              x8_a3Xev
              x9_a3Xew
              x10_a3Xex
              x11_a3Xey
              x12_a3Xez)
  = fmap
      (\ y1_a3XeA
         -> MenuConfig
              x1_a3Xeo
              x2_a3Xep
              x3_a3Xeq
              x4_a3Xer
              y1_a3XeA
              x6_a3Xet
              x7_a3Xeu
              x8_a3Xev
              x9_a3Xew
              x10_a3Xex
              x11_a3Xey
              x12_a3Xez)
      (f_a3Xen x5_a3Xes)
{-# INLINE menuConfig_secondary #-}
menuConfig_size ::
  forall t_a3Xbx.
  Control.Lens.Type.Lens' (MenuConfig t_a3Xbx) (Active t_a3Xbx (Maybe Size))
menuConfig_size
  f_a3XeB
  (MenuConfig x1_a3XeC
              x2_a3XeD
              x3_a3XeE
              x4_a3XeF
              x5_a3XeG
              x6_a3XeH
              x7_a3XeI
              x8_a3XeJ
              x9_a3XeK
              x10_a3XeL
              x11_a3XeM
              x12_a3XeN)
  = fmap
      (\ y1_a3XeO
         -> MenuConfig
              x1_a3XeC
              x2_a3XeD
              y1_a3XeO
              x4_a3XeF
              x5_a3XeG
              x6_a3XeH
              x7_a3XeI
              x8_a3XeJ
              x9_a3XeK
              x10_a3XeL
              x11_a3XeM
              x12_a3XeN)
      (f_a3XeB x3_a3XeE)
{-# INLINE menuConfig_size #-}
menuConfig_text ::
  forall t_a3Xbx.
  Control.Lens.Type.Lens' (MenuConfig t_a3Xbx) (Active t_a3Xbx Bool)
menuConfig_text
  f_a3XeP
  (MenuConfig x1_a3XeQ
              x2_a3XeR
              x3_a3XeS
              x4_a3XeT
              x5_a3XeU
              x6_a3XeV
              x7_a3XeW
              x8_a3XeX
              x9_a3XeY
              x10_a3XeZ
              x11_a3Xf0
              x12_a3Xf1)
  = fmap
      (\ y1_a3Xf2
         -> MenuConfig
              x1_a3XeQ
              x2_a3XeR
              x3_a3XeS
              x4_a3XeT
              x5_a3XeU
              x6_a3XeV
              x7_a3XeW
              x8_a3XeX
              y1_a3Xf2
              x10_a3XeZ
              x11_a3Xf0
              x12_a3Xf1)
      (f_a3XeP x9_a3XeY)
{-# INLINE menuConfig_text #-}
menuConfig_vertical ::
  forall t_a3Xbx.
  Control.Lens.Type.Lens' (MenuConfig t_a3Xbx) (Active t_a3Xbx Bool)
menuConfig_vertical
  f_a3Xf3
  (MenuConfig x1_a3Xf4
              x2_a3Xf5
              x3_a3Xf6
              x4_a3Xf7
              x5_a3Xf8
              x6_a3Xf9
              x7_a3Xfa
              x8_a3Xfb
              x9_a3Xfc
              x10_a3Xfd
              x11_a3Xfe
              x12_a3Xff)
  = fmap
      (\ y1_a3Xfg
         -> MenuConfig
              x1_a3Xf4
              x2_a3Xf5
              x3_a3Xf6
              y1_a3Xfg
              x5_a3Xf8
              x6_a3Xf9
              x7_a3Xfa
              x8_a3Xfb
              x9_a3Xfc
              x10_a3Xfd
              x11_a3Xfe
              x12_a3Xff)
      (f_a3Xf3 x4_a3Xf7)
{-# INLINE menuConfig_vertical #-}
-- src/Reflex/Dom/SemanticUI/Menu.hs:94:1-27: Splicing declarations
--     makeLenses ''MenuItemConfig
--   ======>
menuItemConfig_color ::
  forall t_a3Xfh.
  Control.Lens.Type.Lens' (MenuItemConfig t_a3Xfh) (Active t_a3Xfh (Maybe Color))
menuItemConfig_color
  f_a3XyY
  (MenuItemConfig x1_a3XyZ x2_a3Xz0 x3_a3Xz1 x4_a3Xz2)
  = fmap
      (\ y1_a3Xz3 -> MenuItemConfig y1_a3Xz3 x2_a3Xz0 x3_a3Xz1 x4_a3Xz2)
      (f_a3XyY x1_a3XyZ)
{-# INLINE menuItemConfig_color #-}
menuItemConfig_disabled ::
  forall t_a3Xfh.
  Control.Lens.Type.Lens' (MenuItemConfig t_a3Xfh) (Active t_a3Xfh Bool)
menuItemConfig_disabled
  f_a3Xz4
  (MenuItemConfig x1_a3Xz5 x2_a3Xz6 x3_a3Xz7 x4_a3Xz8)
  = fmap
      (\ y1_a3Xz9 -> MenuItemConfig x1_a3Xz5 y1_a3Xz9 x3_a3Xz7 x4_a3Xz8)
      (f_a3Xz4 x2_a3Xz6)
{-# INLINE menuItemConfig_disabled #-}
menuItemConfig_elConfig ::
  forall t_a3Xfh.
  Control.Lens.Type.Lens' (MenuItemConfig t_a3Xfh) (ActiveElConfig t_a3Xfh)
menuItemConfig_elConfig
  f_a3Xza
  (MenuItemConfig x1_a3Xzb x2_a3Xzc x3_a3Xzd x4_a3Xze)
  = fmap
      (\ y1_a3Xzf -> MenuItemConfig x1_a3Xzb x2_a3Xzc x3_a3Xzd y1_a3Xzf)
      (f_a3Xza x4_a3Xze)
{-# INLINE menuItemConfig_elConfig #-}
menuItemConfig_link ::
  forall t_a3Xfh.
  Control.Lens.Type.Lens' (MenuItemConfig t_a3Xfh) MenuLink
menuItemConfig_link
  f_a3Xzg
  (MenuItemConfig x1_a3Xzh x2_a3Xzi x3_a3Xzj x4_a3Xzk)
  = fmap
      (\ y1_a3Xzl -> MenuItemConfig x1_a3Xzh x2_a3Xzi y1_a3Xzl x4_a3Xzk)
      (f_a3Xzg x3_a3Xzj)
{-# INLINE menuItemConfig_link #-}

