-- src/Reflex/Dom/SemanticUI/List.hs:57:1-62: Splicing declarations
listConfig_aligned ::
  forall t_a2jgs.
  Lens' (ListConfig t_a2jgs) (Active t_a2jgs (Maybe ListAligned))
listConfig_aligned
  f_a2jVN
  (ListConfig x1_a2jVO
              x2_a2jVP
              x3_a2jVQ
              x4_a2jVR
              x5_a2jVS
              x6_a2jVT
              x7_a2jVU
              x8_a2jVV
              x9_a2jVW
              x10_a2jVX
              x11_a2jVZ
              x12_a2jW0
              x13_a2jW1)
  = fmap
      (\ y1_a2jW3
         -> ListConfig
              x1_a2jVO
              x2_a2jVP
              x3_a2jVQ
              x4_a2jVR
              x5_a2jVS
              x6_a2jVT
              x7_a2jVU
              x8_a2jVV
              x9_a2jVW
              x10_a2jVX
              x11_a2jVZ
              y1_a2jW3
              x13_a2jW1)
      (f_a2jVN x12_a2jW0)
{-# INLINE listConfig_aligned #-}
listConfig_animated ::
  forall t_a2jgs. Lens' (ListConfig t_a2jgs) (Active t_a2jgs Bool)
listConfig_animated
  f_a2jW8
  (ListConfig x1_a2jWa
              x2_a2jWb
              x3_a2jWc
              x4_a2jWd
              x5_a2jWe
              x6_a2jWg
              x7_a2jWh
              x8_a2jWi
              x9_a2jWj
              x10_a2jWl
              x11_a2jWm
              x12_a2jWn
              x13_a2jWp)
  = fmap
      (\ y1_a2jWq
         -> ListConfig
              x1_a2jWa
              x2_a2jWb
              x3_a2jWc
              x4_a2jWd
              x5_a2jWe
              y1_a2jWq
              x7_a2jWh
              x8_a2jWi
              x9_a2jWj
              x10_a2jWl
              x11_a2jWm
              x12_a2jWn
              x13_a2jWp)
      (f_a2jW8 x6_a2jWg)
{-# INLINE listConfig_animated #-}
listConfig_celled ::
  forall t_a2jgs. Lens' (ListConfig t_a2jgs) (Active t_a2jgs Bool)
listConfig_celled
  f_a2jWw
  (ListConfig x1_a2jWx
              x2_a2jWy
              x3_a2jWz
              x4_a2jWA
              x5_a2jWB
              x6_a2jWC
              x7_a2jWD
              x8_a2jWE
              x9_a2jWH
              x10_a2jWI
              x11_a2jWJ
              x12_a2jWK
              x13_a2jWL)
  = fmap
      (\ y1_a2jWM
         -> ListConfig
              x1_a2jWx
              x2_a2jWy
              x3_a2jWz
              x4_a2jWA
              x5_a2jWB
              x6_a2jWC
              x7_a2jWD
              x8_a2jWE
              y1_a2jWM
              x10_a2jWI
              x11_a2jWJ
              x12_a2jWK
              x13_a2jWL)
      (f_a2jWw x9_a2jWH)
{-# INLINE listConfig_celled #-}
listConfig_divided ::
  forall t_a2jgs. Lens' (ListConfig t_a2jgs) (Active t_a2jgs Bool)
listConfig_divided
  f_a2jWV
  (ListConfig x1_a2jWW
              x2_a2jWX
              x3_a2jWY
              x4_a2jWZ
              x5_a2jX0
              x6_a2jX1
              x7_a2jX2
              x8_a2jX3
              x9_a2jX4
              x10_a2jX5
              x11_a2jX6
              x12_a2jX8
              x13_a2jX9)
  = fmap
      (\ y1_a2jXa
         -> ListConfig
              x1_a2jWW
              x2_a2jWX
              x3_a2jWY
              x4_a2jWZ
              x5_a2jX0
              x6_a2jX1
              x7_a2jX2
              y1_a2jXa
              x9_a2jX4
              x10_a2jX5
              x11_a2jX6
              x12_a2jX8
              x13_a2jX9)
      (f_a2jWV x8_a2jX3)
{-# INLINE listConfig_divided #-}
listConfig_elConfig ::
  forall t_a2jgs. Lens' (ListConfig t_a2jgs) (ActiveElConfig t_a2jgs)
listConfig_elConfig
  f_a2jXf
  (ListConfig x1_a2jXg
              x2_a2jXh
              x3_a2jXi
              x4_a2jXj
              x5_a2jXk
              x6_a2jXl
              x7_a2jXm
              x8_a2jXn
              x9_a2jXp
              x10_a2jXq
              x11_a2jXr
              x12_a2jXs
              x13_a2jXt)
  = fmap
      (\ y1_a2jXu
         -> ListConfig
              x1_a2jXg
              x2_a2jXh
              x3_a2jXi
              x4_a2jXj
              x5_a2jXk
              x6_a2jXl
              x7_a2jXm
              x8_a2jXn
              x9_a2jXp
              x10_a2jXq
              x11_a2jXr
              x12_a2jXs
              y1_a2jXu)
      (f_a2jXf x13_a2jXt)
{-# INLINE listConfig_elConfig #-}
listConfig_floated ::
  forall t_a2jgs.
  Lens' (ListConfig t_a2jgs) (Active t_a2jgs (Maybe Floated))
listConfig_floated
  f_a2jXA
  (ListConfig x1_a2jXC
              x2_a2jXD
              x3_a2jXF
              x4_a2jXG
              x5_a2jXH
              x6_a2jXI
              x7_a2jXJ
              x8_a2jXK
              x9_a2jXM
              x10_a2jXN
              x11_a2jXO
              x12_a2jXP
              x13_a2jXQ)
  = fmap
      (\ y1_a2jXR
         -> ListConfig
              x1_a2jXC
              x2_a2jXD
              x3_a2jXF
              x4_a2jXG
              x5_a2jXH
              x6_a2jXI
              x7_a2jXJ
              x8_a2jXK
              x9_a2jXM
              x10_a2jXN
              y1_a2jXR
              x12_a2jXP
              x13_a2jXQ)
      (f_a2jXA x11_a2jXO)
{-# INLINE listConfig_floated #-}
listConfig_horizontal ::
  forall t_a2jgs. Lens' (ListConfig t_a2jgs) (Active t_a2jgs Bool)
listConfig_horizontal
  f_a2jXV
  (ListConfig x1_a2jXW
              x2_a2jXX
              x3_a2jXY
              x4_a2jXZ
              x5_a2jY1
              x6_a2jY2
              x7_a2jY3
              x8_a2jY4
              x9_a2jY5
              x10_a2jY6
              x11_a2jY7
              x12_a2jY8
              x13_a2jYa)
  = fmap
      (\ y1_a2jYb
         -> ListConfig
              x1_a2jXW
              x2_a2jXX
              y1_a2jYb
              x4_a2jXZ
              x5_a2jY1
              x6_a2jY2
              x7_a2jY3
              x8_a2jY4
              x9_a2jY5
              x10_a2jY6
              x11_a2jY7
              x12_a2jY8
              x13_a2jYa)
      (f_a2jXV x3_a2jXY)
{-# INLINE listConfig_horizontal #-}
listConfig_inverted ::
  forall t_a2jgs. Lens' (ListConfig t_a2jgs) (Active t_a2jgs Bool)
listConfig_inverted
  f_a2jYf
  (ListConfig x1_a2jYh
              x2_a2jYi
              x3_a2jYj
              x4_a2jYk
              x5_a2jYl
              x6_a2jYn
              x7_a2jYo
              x8_a2jYp
              x9_a2jYq
              x10_a2jYr
              x11_a2jYs
              x12_a2jYt
              x13_a2jYu)
  = fmap
      (\ y1_a2jYw
         -> ListConfig
              x1_a2jYh
              x2_a2jYi
              x3_a2jYj
              y1_a2jYw
              x5_a2jYl
              x6_a2jYn
              x7_a2jYo
              x8_a2jYp
              x9_a2jYq
              x10_a2jYr
              x11_a2jYs
              x12_a2jYt
              x13_a2jYu)
      (f_a2jYf x4_a2jYk)
{-# INLINE listConfig_inverted #-}
listConfig_link ::
  forall t_a2jgs. Lens' (ListConfig t_a2jgs) (Active t_a2jgs Bool)
listConfig_link
  f_a2jYz
  (ListConfig x1_a2jYA
              x2_a2jYB
              x3_a2jYC
              x4_a2jYD
              x5_a2jYE
              x6_a2jYF
              x7_a2jYG
              x8_a2jYH
              x9_a2jYI
              x10_a2jYJ
              x11_a2jYK
              x12_a2jYL
              x13_a2jYN)
  = fmap
      (\ y1_a2jYO
         -> ListConfig
              x1_a2jYA
              y1_a2jYO
              x3_a2jYC
              x4_a2jYD
              x5_a2jYE
              x6_a2jYF
              x7_a2jYG
              x8_a2jYH
              x9_a2jYI
              x10_a2jYJ
              x11_a2jYK
              x12_a2jYL
              x13_a2jYN)
      (f_a2jYz x2_a2jYB)
{-# INLINE listConfig_link #-}
listConfig_relaxed ::
  forall t_a2jgs.
  Lens' (ListConfig t_a2jgs) (Active t_a2jgs (Maybe Relaxed))
listConfig_relaxed
  f_a2jYR
  (ListConfig x1_a2jYS
              x2_a2jYT
              x3_a2jYU
              x4_a2jYV
              x5_a2jYW
              x6_a2jYX
              x7_a2jYY
              x8_a2jYZ
              x9_a2jZ0
              x10_a2jZ1
              x11_a2jZ2
              x12_a2jZ3
              x13_a2jZ4)
  = fmap
      (\ y1_a2jZ5
         -> ListConfig
              x1_a2jYS
              x2_a2jYT
              x3_a2jYU
              x4_a2jYV
              x5_a2jYW
              x6_a2jYX
              y1_a2jZ5
              x8_a2jYZ
              x9_a2jZ0
              x10_a2jZ1
              x11_a2jZ2
              x12_a2jZ3
              x13_a2jZ4)
      (f_a2jYR x7_a2jYY)
{-# INLINE listConfig_relaxed #-}
listConfig_selection ::
  forall t_a2jgs. Lens' (ListConfig t_a2jgs) (Active t_a2jgs Bool)
listConfig_selection
  f_a2jZb
  (ListConfig x1_a2jZd
              x2_a2jZe
              x3_a2jZf
              x4_a2jZg
              x5_a2jZh
              x6_a2jZj
              x7_a2jZk
              x8_a2jZl
              x9_a2jZm
              x10_a2jZn
              x11_a2jZo
              x12_a2jZq
              x13_a2jZr)
  = fmap
      (\ y1_a2jZt
         -> ListConfig
              x1_a2jZd
              x2_a2jZe
              x3_a2jZf
              x4_a2jZg
              y1_a2jZt
              x6_a2jZj
              x7_a2jZk
              x8_a2jZl
              x9_a2jZm
              x10_a2jZn
              x11_a2jZo
              x12_a2jZq
              x13_a2jZr)
      (f_a2jZb x5_a2jZh)
{-# INLINE listConfig_selection #-}
listConfig_size ::
  forall t_a2jgs.
  Lens' (ListConfig t_a2jgs) (Active t_a2jgs (Maybe Size))
listConfig_size
  f_a2jZx
  (ListConfig x1_a2jZy
              x2_a2jZz
              x3_a2jZA
              x4_a2jZB
              x5_a2jZC
              x6_a2jZD
              x7_a2jZE
              x8_a2jZF
              x9_a2jZG
              x10_a2jZH
              x11_a2jZI
              x12_a2jZJ
              x13_a2jZK)
  = fmap
      (\ y1_a2jZL
         -> ListConfig
              x1_a2jZy
              x2_a2jZz
              x3_a2jZA
              x4_a2jZB
              x5_a2jZC
              x6_a2jZD
              x7_a2jZE
              x8_a2jZF
              x9_a2jZG
              y1_a2jZL
              x11_a2jZI
              x12_a2jZJ
              x13_a2jZK)
      (f_a2jZx x10_a2jZH)
{-# INLINE listConfig_size #-}
listConfig_type ::
  forall t_a2jgs.
  Lens' (ListConfig t_a2jgs) (Active t_a2jgs (Maybe ListType))
listConfig_type
  f_a2jZO
  (ListConfig x1_a2jZP
              x2_a2jZQ
              x3_a2jZR
              x4_a2jZT
              x5_a2jZU
              x6_a2jZV
              x7_a2jZW
              x8_a2jZX
              x9_a2jZY
              x10_a2jZZ
              x11_a2k00
              x12_a2k02
              x13_a2k03)
  = fmap
      (\ y1_a2k04
         -> ListConfig
              y1_a2k04
              x2_a2jZQ
              x3_a2jZR
              x4_a2jZT
              x5_a2jZU
              x6_a2jZV
              x7_a2jZW
              x8_a2jZX
              x9_a2jZY
              x10_a2jZZ
              x11_a2k00
              x12_a2k02
              x13_a2k03)
      (f_a2jZO x1_a2jZP)
{-# INLINE listConfig_type #-}
-- src/Reflex/Dom/SemanticUI/List.hs:109:1-66: Splicing declarations
listItemConfig_elConfig ::
  forall t_a2k8T m_a2k8U.
  Lens' (ListItemConfig t_a2k8T m_a2k8U) (ActiveElConfig t_a2k8T)
listItemConfig_elConfig
  f_a2kDb
  (ListItemConfig x1_a2kDc x2_a2kDd x3_a2kDe)
  = fmap
      (\ y1_a2kDf -> ListItemConfig x1_a2kDc x2_a2kDd y1_a2kDf)
      (f_a2kDb x3_a2kDe)
{-# INLINE listItemConfig_elConfig #-}
listItemConfig_element ::
  forall t_a2k8T m_a2k8U.
  Lens' (ListItemConfig t_a2k8T m_a2k8U) ListItemElement
listItemConfig_element
  f_a2kDh
  (ListItemConfig x1_a2kDi x2_a2kDj x3_a2kDk)
  = fmap
      (\ y1_a2kDl -> ListItemConfig x1_a2kDi y1_a2kDl x3_a2kDk)
      (f_a2kDh x2_a2kDj)
{-# INLINE listItemConfig_element #-}
listItemConfig_preContent ::
  forall t_a2k8T m_a2k8U.
  Lens' (ListItemConfig t_a2k8T m_a2k8U) (Maybe (m_a2k8U ()))
listItemConfig_preContent
  f_a2kDn
  (ListItemConfig x1_a2kDo x2_a2kDp x3_a2kDq)
  = fmap
      (\ y1_a2kDr -> ListItemConfig y1_a2kDr x2_a2kDp x3_a2kDq)
      (f_a2kDn x1_a2kDo)
{-# INLINE listItemConfig_preContent #-}
