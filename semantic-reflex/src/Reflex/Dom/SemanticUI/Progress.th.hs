-- src/Reflex/Dom/SemanticUI/Progress.hs:124:1-66: Splicing declarations
progressConfig_active ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Dynamic t_a1Eya Bool)
progressConfig_active
  f_a1GaG
  (ProgressConfig x1_a1GaH
                  x2_a1GaI
                  x3_a1GaJ
                  x4_a1GaK
                  x5_a1GaL
                  x6_a1GaN
                  x7_a1GaO
                  x8_a1GaP
                  x9_a1GaQ
                  x10_a1GaR
                  x11_a1GaS
                  x12_a1GaT
                  x13_a1GaU
                  x14_a1GaV
                  x15_a1GaW
                  x16_a1GaX)
  = fmap
      (\ y1_a1GaY
         -> ProgressConfig
              x1_a1GaH
              x2_a1GaI
              x3_a1GaJ
              y1_a1GaY
              x5_a1GaL
              x6_a1GaN
              x7_a1GaO
              x8_a1GaP
              x9_a1GaQ
              x10_a1GaR
              x11_a1GaS
              x12_a1GaT
              x13_a1GaU
              x14_a1GaV
              x15_a1GaW
              x16_a1GaX)
      (f_a1GaG x4_a1GaK)
{-# INLINE progressConfig_active #-}
progressConfig_attached ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Dynamic t_a1Eya (Maybe VerticalAttached))
progressConfig_attached
  f_a1Gb2
  (ProgressConfig x1_a1Gb3
                  x2_a1Gb4
                  x3_a1Gb6
                  x4_a1Gb7
                  x5_a1Gb9
                  x6_a1Gba
                  x7_a1Gbb
                  x8_a1Gbc
                  x9_a1Gbd
                  x10_a1Gbe
                  x11_a1Gbf
                  x12_a1Gbg
                  x13_a1Gbh
                  x14_a1Gbi
                  x15_a1Gbj
                  x16_a1Gbk)
  = fmap
      (\ y1_a1Gbl
         -> ProgressConfig
              x1_a1Gb3
              x2_a1Gb4
              x3_a1Gb6
              x4_a1Gb7
              x5_a1Gb9
              x6_a1Gba
              x7_a1Gbb
              x8_a1Gbc
              x9_a1Gbd
              x10_a1Gbe
              y1_a1Gbl
              x12_a1Gbg
              x13_a1Gbh
              x14_a1Gbi
              x15_a1Gbj
              x16_a1Gbk)
      (f_a1Gb2 x11_a1Gbf)
{-# INLINE progressConfig_attached #-}
progressConfig_bar ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Maybe (Bar m_a1Eyb))
progressConfig_bar
  f_a1Gbm
  (ProgressConfig x1_a1Gbn
                  x2_a1Gbo
                  x3_a1Gbp
                  x4_a1Gbq
                  x5_a1Gbr
                  x6_a1Gbs
                  x7_a1Gbt
                  x8_a1Gbu
                  x9_a1Gbv
                  x10_a1Gbw
                  x11_a1Gbx
                  x12_a1Gby
                  x13_a1Gbz
                  x14_a1GbA
                  x15_a1GbB
                  x16_a1GbC)
  = fmap
      (\ y1_a1GbE
         -> ProgressConfig
              y1_a1GbE
              x2_a1Gbo
              x3_a1Gbp
              x4_a1Gbq
              x5_a1Gbr
              x6_a1Gbs
              x7_a1Gbt
              x8_a1Gbu
              x9_a1Gbv
              x10_a1Gbw
              x11_a1Gbx
              x12_a1Gby
              x13_a1Gbz
              x14_a1GbA
              x15_a1GbB
              x16_a1GbC)
      (f_a1Gbm x1_a1Gbn)
{-# INLINE progressConfig_bar #-}
progressConfig_color ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Dynamic t_a1Eya (Maybe Color))
progressConfig_color
  f_a1GbG
  (ProgressConfig x1_a1GbH
                  x2_a1GbI
                  x3_a1GbJ
                  x4_a1GbK
                  x5_a1GbL
                  x6_a1GbN
                  x7_a1GbO
                  x8_a1GbP
                  x9_a1GbR
                  x10_a1GbS
                  x11_a1GbT
                  x12_a1GbU
                  x13_a1GbV
                  x14_a1GbW
                  x15_a1GbX
                  x16_a1GbZ)
  = fmap
      (\ y1_a1Gc0
         -> ProgressConfig
              x1_a1GbH
              x2_a1GbI
              x3_a1GbJ
              x4_a1GbK
              x5_a1GbL
              x6_a1GbN
              x7_a1GbO
              x8_a1GbP
              x9_a1GbR
              x10_a1GbS
              x11_a1GbT
              x12_a1GbU
              y1_a1Gc0
              x14_a1GbW
              x15_a1GbX
              x16_a1GbZ)
      (f_a1GbG x13_a1GbV)
{-# INLINE progressConfig_color #-}
progressConfig_disabled ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Dynamic t_a1Eya Bool)
progressConfig_disabled
  f_a1Gc3
  (ProgressConfig x1_a1Gc4
                  x2_a1Gc5
                  x3_a1Gc6
                  x4_a1Gc7
                  x5_a1Gc8
                  x6_a1Gc9
                  x7_a1Gca
                  x8_a1Gcb
                  x9_a1Gcc
                  x10_a1Gcd
                  x11_a1Gce
                  x12_a1Gcf
                  x13_a1Gcg
                  x14_a1Gch
                  x15_a1Gci
                  x16_a1Gcj)
  = fmap
      (\ y1_a1Gck
         -> ProgressConfig
              x1_a1Gc4
              x2_a1Gc5
              x3_a1Gc6
              x4_a1Gc7
              y1_a1Gck
              x6_a1Gc9
              x7_a1Gca
              x8_a1Gcb
              x9_a1Gcc
              x10_a1Gcd
              x11_a1Gce
              x12_a1Gcf
              x13_a1Gcg
              x14_a1Gch
              x15_a1Gci
              x16_a1Gcj)
      (f_a1Gc3 x5_a1Gc8)
{-# INLINE progressConfig_disabled #-}
progressConfig_elConfig ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (ActiveElConfig t_a1Eya)
progressConfig_elConfig
  f_a1Gcn
  (ProgressConfig x1_a1Gcp
                  x2_a1Gcq
                  x3_a1Gcr
                  x4_a1Gcs
                  x5_a1Gct
                  x6_a1Gcu
                  x7_a1Gcv
                  x8_a1Gcw
                  x9_a1Gcx
                  x10_a1Gcy
                  x11_a1Gcz
                  x12_a1GcA
                  x13_a1GcB
                  x14_a1GcC
                  x15_a1GcD
                  x16_a1GcE)
  = fmap
      (\ y1_a1GcF
         -> ProgressConfig
              x1_a1Gcp
              x2_a1Gcq
              x3_a1Gcr
              x4_a1Gcs
              x5_a1Gct
              x6_a1Gcu
              x7_a1Gcv
              x8_a1Gcw
              x9_a1Gcx
              x10_a1Gcy
              x11_a1Gcz
              x12_a1GcA
              x13_a1GcB
              x14_a1GcC
              x15_a1GcD
              y1_a1GcF)
      (f_a1Gcn x16_a1GcE)
{-# INLINE progressConfig_elConfig #-}
progressConfig_error ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Dynamic t_a1Eya Bool)
progressConfig_error
  f_a1GcI
  (ProgressConfig x1_a1GcJ
                  x2_a1GcK
                  x3_a1GcL
                  x4_a1GcM
                  x5_a1GcN
                  x6_a1GcO
                  x7_a1GcP
                  x8_a1GcQ
                  x9_a1GcR
                  x10_a1GcS
                  x11_a1GcT
                  x12_a1GcU
                  x13_a1GcW
                  x14_a1GcX
                  x15_a1GcY
                  x16_a1GcZ)
  = fmap
      (\ y1_a1Gd0
         -> ProgressConfig
              x1_a1GcJ
              x2_a1GcK
              x3_a1GcL
              x4_a1GcM
              x5_a1GcN
              x6_a1GcO
              x7_a1GcP
              x8_a1GcQ
              y1_a1Gd0
              x10_a1GcS
              x11_a1GcT
              x12_a1GcU
              x13_a1GcW
              x14_a1GcX
              x15_a1GcY
              x16_a1GcZ)
      (f_a1GcI x9_a1GcR)
{-# INLINE progressConfig_error #-}
progressConfig_indeterminate ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Dynamic t_a1Eya Bool)
progressConfig_indeterminate
  f_a1Gd3
  (ProgressConfig x1_a1Gd4
                  x2_a1Gd5
                  x3_a1Gd7
                  x4_a1Gd8
                  x5_a1Gd9
                  x6_a1Gda
                  x7_a1Gdb
                  x8_a1Gdc
                  x9_a1Gdd
                  x10_a1Gde
                  x11_a1Gdf
                  x12_a1Gdh
                  x13_a1Gdi
                  x14_a1Gdj
                  x15_a1Gdk
                  x16_a1Gdl)
  = fmap
      (\ y1_a1Gdm
         -> ProgressConfig
              x1_a1Gd4
              x2_a1Gd5
              x3_a1Gd7
              x4_a1Gd8
              x5_a1Gd9
              y1_a1Gdm
              x7_a1Gdb
              x8_a1Gdc
              x9_a1Gdd
              x10_a1Gde
              x11_a1Gdf
              x12_a1Gdh
              x13_a1Gdi
              x14_a1Gdj
              x15_a1Gdk
              x16_a1Gdl)
      (f_a1Gd3 x6_a1Gda)
{-# INLINE progressConfig_indeterminate #-}
progressConfig_indicating ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Dynamic t_a1Eya Bool)
progressConfig_indicating
  f_a1Gdq
  (ProgressConfig x1_a1Gdr
                  x2_a1Gds
                  x3_a1Gdt
                  x4_a1Gdu
                  x5_a1Gdv
                  x6_a1Gdw
                  x7_a1Gdx
                  x8_a1Gdy
                  x9_a1Gdz
                  x10_a1GdB
                  x11_a1GdC
                  x12_a1GdD
                  x13_a1GdE
                  x14_a1GdF
                  x15_a1GdG
                  x16_a1GdH)
  = fmap
      (\ y1_a1GdI
         -> ProgressConfig
              x1_a1Gdr
              x2_a1Gds
              y1_a1GdI
              x4_a1Gdu
              x5_a1Gdv
              x6_a1Gdw
              x7_a1Gdx
              x8_a1Gdy
              x9_a1Gdz
              x10_a1GdB
              x11_a1GdC
              x12_a1GdD
              x13_a1GdE
              x14_a1GdF
              x15_a1GdG
              x16_a1GdH)
      (f_a1Gdq x3_a1Gdt)
{-# INLINE progressConfig_indicating #-}
progressConfig_inverted ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Dynamic t_a1Eya Bool)
progressConfig_inverted
  f_a1GdM
  (ProgressConfig x1_a1GdN
                  x2_a1GdO
                  x3_a1GdQ
                  x4_a1GdR
                  x5_a1GdS
                  x6_a1GdU
                  x7_a1GdV
                  x8_a1GdW
                  x9_a1GdX
                  x10_a1GdY
                  x11_a1GdZ
                  x12_a1Ge0
                  x13_a1Ge1
                  x14_a1Ge2
                  x15_a1Ge3
                  x16_a1Ge4)
  = fmap
      (\ y1_a1Ge5
         -> ProgressConfig
              x1_a1GdN
              x2_a1GdO
              x3_a1GdQ
              x4_a1GdR
              x5_a1GdS
              x6_a1GdU
              x7_a1GdV
              x8_a1GdW
              x9_a1GdX
              y1_a1Ge5
              x11_a1GdZ
              x12_a1Ge0
              x13_a1Ge1
              x14_a1Ge2
              x15_a1Ge3
              x16_a1Ge4)
      (f_a1GdM x10_a1GdY)
{-# INLINE progressConfig_inverted #-}
progressConfig_label ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Maybe (m_a1Eyb ()))
progressConfig_label
  f_a1Ge7
  (ProgressConfig x1_a1Ge8
                  x2_a1Gea
                  x3_a1Geb
                  x4_a1Gee
                  x5_a1Gef
                  x6_a1Geg
                  x7_a1Gei
                  x8_a1Gej
                  x9_a1Gek
                  x10_a1Gel
                  x11_a1Gem
                  x12_a1Gen
                  x13_a1Geo
                  x14_a1Geq
                  x15_a1Ger
                  x16_a1Ges)
  = fmap
      (\ y1_a1Get
         -> ProgressConfig
              x1_a1Ge8
              y1_a1Get
              x3_a1Geb
              x4_a1Gee
              x5_a1Gef
              x6_a1Geg
              x7_a1Gei
              x8_a1Gej
              x9_a1Gek
              x10_a1Gel
              x11_a1Gem
              x12_a1Gen
              x13_a1Geo
              x14_a1Geq
              x15_a1Ger
              x16_a1Ges)
      (f_a1Ge7 x2_a1Gea)
{-# INLINE progressConfig_label #-}
progressConfig_minWidth ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Dynamic t_a1Eya Bool)
progressConfig_minWidth
  f_a1Gew
  (ProgressConfig x1_a1Gex
                  x2_a1Gez
                  x3_a1GeA
                  x4_a1GeB
                  x5_a1GeC
                  x6_a1GeD
                  x7_a1GeE
                  x8_a1GeF
                  x9_a1GeG
                  x10_a1GeI
                  x11_a1GeJ
                  x12_a1GeK
                  x13_a1GeL
                  x14_a1GeM
                  x15_a1GeN
                  x16_a1GeO)
  = fmap
      (\ y1_a1GeP
         -> ProgressConfig
              x1_a1Gex
              x2_a1Gez
              x3_a1GeA
              x4_a1GeB
              x5_a1GeC
              x6_a1GeD
              x7_a1GeE
              x8_a1GeF
              x9_a1GeG
              x10_a1GeI
              x11_a1GeJ
              x12_a1GeK
              x13_a1GeL
              y1_a1GeP
              x15_a1GeN
              x16_a1GeO)
      (f_a1Gew x14_a1GeM)
{-# INLINE progressConfig_minWidth #-}
progressConfig_rateLimit ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Maybe NominalDiffTime)
progressConfig_rateLimit
  f_a1GeR
  (ProgressConfig x1_a1GeS
                  x2_a1GeT
                  x3_a1GeU
                  x4_a1GeV
                  x5_a1GeW
                  x6_a1GeX
                  x7_a1GeY
                  x8_a1GeZ
                  x9_a1Gf0
                  x10_a1Gf1
                  x11_a1Gf2
                  x12_a1Gf3
                  x13_a1Gf4
                  x14_a1Gf5
                  x15_a1Gf7
                  x16_a1Gf8)
  = fmap
      (\ y1_a1Gf9
         -> ProgressConfig
              x1_a1GeS
              x2_a1GeT
              x3_a1GeU
              x4_a1GeV
              x5_a1GeW
              x6_a1GeX
              x7_a1GeY
              x8_a1GeZ
              x9_a1Gf0
              x10_a1Gf1
              x11_a1Gf2
              x12_a1Gf3
              x13_a1Gf4
              x14_a1Gf5
              y1_a1Gf9
              x16_a1Gf8)
      (f_a1GeR x15_a1Gf7)
{-# INLINE progressConfig_rateLimit #-}
progressConfig_size ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Dynamic t_a1Eya (Maybe Size))
progressConfig_size
  f_a1Gfe
  (ProgressConfig x1_a1Gff
                  x2_a1Gfg
                  x3_a1Gfh
                  x4_a1Gfi
                  x5_a1Gfj
                  x6_a1Gfk
                  x7_a1Gfl
                  x8_a1Gfm
                  x9_a1Gfn
                  x10_a1Gfo
                  x11_a1Gfp
                  x12_a1Gfq
                  x13_a1Gfr
                  x14_a1Gfs
                  x15_a1Gft
                  x16_a1Gfu)
  = fmap
      (\ y1_a1Gfv
         -> ProgressConfig
              x1_a1Gff
              x2_a1Gfg
              x3_a1Gfh
              x4_a1Gfi
              x5_a1Gfj
              x6_a1Gfk
              x7_a1Gfl
              x8_a1Gfm
              x9_a1Gfn
              x10_a1Gfo
              x11_a1Gfp
              y1_a1Gfv
              x13_a1Gfr
              x14_a1Gfs
              x15_a1Gft
              x16_a1Gfu)
      (f_a1Gfe x12_a1Gfq)
{-# INLINE progressConfig_size #-}
progressConfig_success ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Dynamic t_a1Eya (Maybe Bool))
progressConfig_success
  f_a1Gfx
  (ProgressConfig x1_a1Gfz
                  x2_a1GfA
                  x3_a1GfB
                  x4_a1GfC
                  x5_a1GfD
                  x6_a1GfE
                  x7_a1GfF
                  x8_a1GfG
                  x9_a1GfI
                  x10_a1GfJ
                  x11_a1GfK
                  x12_a1GfL
                  x13_a1GfM
                  x14_a1GfN
                  x15_a1GfO
                  x16_a1GfP)
  = fmap
      (\ y1_a1GfQ
         -> ProgressConfig
              x1_a1Gfz
              x2_a1GfA
              x3_a1GfB
              x4_a1GfC
              x5_a1GfD
              x6_a1GfE
              y1_a1GfQ
              x8_a1GfG
              x9_a1GfI
              x10_a1GfJ
              x11_a1GfK
              x12_a1GfL
              x13_a1GfM
              x14_a1GfN
              x15_a1GfO
              x16_a1GfP)
      (f_a1Gfx x7_a1GfF)
{-# INLINE progressConfig_success #-}
progressConfig_warning ::
  forall t_a1Eya m_a1Eyb.
  Lens' (ProgressConfig t_a1Eya m_a1Eyb) (Dynamic t_a1Eya Bool)
progressConfig_warning
  f_a1GfU
  (ProgressConfig x1_a1GfV
                  x2_a1GfW
                  x3_a1GfX
                  x4_a1GfY
                  x5_a1GfZ
                  x6_a1Gg0
                  x7_a1Gg1
                  x8_a1Gg2
                  x9_a1Gg3
                  x10_a1Gg4
                  x11_a1Gg5
                  x12_a1Gg6
                  x13_a1Gg7
                  x14_a1Gg8
                  x15_a1Gg9
                  x16_a1Gga)
  = fmap
      (\ y1_a1Ggb
         -> ProgressConfig
              x1_a1GfV
              x2_a1GfW
              x3_a1GfX
              x4_a1GfY
              x5_a1GfZ
              x6_a1Gg0
              x7_a1Gg1
              y1_a1Ggb
              x9_a1Gg3
              x10_a1Gg4
              x11_a1Gg5
              x12_a1Gg6
              x13_a1Gg7
              x14_a1Gg8
              x15_a1Gg9
              x16_a1Gga)
      (f_a1GfU x8_a1Gg2)
{-# INLINE progressConfig_warning #-}
-- src/Reflex/Dom/SemanticUI/Progress.hs:187:1-60: Splicing declarations
progress_barElement ::
  forall t_a1GxA m_a1GxD.
  Lens' (Progress t_a1GxA m_a1GxD) (Element EventResult (DomBuilderSpace m_a1GxD) t_a1GxA)
progress_barElement f_a1Hr7 (Progress x1_a1Hr8 x2_a1Hra x3_a1Hrb)
  = fmap
      (\ y1_a1Hrc -> Progress x1_a1Hr8 x2_a1Hra y1_a1Hrc)
      (f_a1Hr7 x3_a1Hrb)
{-# INLINE progress_barElement #-}
progress_element ::
  forall t_a1GxA m_a1GxD.
  Lens' (Progress t_a1GxA m_a1GxD) (Element EventResult (DomBuilderSpace m_a1GxD) t_a1GxA)
progress_element f_a1Hrd (Progress x1_a1Hre x2_a1Hrf x3_a1Hrg)
  = fmap
      (\ y1_a1Hrh -> Progress x1_a1Hre y1_a1Hrh x3_a1Hrg)
      (f_a1Hrd x2_a1Hrf)
{-# INLINE progress_element #-}
progress_percent ::
  forall t_a1GxA m_a1GxD.
  Lens' (Progress t_a1GxA m_a1GxD) (Dynamic t_a1GxA (Maybe Percent))
progress_percent f_a1Hrj (Progress x1_a1Hrk x2_a1Hrl x3_a1Hrm)
  = fmap
      (\ y1_a1Hrn -> Progress y1_a1Hrn x2_a1Hrl x3_a1Hrm)
      (f_a1Hrj x1_a1Hrk)
{-# INLINE progress_percent #-}
