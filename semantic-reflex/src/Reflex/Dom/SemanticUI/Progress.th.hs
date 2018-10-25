-- src/Reflex/Dom/SemanticUI/Progress.hs:124:1-66: Splicing declarations
progressConfig_active ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Dynamic t_a1jDo Bool)
progressConfig_active
  f_a1ksz
  (ProgressConfig x1_a1ksA
                  x2_a1ksB
                  x3_a1ksC
                  x4_a1ksD
                  x5_a1ksE
                  x6_a1ksF
                  x7_a1ksG
                  x8_a1ksH
                  x9_a1ksI
                  x10_a1ksK
                  x11_a1ksO
                  x12_a1ksP
                  x13_a1ksQ
                  x14_a1ksR
                  x15_a1ksS
                  x16_a1ksT)
  = fmap
      (\ y1_a1ksU
         -> ProgressConfig
              x1_a1ksA
              x2_a1ksB
              x3_a1ksC
              y1_a1ksU
              x5_a1ksE
              x6_a1ksF
              x7_a1ksG
              x8_a1ksH
              x9_a1ksI
              x10_a1ksK
              x11_a1ksO
              x12_a1ksP
              x13_a1ksQ
              x14_a1ksR
              x15_a1ksS
              x16_a1ksT)
      (f_a1ksz x4_a1ksD)
{-# INLINE progressConfig_active #-}
progressConfig_attached ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Dynamic t_a1jDo (Maybe VerticalAttached))
progressConfig_attached
  f_a1kt2
  (ProgressConfig x1_a1kt3
                  x2_a1kt4
                  x3_a1kt5
                  x4_a1kt7
                  x5_a1kt8
                  x6_a1kta
                  x7_a1ktd
                  x8_a1kte
                  x9_a1ktf
                  x10_a1ktg
                  x11_a1kth
                  x12_a1kti
                  x13_a1ktj
                  x14_a1ktk
                  x15_a1ktl
                  x16_a1ktm)
  = fmap
      (\ y1_a1ktn
         -> ProgressConfig
              x1_a1kt3
              x2_a1kt4
              x3_a1kt5
              x4_a1kt7
              x5_a1kt8
              x6_a1kta
              x7_a1ktd
              x8_a1kte
              x9_a1ktf
              x10_a1ktg
              y1_a1ktn
              x12_a1kti
              x13_a1ktj
              x14_a1ktk
              x15_a1ktl
              x16_a1ktm)
      (f_a1kt2 x11_a1kth)
{-# INLINE progressConfig_attached #-}
progressConfig_bar ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Maybe (Bar m_a1jDq))
progressConfig_bar
  f_a1ktz
  (ProgressConfig x1_a1ktA
                  x2_a1ktB
                  x3_a1ktC
                  x4_a1ktD
                  x5_a1ktE
                  x6_a1ktF
                  x7_a1ktG
                  x8_a1ktH
                  x9_a1ktI
                  x10_a1ktJ
                  x11_a1ktK
                  x12_a1ktL
                  x13_a1ktM
                  x14_a1ktN
                  x15_a1ktO
                  x16_a1ktP)
  = fmap
      (\ y1_a1ktQ
         -> ProgressConfig
              y1_a1ktQ
              x2_a1ktB
              x3_a1ktC
              x4_a1ktD
              x5_a1ktE
              x6_a1ktF
              x7_a1ktG
              x8_a1ktH
              x9_a1ktI
              x10_a1ktJ
              x11_a1ktK
              x12_a1ktL
              x13_a1ktM
              x14_a1ktN
              x15_a1ktO
              x16_a1ktP)
      (f_a1ktz x1_a1ktA)
{-# INLINE progressConfig_bar #-}
progressConfig_color ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Dynamic t_a1jDo (Maybe Color))
progressConfig_color
  f_a1ku1
  (ProgressConfig x1_a1ku2
                  x2_a1ku3
                  x3_a1ku7
                  x4_a1ku9
                  x5_a1kua
                  x6_a1kub
                  x7_a1kuc
                  x8_a1kug
                  x9_a1kui
                  x10_a1kuj
                  x11_a1kuk
                  x12_a1kul
                  x13_a1kum
                  x14_a1kun
                  x15_a1kuo
                  x16_a1kup)
  = fmap
      (\ y1_a1kuq
         -> ProgressConfig
              x1_a1ku2
              x2_a1ku3
              x3_a1ku7
              x4_a1ku9
              x5_a1kua
              x6_a1kub
              x7_a1kuc
              x8_a1kug
              x9_a1kui
              x10_a1kuj
              x11_a1kuk
              x12_a1kul
              y1_a1kuq
              x14_a1kun
              x15_a1kuo
              x16_a1kup)
      (f_a1ku1 x13_a1kum)
{-# INLINE progressConfig_color #-}
progressConfig_disabled ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Dynamic t_a1jDo Bool)
progressConfig_disabled
  f_a1kuA
  (ProgressConfig x1_a1kuB
                  x2_a1kuC
                  x3_a1kuD
                  x4_a1kuE
                  x5_a1kuF
                  x6_a1kuG
                  x7_a1kuH
                  x8_a1kuI
                  x9_a1kuJ
                  x10_a1kuK
                  x11_a1kuL
                  x12_a1kuM
                  x13_a1kuN
                  x14_a1kuO
                  x15_a1kuP
                  x16_a1kuQ)
  = fmap
      (\ y1_a1kuR
         -> ProgressConfig
              x1_a1kuB
              x2_a1kuC
              x3_a1kuD
              x4_a1kuE
              y1_a1kuR
              x6_a1kuG
              x7_a1kuH
              x8_a1kuI
              x9_a1kuJ
              x10_a1kuK
              x11_a1kuL
              x12_a1kuM
              x13_a1kuN
              x14_a1kuO
              x15_a1kuP
              x16_a1kuQ)
      (f_a1kuA x5_a1kuF)
{-# INLINE progressConfig_disabled #-}
progressConfig_elConfig ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (ActiveElConfig t_a1jDo)
progressConfig_elConfig
  f_a1kuS
  (ProgressConfig x1_a1kuT
                  x2_a1kuU
                  x3_a1kuV
                  x4_a1kuW
                  x5_a1kuX
                  x6_a1kuY
                  x7_a1kuZ
                  x8_a1kv0
                  x9_a1kv1
                  x10_a1kv2
                  x11_a1kv3
                  x12_a1kv4
                  x13_a1kv5
                  x14_a1kv6
                  x15_a1kv7
                  x16_a1kv8)
  = fmap
      (\ y1_a1kv9
         -> ProgressConfig
              x1_a1kuT
              x2_a1kuU
              x3_a1kuV
              x4_a1kuW
              x5_a1kuX
              x6_a1kuY
              x7_a1kuZ
              x8_a1kv0
              x9_a1kv1
              x10_a1kv2
              x11_a1kv3
              x12_a1kv4
              x13_a1kv5
              x14_a1kv6
              x15_a1kv7
              y1_a1kv9)
      (f_a1kuS x16_a1kv8)
{-# INLINE progressConfig_elConfig #-}
progressConfig_error ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Dynamic t_a1jDo Bool)
progressConfig_error
  f_a1kvd
  (ProgressConfig x1_a1kvg
                  x2_a1kvh
                  x3_a1kvi
                  x4_a1kvj
                  x5_a1kvk
                  x6_a1kvl
                  x7_a1kvm
                  x8_a1kvn
                  x9_a1kvo
                  x10_a1kvp
                  x11_a1kvq
                  x12_a1kvr
                  x13_a1kvt
                  x14_a1kvu
                  x15_a1kvv
                  x16_a1kvw)
  = fmap
      (\ y1_a1kvx
         -> ProgressConfig
              x1_a1kvg
              x2_a1kvh
              x3_a1kvi
              x4_a1kvj
              x5_a1kvk
              x6_a1kvl
              x7_a1kvm
              x8_a1kvn
              y1_a1kvx
              x10_a1kvp
              x11_a1kvq
              x12_a1kvr
              x13_a1kvt
              x14_a1kvu
              x15_a1kvv
              x16_a1kvw)
      (f_a1kvd x9_a1kvo)
{-# INLINE progressConfig_error #-}
progressConfig_indeterminate ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Dynamic t_a1jDo Bool)
progressConfig_indeterminate
  f_a1kvz
  (ProgressConfig x1_a1kvA
                  x2_a1kvB
                  x3_a1kvC
                  x4_a1kvD
                  x5_a1kvE
                  x6_a1kvF
                  x7_a1kvG
                  x8_a1kvH
                  x9_a1kvI
                  x10_a1kvJ
                  x11_a1kvK
                  x12_a1kvL
                  x13_a1kvM
                  x14_a1kvN
                  x15_a1kvO
                  x16_a1kvP)
  = fmap
      (\ y1_a1kvQ
         -> ProgressConfig
              x1_a1kvA
              x2_a1kvB
              x3_a1kvC
              x4_a1kvD
              x5_a1kvE
              y1_a1kvQ
              x7_a1kvG
              x8_a1kvH
              x9_a1kvI
              x10_a1kvJ
              x11_a1kvK
              x12_a1kvL
              x13_a1kvM
              x14_a1kvN
              x15_a1kvO
              x16_a1kvP)
      (f_a1kvz x6_a1kvF)
{-# INLINE progressConfig_indeterminate #-}
progressConfig_indicating ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Dynamic t_a1jDo Bool)
progressConfig_indicating
  f_a1kvS
  (ProgressConfig x1_a1kvT
                  x2_a1kvU
                  x3_a1kvV
                  x4_a1kvW
                  x5_a1kvX
                  x6_a1kvY
                  x7_a1kvZ
                  x8_a1kw0
                  x9_a1kw1
                  x10_a1kw2
                  x11_a1kw3
                  x12_a1kw4
                  x13_a1kw5
                  x14_a1kw6
                  x15_a1kw7
                  x16_a1kw8)
  = fmap
      (\ y1_a1kw9
         -> ProgressConfig
              x1_a1kvT
              x2_a1kvU
              y1_a1kw9
              x4_a1kvW
              x5_a1kvX
              x6_a1kvY
              x7_a1kvZ
              x8_a1kw0
              x9_a1kw1
              x10_a1kw2
              x11_a1kw3
              x12_a1kw4
              x13_a1kw5
              x14_a1kw6
              x15_a1kw7
              x16_a1kw8)
      (f_a1kvS x3_a1kvV)
{-# INLINE progressConfig_indicating #-}
progressConfig_inverted ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Dynamic t_a1jDo Bool)
progressConfig_inverted
  f_a1kwb
  (ProgressConfig x1_a1kwc
                  x2_a1kwd
                  x3_a1kwe
                  x4_a1kwf
                  x5_a1kwg
                  x6_a1kwh
                  x7_a1kwi
                  x8_a1kwj
                  x9_a1kwk
                  x10_a1kwl
                  x11_a1kwm
                  x12_a1kwn
                  x13_a1kwo
                  x14_a1kwp
                  x15_a1kwq
                  x16_a1kwr)
  = fmap
      (\ y1_a1kws
         -> ProgressConfig
              x1_a1kwc
              x2_a1kwd
              x3_a1kwe
              x4_a1kwf
              x5_a1kwg
              x6_a1kwh
              x7_a1kwi
              x8_a1kwj
              x9_a1kwk
              y1_a1kws
              x11_a1kwm
              x12_a1kwn
              x13_a1kwo
              x14_a1kwp
              x15_a1kwq
              x16_a1kwr)
      (f_a1kwb x10_a1kwl)
{-# INLINE progressConfig_inverted #-}
progressConfig_label ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Maybe (m_a1jDq ()))
progressConfig_label
  f_a1kwt
  (ProgressConfig x1_a1kwu
                  x2_a1kwv
                  x3_a1kww
                  x4_a1kwx
                  x5_a1kwy
                  x6_a1kwz
                  x7_a1kwA
                  x8_a1kwC
                  x9_a1kwD
                  x10_a1kwE
                  x11_a1kwF
                  x12_a1kwG
                  x13_a1kwH
                  x14_a1kwI
                  x15_a1kwJ
                  x16_a1kwK)
  = fmap
      (\ y1_a1kwL
         -> ProgressConfig
              x1_a1kwu
              y1_a1kwL
              x3_a1kww
              x4_a1kwx
              x5_a1kwy
              x6_a1kwz
              x7_a1kwA
              x8_a1kwC
              x9_a1kwD
              x10_a1kwE
              x11_a1kwF
              x12_a1kwG
              x13_a1kwH
              x14_a1kwI
              x15_a1kwJ
              x16_a1kwK)
      (f_a1kwt x2_a1kwv)
{-# INLINE progressConfig_label #-}
progressConfig_minWidth ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Dynamic t_a1jDo Bool)
progressConfig_minWidth
  f_a1kwM
  (ProgressConfig x1_a1kwO
                  x2_a1kwP
                  x3_a1kwQ
                  x4_a1kwR
                  x5_a1kwS
                  x6_a1kwT
                  x7_a1kwU
                  x8_a1kwV
                  x9_a1kwW
                  x10_a1kwX
                  x11_a1kwY
                  x12_a1kwZ
                  x13_a1kx0
                  x14_a1kx1
                  x15_a1kx2
                  x16_a1kx3)
  = fmap
      (\ y1_a1kx4
         -> ProgressConfig
              x1_a1kwO
              x2_a1kwP
              x3_a1kwQ
              x4_a1kwR
              x5_a1kwS
              x6_a1kwT
              x7_a1kwU
              x8_a1kwV
              x9_a1kwW
              x10_a1kwX
              x11_a1kwY
              x12_a1kwZ
              x13_a1kx0
              y1_a1kx4
              x15_a1kx2
              x16_a1kx3)
      (f_a1kwM x14_a1kx1)
{-# INLINE progressConfig_minWidth #-}
progressConfig_rateLimit ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Maybe NominalDiffTime)
progressConfig_rateLimit
  f_a1kx7
  (ProgressConfig x1_a1kx8
                  x2_a1kx9
                  x3_a1kxa
                  x4_a1kxb
                  x5_a1kxc
                  x6_a1kxd
                  x7_a1kxe
                  x8_a1kxf
                  x9_a1kxg
                  x10_a1kxh
                  x11_a1kxi
                  x12_a1kxj
                  x13_a1kxk
                  x14_a1kxl
                  x15_a1kxm
                  x16_a1kxn)
  = fmap
      (\ y1_a1kxo
         -> ProgressConfig
              x1_a1kx8
              x2_a1kx9
              x3_a1kxa
              x4_a1kxb
              x5_a1kxc
              x6_a1kxd
              x7_a1kxe
              x8_a1kxf
              x9_a1kxg
              x10_a1kxh
              x11_a1kxi
              x12_a1kxj
              x13_a1kxk
              x14_a1kxl
              y1_a1kxo
              x16_a1kxn)
      (f_a1kx7 x15_a1kxm)
{-# INLINE progressConfig_rateLimit #-}
progressConfig_size ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Dynamic t_a1jDo (Maybe Size))
progressConfig_size
  f_a1kxr
  (ProgressConfig x1_a1kxs
                  x2_a1kxt
                  x3_a1kxu
                  x4_a1kxv
                  x5_a1kxw
                  x6_a1kxx
                  x7_a1kxy
                  x8_a1kxz
                  x9_a1kxA
                  x10_a1kxB
                  x11_a1kxC
                  x12_a1kxD
                  x13_a1kxE
                  x14_a1kxF
                  x15_a1kxG
                  x16_a1kxH)
  = fmap
      (\ y1_a1kxI
         -> ProgressConfig
              x1_a1kxs
              x2_a1kxt
              x3_a1kxu
              x4_a1kxv
              x5_a1kxw
              x6_a1kxx
              x7_a1kxy
              x8_a1kxz
              x9_a1kxA
              x10_a1kxB
              x11_a1kxC
              y1_a1kxI
              x13_a1kxE
              x14_a1kxF
              x15_a1kxG
              x16_a1kxH)
      (f_a1kxr x12_a1kxD)
{-# INLINE progressConfig_size #-}
progressConfig_success ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Dynamic t_a1jDo (Maybe Bool))
progressConfig_success
  f_a1kxJ
  (ProgressConfig x1_a1kxK
                  x2_a1kxL
                  x3_a1kxM
                  x4_a1kxN
                  x5_a1kxO
                  x6_a1kxQ
                  x7_a1kxR
                  x8_a1kxS
                  x9_a1kxT
                  x10_a1kxU
                  x11_a1kxV
                  x12_a1kxW
                  x13_a1kxX
                  x14_a1kxY
                  x15_a1kxZ
                  x16_a1ky0)
  = fmap
      (\ y1_a1ky1
         -> ProgressConfig
              x1_a1kxK
              x2_a1kxL
              x3_a1kxM
              x4_a1kxN
              x5_a1kxO
              x6_a1kxQ
              y1_a1ky1
              x8_a1kxS
              x9_a1kxT
              x10_a1kxU
              x11_a1kxV
              x12_a1kxW
              x13_a1kxX
              x14_a1kxY
              x15_a1kxZ
              x16_a1ky0)
      (f_a1kxJ x7_a1kxR)
{-# INLINE progressConfig_success #-}
progressConfig_warning ::
  forall t_a1jDo m_a1jDq.
  Lens' (ProgressConfig t_a1jDo m_a1jDq) (Dynamic t_a1jDo Bool)
progressConfig_warning
  f_a1ky2
  (ProgressConfig x1_a1ky3
                  x2_a1ky4
                  x3_a1ky5
                  x4_a1ky6
                  x5_a1ky7
                  x6_a1ky8
                  x7_a1ky9
                  x8_a1kya
                  x9_a1kyb
                  x10_a1kyc
                  x11_a1kyd
                  x12_a1kye
                  x13_a1kyf
                  x14_a1kyg
                  x15_a1kyh
                  x16_a1kyi)
  = fmap
      (\ y1_a1kyj
         -> ProgressConfig
              x1_a1ky3
              x2_a1ky4
              x3_a1ky5
              x4_a1ky6
              x5_a1ky7
              x6_a1ky8
              x7_a1ky9
              y1_a1kyj
              x9_a1kyb
              x10_a1kyc
              x11_a1kyd
              x12_a1kye
              x13_a1kyf
              x14_a1kyg
              x15_a1kyh
              x16_a1kyi)
      (f_a1ky2 x8_a1kya)
{-# INLINE progressConfig_warning #-}
-- src/Reflex/Dom/SemanticUI/Progress.hs:187:1-60: Splicing declarations
progress_barElement ::
  forall t_a1kLJ m_a1kLM.
  Lens' (Progress t_a1kLJ m_a1kLM) (Element EventResult (DomBuilderSpace m_a1kLM) t_a1kLJ)
progress_barElement f_a1lQS (Progress x1_a1lQT x2_a1lQU x3_a1lQV)
  = fmap
      (\ y1_a1lQW -> Progress x1_a1lQT x2_a1lQU y1_a1lQW)
      (f_a1lQS x3_a1lQV)
{-# INLINE progress_barElement #-}
progress_element ::
  forall t_a1kLJ m_a1kLM.
  Lens' (Progress t_a1kLJ m_a1kLM) (Element EventResult (DomBuilderSpace m_a1kLM) t_a1kLJ)
progress_element f_a1lQX (Progress x1_a1lQY x2_a1lQZ x3_a1lR0)
  = fmap
      (\ y1_a1lR1 -> Progress x1_a1lQY y1_a1lR1 x3_a1lR0)
      (f_a1lQX x2_a1lQZ)
{-# INLINE progress_element #-}
progress_percent ::
  forall t_a1kLJ m_a1kLM.
  Lens' (Progress t_a1kLJ m_a1kLM) (Dynamic t_a1kLJ (Maybe Percent))
progress_percent f_a1lR2 (Progress x1_a1lR3 x2_a1lR6 x3_a1lR8)
  = fmap
      (\ y1_a1lR9 -> Progress y1_a1lR9 x2_a1lR6 x3_a1lR8)
      (f_a1lR2 x1_a1lR3)
{-# INLINE progress_percent #-}
