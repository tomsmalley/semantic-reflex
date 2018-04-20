-- src/Reflex/Dom/SemanticUI/Progress.hs:119:1-66: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''ProgressConfig
--   ======>
progressConfig_active ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Dynamic t_a417I Bool)
progressConfig_active
  f_a41jk
  (ProgressConfig x1_a41jl
                  x2_a41jm
                  x3_a41jn
                  x4_a41jo
                  x5_a41jp
                  x6_a41jq
                  x7_a41jr
                  x8_a41js
                  x9_a41jt
                  x10_a41ju
                  x11_a41jv
                  x12_a41jw
                  x13_a41jx
                  x14_a41jy
                  x15_a41jz
                  x16_a41jA)
  = fmap
      (\ y1_a41jB
         -> ProgressConfig
              x1_a41jl
              x2_a41jm
              x3_a41jn
              y1_a41jB
              x5_a41jp
              x6_a41jq
              x7_a41jr
              x8_a41js
              x9_a41jt
              x10_a41ju
              x11_a41jv
              x12_a41jw
              x13_a41jx
              x14_a41jy
              x15_a41jz
              x16_a41jA)
      (f_a41jk x4_a41jo)
{-# INLINE progressConfig_active #-}
progressConfig_attached ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Dynamic t_a417I (Maybe VerticalAttached))
progressConfig_attached
  f_a41jC
  (ProgressConfig x1_a41jD
                  x2_a41jE
                  x3_a41jF
                  x4_a41jG
                  x5_a41jH
                  x6_a41jI
                  x7_a41jJ
                  x8_a41jK
                  x9_a41jL
                  x10_a41jM
                  x11_a41jN
                  x12_a41jO
                  x13_a41jP
                  x14_a41jQ
                  x15_a41jR
                  x16_a41jS)
  = fmap
      (\ y1_a41jT
         -> ProgressConfig
              x1_a41jD
              x2_a41jE
              x3_a41jF
              x4_a41jG
              x5_a41jH
              x6_a41jI
              x7_a41jJ
              x8_a41jK
              x9_a41jL
              x10_a41jM
              y1_a41jT
              x12_a41jO
              x13_a41jP
              x14_a41jQ
              x15_a41jR
              x16_a41jS)
      (f_a41jC x11_a41jN)
{-# INLINE progressConfig_attached #-}
progressConfig_bar ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Maybe (Bar m_a417J))
progressConfig_bar
  f_a41jU
  (ProgressConfig x1_a41jV
                  x2_a41jW
                  x3_a41jX
                  x4_a41jY
                  x5_a41jZ
                  x6_a41k0
                  x7_a41k1
                  x8_a41k2
                  x9_a41k3
                  x10_a41k4
                  x11_a41k5
                  x12_a41k6
                  x13_a41k7
                  x14_a41k8
                  x15_a41k9
                  x16_a41ka)
  = fmap
      (\ y1_a41kb
         -> ProgressConfig
              y1_a41kb
              x2_a41jW
              x3_a41jX
              x4_a41jY
              x5_a41jZ
              x6_a41k0
              x7_a41k1
              x8_a41k2
              x9_a41k3
              x10_a41k4
              x11_a41k5
              x12_a41k6
              x13_a41k7
              x14_a41k8
              x15_a41k9
              x16_a41ka)
      (f_a41jU x1_a41jV)
{-# INLINE progressConfig_bar #-}
progressConfig_color ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Dynamic t_a417I (Maybe Color))
progressConfig_color
  f_a41kc
  (ProgressConfig x1_a41kd
                  x2_a41ke
                  x3_a41kf
                  x4_a41kg
                  x5_a41kh
                  x6_a41ki
                  x7_a41kj
                  x8_a41kk
                  x9_a41kl
                  x10_a41km
                  x11_a41kn
                  x12_a41ko
                  x13_a41kp
                  x14_a41kq
                  x15_a41kr
                  x16_a41ks)
  = fmap
      (\ y1_a41kt
         -> ProgressConfig
              x1_a41kd
              x2_a41ke
              x3_a41kf
              x4_a41kg
              x5_a41kh
              x6_a41ki
              x7_a41kj
              x8_a41kk
              x9_a41kl
              x10_a41km
              x11_a41kn
              x12_a41ko
              y1_a41kt
              x14_a41kq
              x15_a41kr
              x16_a41ks)
      (f_a41kc x13_a41kp)
{-# INLINE progressConfig_color #-}
progressConfig_disabled ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Dynamic t_a417I Bool)
progressConfig_disabled
  f_a41ku
  (ProgressConfig x1_a41kv
                  x2_a41kw
                  x3_a41kx
                  x4_a41ky
                  x5_a41kz
                  x6_a41kA
                  x7_a41kB
                  x8_a41kC
                  x9_a41kD
                  x10_a41kE
                  x11_a41kF
                  x12_a41kG
                  x13_a41kH
                  x14_a41kI
                  x15_a41kJ
                  x16_a41kK)
  = fmap
      (\ y1_a41kL
         -> ProgressConfig
              x1_a41kv
              x2_a41kw
              x3_a41kx
              x4_a41ky
              y1_a41kL
              x6_a41kA
              x7_a41kB
              x8_a41kC
              x9_a41kD
              x10_a41kE
              x11_a41kF
              x12_a41kG
              x13_a41kH
              x14_a41kI
              x15_a41kJ
              x16_a41kK)
      (f_a41ku x5_a41kz)
{-# INLINE progressConfig_disabled #-}
progressConfig_elConfig ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (ActiveElConfig t_a417I)
progressConfig_elConfig
  f_a41kM
  (ProgressConfig x1_a41kN
                  x2_a41kO
                  x3_a41kP
                  x4_a41kQ
                  x5_a41kR
                  x6_a41kS
                  x7_a41kT
                  x8_a41kU
                  x9_a41kV
                  x10_a41kW
                  x11_a41kX
                  x12_a41kY
                  x13_a41kZ
                  x14_a41l0
                  x15_a41l1
                  x16_a41l2)
  = fmap
      (\ y1_a41l3
         -> ProgressConfig
              x1_a41kN
              x2_a41kO
              x3_a41kP
              x4_a41kQ
              x5_a41kR
              x6_a41kS
              x7_a41kT
              x8_a41kU
              x9_a41kV
              x10_a41kW
              x11_a41kX
              x12_a41kY
              x13_a41kZ
              x14_a41l0
              x15_a41l1
              y1_a41l3)
      (f_a41kM x16_a41l2)
{-# INLINE progressConfig_elConfig #-}
progressConfig_error ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Dynamic t_a417I Bool)
progressConfig_error
  f_a41l4
  (ProgressConfig x1_a41l5
                  x2_a41l6
                  x3_a41l7
                  x4_a41l8
                  x5_a41l9
                  x6_a41la
                  x7_a41lb
                  x8_a41lc
                  x9_a41ld
                  x10_a41le
                  x11_a41lf
                  x12_a41lg
                  x13_a41lh
                  x14_a41li
                  x15_a41lj
                  x16_a41lk)
  = fmap
      (\ y1_a41ll
         -> ProgressConfig
              x1_a41l5
              x2_a41l6
              x3_a41l7
              x4_a41l8
              x5_a41l9
              x6_a41la
              x7_a41lb
              x8_a41lc
              y1_a41ll
              x10_a41le
              x11_a41lf
              x12_a41lg
              x13_a41lh
              x14_a41li
              x15_a41lj
              x16_a41lk)
      (f_a41l4 x9_a41ld)
{-# INLINE progressConfig_error #-}
progressConfig_indeterminate ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Dynamic t_a417I Bool)
progressConfig_indeterminate
  f_a41lm
  (ProgressConfig x1_a41ln
                  x2_a41lo
                  x3_a41lp
                  x4_a41lq
                  x5_a41lr
                  x6_a41ls
                  x7_a41lt
                  x8_a41lu
                  x9_a41lv
                  x10_a41lw
                  x11_a41lx
                  x12_a41ly
                  x13_a41lz
                  x14_a41lA
                  x15_a41lB
                  x16_a41lC)
  = fmap
      (\ y1_a41lD
         -> ProgressConfig
              x1_a41ln
              x2_a41lo
              x3_a41lp
              x4_a41lq
              x5_a41lr
              y1_a41lD
              x7_a41lt
              x8_a41lu
              x9_a41lv
              x10_a41lw
              x11_a41lx
              x12_a41ly
              x13_a41lz
              x14_a41lA
              x15_a41lB
              x16_a41lC)
      (f_a41lm x6_a41ls)
{-# INLINE progressConfig_indeterminate #-}
progressConfig_indicating ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Dynamic t_a417I Bool)
progressConfig_indicating
  f_a41lE
  (ProgressConfig x1_a41lF
                  x2_a41lG
                  x3_a41lH
                  x4_a41lI
                  x5_a41lJ
                  x6_a41lK
                  x7_a41lL
                  x8_a41lM
                  x9_a41lN
                  x10_a41lO
                  x11_a41lP
                  x12_a41lQ
                  x13_a41lR
                  x14_a41lS
                  x15_a41lT
                  x16_a41lU)
  = fmap
      (\ y1_a41lV
         -> ProgressConfig
              x1_a41lF
              x2_a41lG
              y1_a41lV
              x4_a41lI
              x5_a41lJ
              x6_a41lK
              x7_a41lL
              x8_a41lM
              x9_a41lN
              x10_a41lO
              x11_a41lP
              x12_a41lQ
              x13_a41lR
              x14_a41lS
              x15_a41lT
              x16_a41lU)
      (f_a41lE x3_a41lH)
{-# INLINE progressConfig_indicating #-}
progressConfig_inverted ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Dynamic t_a417I Bool)
progressConfig_inverted
  f_a41lW
  (ProgressConfig x1_a41lX
                  x2_a41lY
                  x3_a41lZ
                  x4_a41m0
                  x5_a41m1
                  x6_a41m2
                  x7_a41m3
                  x8_a41m4
                  x9_a41m5
                  x10_a41m6
                  x11_a41m7
                  x12_a41m8
                  x13_a41m9
                  x14_a41ma
                  x15_a41mb
                  x16_a41mc)
  = fmap
      (\ y1_a41md
         -> ProgressConfig
              x1_a41lX
              x2_a41lY
              x3_a41lZ
              x4_a41m0
              x5_a41m1
              x6_a41m2
              x7_a41m3
              x8_a41m4
              x9_a41m5
              y1_a41md
              x11_a41m7
              x12_a41m8
              x13_a41m9
              x14_a41ma
              x15_a41mb
              x16_a41mc)
      (f_a41lW x10_a41m6)
{-# INLINE progressConfig_inverted #-}
progressConfig_label ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Maybe (m_a417J ()))
progressConfig_label
  f_a41me
  (ProgressConfig x1_a41mf
                  x2_a41mg
                  x3_a41mh
                  x4_a41mi
                  x5_a41mj
                  x6_a41mk
                  x7_a41ml
                  x8_a41mm
                  x9_a41mn
                  x10_a41mo
                  x11_a41mp
                  x12_a41mq
                  x13_a41mr
                  x14_a41ms
                  x15_a41mt
                  x16_a41mu)
  = fmap
      (\ y1_a41mv
         -> ProgressConfig
              x1_a41mf
              y1_a41mv
              x3_a41mh
              x4_a41mi
              x5_a41mj
              x6_a41mk
              x7_a41ml
              x8_a41mm
              x9_a41mn
              x10_a41mo
              x11_a41mp
              x12_a41mq
              x13_a41mr
              x14_a41ms
              x15_a41mt
              x16_a41mu)
      (f_a41me x2_a41mg)
{-# INLINE progressConfig_label #-}
progressConfig_minWidth ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Dynamic t_a417I Bool)
progressConfig_minWidth
  f_a41mw
  (ProgressConfig x1_a41mx
                  x2_a41my
                  x3_a41mz
                  x4_a41mA
                  x5_a41mB
                  x6_a41mC
                  x7_a41mD
                  x8_a41mE
                  x9_a41mF
                  x10_a41mG
                  x11_a41mH
                  x12_a41mI
                  x13_a41mJ
                  x14_a41mK
                  x15_a41mL
                  x16_a41mM)
  = fmap
      (\ y1_a41mN
         -> ProgressConfig
              x1_a41mx
              x2_a41my
              x3_a41mz
              x4_a41mA
              x5_a41mB
              x6_a41mC
              x7_a41mD
              x8_a41mE
              x9_a41mF
              x10_a41mG
              x11_a41mH
              x12_a41mI
              x13_a41mJ
              y1_a41mN
              x15_a41mL
              x16_a41mM)
      (f_a41mw x14_a41mK)
{-# INLINE progressConfig_minWidth #-}
progressConfig_rateLimit ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Maybe NominalDiffTime)
progressConfig_rateLimit
  f_a41mO
  (ProgressConfig x1_a41mP
                  x2_a41mQ
                  x3_a41mR
                  x4_a41mS
                  x5_a41mT
                  x6_a41mU
                  x7_a41mV
                  x8_a41mW
                  x9_a41mX
                  x10_a41mY
                  x11_a41mZ
                  x12_a41n0
                  x13_a41n1
                  x14_a41n2
                  x15_a41n3
                  x16_a41n4)
  = fmap
      (\ y1_a41n5
         -> ProgressConfig
              x1_a41mP
              x2_a41mQ
              x3_a41mR
              x4_a41mS
              x5_a41mT
              x6_a41mU
              x7_a41mV
              x8_a41mW
              x9_a41mX
              x10_a41mY
              x11_a41mZ
              x12_a41n0
              x13_a41n1
              x14_a41n2
              y1_a41n5
              x16_a41n4)
      (f_a41mO x15_a41n3)
{-# INLINE progressConfig_rateLimit #-}
progressConfig_size ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Dynamic t_a417I (Maybe Size))
progressConfig_size
  f_a41n6
  (ProgressConfig x1_a41n7
                  x2_a41n8
                  x3_a41n9
                  x4_a41na
                  x5_a41nb
                  x6_a41nc
                  x7_a41nd
                  x8_a41ne
                  x9_a41nf
                  x10_a41ng
                  x11_a41nh
                  x12_a41ni
                  x13_a41nj
                  x14_a41nk
                  x15_a41nl
                  x16_a41nm)
  = fmap
      (\ y1_a41nn
         -> ProgressConfig
              x1_a41n7
              x2_a41n8
              x3_a41n9
              x4_a41na
              x5_a41nb
              x6_a41nc
              x7_a41nd
              x8_a41ne
              x9_a41nf
              x10_a41ng
              x11_a41nh
              y1_a41nn
              x13_a41nj
              x14_a41nk
              x15_a41nl
              x16_a41nm)
      (f_a41n6 x12_a41ni)
{-# INLINE progressConfig_size #-}
progressConfig_success ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Dynamic t_a417I (Maybe Bool))
progressConfig_success
  f_a41no
  (ProgressConfig x1_a41np
                  x2_a41nq
                  x3_a41nr
                  x4_a41ns
                  x5_a41nt
                  x6_a41nu
                  x7_a41nv
                  x8_a41nw
                  x9_a41nx
                  x10_a41ny
                  x11_a41nz
                  x12_a41nA
                  x13_a41nB
                  x14_a41nC
                  x15_a41nD
                  x16_a41nE)
  = fmap
      (\ y1_a41nF
         -> ProgressConfig
              x1_a41np
              x2_a41nq
              x3_a41nr
              x4_a41ns
              x5_a41nt
              x6_a41nu
              y1_a41nF
              x8_a41nw
              x9_a41nx
              x10_a41ny
              x11_a41nz
              x12_a41nA
              x13_a41nB
              x14_a41nC
              x15_a41nD
              x16_a41nE)
      (f_a41no x7_a41nv)
{-# INLINE progressConfig_success #-}
progressConfig_warning ::
  forall t_a417I m_a417J.
  Control.Lens.Type.Lens' (ProgressConfig t_a417I m_a417J) (Dynamic t_a417I Bool)
progressConfig_warning
  f_a41nG
  (ProgressConfig x1_a41nH
                  x2_a41nI
                  x3_a41nJ
                  x4_a41nK
                  x5_a41nL
                  x6_a41nM
                  x7_a41nN
                  x8_a41nO
                  x9_a41nP
                  x10_a41nQ
                  x11_a41nR
                  x12_a41nS
                  x13_a41nT
                  x14_a41nU
                  x15_a41nV
                  x16_a41nW)
  = fmap
      (\ y1_a41nX
         -> ProgressConfig
              x1_a41nH
              x2_a41nI
              x3_a41nJ
              x4_a41nK
              x5_a41nL
              x6_a41nM
              x7_a41nN
              y1_a41nX
              x9_a41nP
              x10_a41nQ
              x11_a41nR
              x12_a41nS
              x13_a41nT
              x14_a41nU
              x15_a41nV
              x16_a41nW)
      (f_a41nG x8_a41nO)
{-# INLINE progressConfig_warning #-}
-- src/Reflex/Dom/SemanticUI/Progress.hs:180:1-60: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''Progress
--   ======>
progress_barElement ::
  forall t_a41nY m_a41nZ.
  Control.Lens.Type.Lens' (Progress t_a41nY m_a41nZ) (Element EventResult (DomBuilderSpace m_a41nZ) t_a41nY)
progress_barElement f_a41BA (Progress x1_a41BB x2_a41BC x3_a41BD)
  = fmap
      (\ y1_a41BE -> Progress x1_a41BB x2_a41BC y1_a41BE)
      (f_a41BA x3_a41BD)
{-# INLINE progress_barElement #-}
progress_element ::
  forall t_a41nY m_a41nZ.
  Control.Lens.Type.Lens' (Progress t_a41nY m_a41nZ) (Element EventResult (DomBuilderSpace m_a41nZ) t_a41nY)
progress_element f_a41BF (Progress x1_a41BG x2_a41BH x3_a41BI)
  = fmap
      (\ y1_a41BJ -> Progress x1_a41BG y1_a41BJ x3_a41BI)
      (f_a41BF x2_a41BH)
{-# INLINE progress_element #-}
progress_percent ::
  forall t_a41nY m_a41nZ.
  Control.Lens.Type.Lens' (Progress t_a41nY m_a41nZ) (Dynamic t_a41nY (Maybe Percent))
progress_percent f_a41BK (Progress x1_a41BL x2_a41BM x3_a41BN)
  = fmap
      (\ y1_a41BO -> Progress y1_a41BO x2_a41BM x3_a41BN)
      (f_a41BK x1_a41BL)
{-# INLINE progress_percent #-}

