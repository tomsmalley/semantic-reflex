-- src/Reflex/Dom/SemanticUI/Segment.hs:96:1-65: Splicing declarations
segmentConfig_aligned ::
  forall t_a25jJ.
  Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ (Maybe Aligned))
segmentConfig_aligned
  f_a25vK
  (SegmentConfig x1_a25vL
                 x2_a25vM
                 x3_a25vN
                 x4_a25vO
                 x5_a25vP
                 x6_a25vQ
                 x7_a25vR
                 x8_a25vS
                 x9_a25vT
                 x10_a25vU
                 x11_a25vV
                 x12_a25vW
                 x13_a25vX
                 x14_a25vY
                 x15_a25vZ
                 x16_a25w0)
  = fmap
      (\ y1_a25w1
         -> SegmentConfig
              x1_a25vL
              x2_a25vM
              x3_a25vN
              x4_a25vO
              x5_a25vP
              x6_a25vQ
              x7_a25vR
              x8_a25vS
              x9_a25vT
              x10_a25vU
              x11_a25vV
              x12_a25vW
              x13_a25vX
              y1_a25w1
              x15_a25vZ
              x16_a25w0)
      (f_a25vK x14_a25vY)
{-# INLINE segmentConfig_aligned #-}
segmentConfig_attached ::
  forall t_a25jJ.
  Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ (Maybe VerticalAttached))
segmentConfig_attached
  f_a25w3
  (SegmentConfig x1_a25w4
                 x2_a25w5
                 x3_a25w6
                 x4_a25w7
                 x5_a25w8
                 x6_a25w9
                 x7_a25wa
                 x8_a25wb
                 x9_a25wc
                 x10_a25wd
                 x11_a25we
                 x12_a25wg
                 x13_a25wh
                 x14_a25wi
                 x15_a25wj
                 x16_a25wk)
  = fmap
      (\ y1_a25wl
         -> SegmentConfig
              x1_a25w4
              x2_a25w5
              x3_a25w6
              x4_a25w7
              x5_a25w8
              x6_a25w9
              x7_a25wa
              x8_a25wb
              x9_a25wc
              y1_a25wl
              x11_a25we
              x12_a25wg
              x13_a25wh
              x14_a25wi
              x15_a25wj
              x16_a25wk)
      (f_a25w3 x10_a25wd)
{-# INLINE segmentConfig_attached #-}
segmentConfig_basic ::
  forall t_a25jJ. Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ Bool)
segmentConfig_basic
  f_a25wo
  (SegmentConfig x1_a25wp
                 x2_a25wq
                 x3_a25wr
                 x4_a25ws
                 x5_a25wt
                 x6_a25wu
                 x7_a25wv
                 x8_a25wx
                 x9_a25wy
                 x10_a25wz
                 x11_a25wA
                 x12_a25wB
                 x13_a25wC
                 x14_a25wD
                 x15_a25wE
                 x16_a25wF)
  = fmap
      (\ y1_a25wG
         -> SegmentConfig
              x1_a25wp
              x2_a25wq
              x3_a25wr
              x4_a25ws
              x5_a25wt
              x6_a25wu
              x7_a25wv
              y1_a25wG
              x9_a25wy
              x10_a25wz
              x11_a25wA
              x12_a25wB
              x13_a25wC
              x14_a25wD
              x15_a25wE
              x16_a25wF)
      (f_a25wo x8_a25wx)
{-# INLINE segmentConfig_basic #-}
segmentConfig_circular ::
  forall t_a25jJ. Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ Bool)
segmentConfig_circular
  f_a25wI
  (SegmentConfig x1_a25wJ
                 x2_a25wK
                 x3_a25wL
                 x4_a25wM
                 x5_a25wN
                 x6_a25wO
                 x7_a25wP
                 x8_a25wQ
                 x9_a25wR
                 x10_a25wS
                 x11_a25wT
                 x12_a25wU
                 x13_a25wV
                 x14_a25wW
                 x15_a25wX
                 x16_a25wY)
  = fmap
      (\ y1_a25x0
         -> SegmentConfig
              x1_a25wJ
              x2_a25wK
              x3_a25wL
              x4_a25wM
              x5_a25wN
              y1_a25x0
              x7_a25wP
              x8_a25wQ
              x9_a25wR
              x10_a25wS
              x11_a25wT
              x12_a25wU
              x13_a25wV
              x14_a25wW
              x15_a25wX
              x16_a25wY)
      (f_a25wI x6_a25wO)
{-# INLINE segmentConfig_circular #-}
segmentConfig_clearing ::
  forall t_a25jJ. Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ Bool)
segmentConfig_clearing
  f_a25x2
  (SegmentConfig x1_a25x3
                 x2_a25x4
                 x3_a25x5
                 x4_a25x6
                 x5_a25x7
                 x6_a25x8
                 x7_a25x9
                 x8_a25xa
                 x9_a25xc
                 x10_a25xd
                 x11_a25xe
                 x12_a25xf
                 x13_a25xg
                 x14_a25xh
                 x15_a25xi
                 x16_a25xk)
  = fmap
      (\ y1_a25xl
         -> SegmentConfig
              x1_a25x3
              x2_a25x4
              x3_a25x5
              x4_a25x6
              x5_a25x7
              x6_a25x8
              y1_a25xl
              x8_a25xa
              x9_a25xc
              x10_a25xd
              x11_a25xe
              x12_a25xf
              x13_a25xg
              x14_a25xh
              x15_a25xi
              x16_a25xk)
      (f_a25x2 x7_a25x9)
{-# INLINE segmentConfig_clearing #-}
segmentConfig_color ::
  forall t_a25jJ.
  Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ (Maybe Color))
segmentConfig_color
  f_a25xn
  (SegmentConfig x1_a25xo
                 x2_a25xp
                 x3_a25xq
                 x4_a25xr
                 x5_a25xs
                 x6_a25xt
                 x7_a25xu
                 x8_a25xv
                 x9_a25xw
                 x10_a25xx
                 x11_a25xy
                 x12_a25xz
                 x13_a25xA
                 x14_a25xB
                 x15_a25xC
                 x16_a25xE)
  = fmap
      (\ y1_a25xF
         -> SegmentConfig
              x1_a25xo
              x2_a25xp
              x3_a25xq
              x4_a25xr
              x5_a25xs
              x6_a25xt
              x7_a25xu
              x8_a25xv
              x9_a25xw
              x10_a25xx
              y1_a25xF
              x12_a25xz
              x13_a25xA
              x14_a25xB
              x15_a25xC
              x16_a25xE)
      (f_a25xn x11_a25xy)
{-# INLINE segmentConfig_color #-}
segmentConfig_compact ::
  forall t_a25jJ. Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ Bool)
segmentConfig_compact
  f_a25xH
  (SegmentConfig x1_a25xI
                 x2_a25xJ
                 x3_a25xK
                 x4_a25xL
                 x5_a25xM
                 x6_a25xN
                 x7_a25xO
                 x8_a25xP
                 x9_a25xQ
                 x10_a25xR
                 x11_a25xS
                 x12_a25xT
                 x13_a25xU
                 x14_a25xV
                 x15_a25xW
                 x16_a25xX)
  = fmap
      (\ y1_a25xY
         -> SegmentConfig
              x1_a25xI
              x2_a25xJ
              x3_a25xK
              x4_a25xL
              y1_a25xY
              x6_a25xN
              x7_a25xO
              x8_a25xP
              x9_a25xQ
              x10_a25xR
              x11_a25xS
              x12_a25xT
              x13_a25xU
              x14_a25xV
              x15_a25xW
              x16_a25xX)
      (f_a25xH x5_a25xM)
{-# INLINE segmentConfig_compact #-}
segmentConfig_elConfig ::
  forall t_a25jJ.
  Lens' (SegmentConfig t_a25jJ) (ActiveElConfig t_a25jJ)
segmentConfig_elConfig
  f_a25y0
  (SegmentConfig x1_a25y1
                 x2_a25y2
                 x3_a25y3
                 x4_a25y4
                 x5_a25y5
                 x6_a25y6
                 x7_a25y7
                 x8_a25y8
                 x9_a25ya
                 x10_a25yb
                 x11_a25yc
                 x12_a25yd
                 x13_a25ye
                 x14_a25yf
                 x15_a25yg
                 x16_a25yh)
  = fmap
      (\ y1_a25yi
         -> SegmentConfig
              x1_a25y1
              x2_a25y2
              x3_a25y3
              x4_a25y4
              x5_a25y5
              x6_a25y6
              x7_a25y7
              x8_a25y8
              x9_a25ya
              x10_a25yb
              x11_a25yc
              x12_a25yd
              x13_a25ye
              x14_a25yf
              x15_a25yg
              y1_a25yi)
      (f_a25y0 x16_a25yh)
{-# INLINE segmentConfig_elConfig #-}
segmentConfig_emphasis ::
  forall t_a25jJ.
  Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ Emphasis)
segmentConfig_emphasis
  f_a25yk
  (SegmentConfig x1_a25yl
                 x2_a25ym
                 x3_a25yn
                 x4_a25yo
                 x5_a25yp
                 x6_a25yq
                 x7_a25yr
                 x8_a25ys
                 x9_a25yt
                 x10_a25yu
                 x11_a25yv
                 x12_a25yw
                 x13_a25yx
                 x14_a25yy
                 x15_a25yA
                 x16_a25yB)
  = fmap
      (\ y1_a25yC
         -> SegmentConfig
              x1_a25yl
              x2_a25ym
              x3_a25yn
              x4_a25yo
              x5_a25yp
              x6_a25yq
              x7_a25yr
              x8_a25ys
              x9_a25yt
              x10_a25yu
              x11_a25yv
              y1_a25yC
              x13_a25yx
              x14_a25yy
              x15_a25yA
              x16_a25yB)
      (f_a25yk x12_a25yw)
{-# INLINE segmentConfig_emphasis #-}
segmentConfig_floated ::
  forall t_a25jJ.
  Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ (Maybe Floated))
segmentConfig_floated
  f_a25yE
  (SegmentConfig x1_a25yF
                 x2_a25yG
                 x3_a25yH
                 x4_a25yI
                 x5_a25yJ
                 x6_a25yK
                 x7_a25yL
                 x8_a25yM
                 x9_a25yN
                 x10_a25yO
                 x11_a25yP
                 x12_a25yR
                 x13_a25yS
                 x14_a25yT
                 x15_a25yU
                 x16_a25yV)
  = fmap
      (\ y1_a25yW
         -> SegmentConfig
              x1_a25yF
              x2_a25yG
              x3_a25yH
              x4_a25yI
              x5_a25yJ
              x6_a25yK
              x7_a25yL
              x8_a25yM
              x9_a25yN
              x10_a25yO
              x11_a25yP
              x12_a25yR
              y1_a25yW
              x14_a25yT
              x15_a25yU
              x16_a25yV)
      (f_a25yE x13_a25yS)
{-# INLINE segmentConfig_floated #-}
segmentConfig_inverted ::
  forall t_a25jJ. Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ Bool)
segmentConfig_inverted
  f_a25yY
  (SegmentConfig x1_a25yZ
                 x2_a25z0
                 x3_a25z1
                 x4_a25z2
                 x5_a25z3
                 x6_a25z4
                 x7_a25z6
                 x8_a25z7
                 x9_a25z8
                 x10_a25z9
                 x11_a25zb
                 x12_a25zc
                 x13_a25zd
                 x14_a25ze
                 x15_a25zf
                 x16_a25zg)
  = fmap
      (\ y1_a25zh
         -> SegmentConfig
              x1_a25yZ
              x2_a25z0
              y1_a25zh
              x4_a25z2
              x5_a25z3
              x6_a25z4
              x7_a25z6
              x8_a25z7
              x9_a25z8
              x10_a25z9
              x11_a25zb
              x12_a25zc
              x13_a25zd
              x14_a25ze
              x15_a25zf
              x16_a25zg)
      (f_a25yY x3_a25z1)
{-# INLINE segmentConfig_inverted #-}
segmentConfig_padded ::
  forall t_a25jJ. Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ Bool)
segmentConfig_padded
  f_a25zk
  (SegmentConfig x1_a25zl
                 x2_a25zm
                 x3_a25zn
                 x4_a25zo
                 x5_a25zp
                 x6_a25zq
                 x7_a25zr
                 x8_a25zs
                 x9_a25zt
                 x10_a25zu
                 x11_a25zv
                 x12_a25zw
                 x13_a25zx
                 x14_a25zy
                 x15_a25zz
                 x16_a25zA)
  = fmap
      (\ y1_a25zC
         -> SegmentConfig
              x1_a25zl
              x2_a25zm
              x3_a25zn
              y1_a25zC
              x5_a25zp
              x6_a25zq
              x7_a25zr
              x8_a25zs
              x9_a25zt
              x10_a25zu
              x11_a25zv
              x12_a25zw
              x13_a25zx
              x14_a25zy
              x15_a25zz
              x16_a25zA)
      (f_a25zk x4_a25zo)
{-# INLINE segmentConfig_padded #-}
segmentConfig_raised ::
  forall t_a25jJ. Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ Bool)
segmentConfig_raised
  f_a25zE
  (SegmentConfig x1_a25zF
                 x2_a25zG
                 x3_a25zH
                 x4_a25zI
                 x5_a25zJ
                 x6_a25zK
                 x7_a25zL
                 x8_a25zM
                 x9_a25zN
                 x10_a25zO
                 x11_a25zP
                 x12_a25zQ
                 x13_a25zR
                 x14_a25zS
                 x15_a25zT
                 x16_a25zU)
  = fmap
      (\ y1_a25zV
         -> SegmentConfig
              y1_a25zV
              x2_a25zG
              x3_a25zH
              x4_a25zI
              x5_a25zJ
              x6_a25zK
              x7_a25zL
              x8_a25zM
              x9_a25zN
              x10_a25zO
              x11_a25zP
              x12_a25zQ
              x13_a25zR
              x14_a25zS
              x15_a25zT
              x16_a25zU)
      (f_a25zE x1_a25zF)
{-# INLINE segmentConfig_raised #-}
segmentConfig_size ::
  forall t_a25jJ.
  Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ (Maybe Size))
segmentConfig_size
  f_a25zW
  (SegmentConfig x1_a25zX
                 x2_a25zY
                 x3_a25zZ
                 x4_a25A0
                 x5_a25A1
                 x6_a25A2
                 x7_a25A3
                 x8_a25A4
                 x9_a25A5
                 x10_a25A6
                 x11_a25A7
                 x12_a25A8
                 x13_a25A9
                 x14_a25Aa
                 x15_a25Ab
                 x16_a25Ac)
  = fmap
      (\ y1_a25Ad
         -> SegmentConfig
              x1_a25zX
              x2_a25zY
              x3_a25zZ
              x4_a25A0
              x5_a25A1
              x6_a25A2
              x7_a25A3
              x8_a25A4
              x9_a25A5
              x10_a25A6
              x11_a25A7
              x12_a25A8
              x13_a25A9
              x14_a25Aa
              y1_a25Ad
              x16_a25Ac)
      (f_a25zW x15_a25Ab)
{-# INLINE segmentConfig_size #-}
segmentConfig_stacked ::
  forall t_a25jJ.
  Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ (Maybe Stacked))
segmentConfig_stacked
  f_a25Af
  (SegmentConfig x1_a25Ag
                 x2_a25Ah
                 x3_a25Ai
                 x4_a25Aj
                 x5_a25Ak
                 x6_a25Al
                 x7_a25Am
                 x8_a25An
                 x9_a25Ao
                 x10_a25Ap
                 x11_a25Aq
                 x12_a25Ar
                 x13_a25As
                 x14_a25At
                 x15_a25Au
                 x16_a25Av)
  = fmap
      (\ y1_a25Aw
         -> SegmentConfig
              x1_a25Ag
              x2_a25Ah
              x3_a25Ai
              x4_a25Aj
              x5_a25Ak
              x6_a25Al
              x7_a25Am
              x8_a25An
              y1_a25Aw
              x10_a25Ap
              x11_a25Aq
              x12_a25Ar
              x13_a25As
              x14_a25At
              x15_a25Au
              x16_a25Av)
      (f_a25Af x9_a25Ao)
{-# INLINE segmentConfig_stacked #-}
segmentConfig_vertical ::
  forall t_a25jJ. Lens' (SegmentConfig t_a25jJ) (Active t_a25jJ Bool)
segmentConfig_vertical
  f_a25Ay
  (SegmentConfig x1_a25Az
                 x2_a25AA
                 x3_a25AB
                 x4_a25AC
                 x5_a25AD
                 x6_a25AE
                 x7_a25AF
                 x8_a25AG
                 x9_a25AH
                 x10_a25AI
                 x11_a25AJ
                 x12_a25AK
                 x13_a25AL
                 x14_a25AM
                 x15_a25AN
                 x16_a25AO)
  = fmap
      (\ y1_a25AP
         -> SegmentConfig
              x1_a25Az
              y1_a25AP
              x3_a25AB
              x4_a25AC
              x5_a25AD
              x6_a25AE
              x7_a25AF
              x8_a25AG
              x9_a25AH
              x10_a25AI
              x11_a25AJ
              x12_a25AK
              x13_a25AL
              x14_a25AM
              x15_a25AN
              x16_a25AO)
      (f_a25Ay x2_a25AA)
{-# INLINE segmentConfig_vertical #-}
-- src/Reflex/Dom/SemanticUI/Segment.hs:169:1-66: Splicing declarations
segmentsConfig_compact ::
  forall t_a25Sk.
  Lens' (SegmentsConfig t_a25Sk) (Active t_a25Sk Bool)
segmentsConfig_compact
  f_a26Ou
  (SegmentsConfig x1_a26Ov x2_a26Ow x3_a26Ox x4_a26Oy x5_a26Oz)
  = fmap
      (\ y1_a26OA
         -> SegmentsConfig x1_a26Ov x2_a26Ow x3_a26Ox y1_a26OA x5_a26Oz)
      (f_a26Ou x4_a26Oy)
{-# INLINE segmentsConfig_compact #-}
segmentsConfig_elConfig ::
  forall t_a25Sk.
  Lens' (SegmentsConfig t_a25Sk) (ActiveElConfig t_a25Sk)
segmentsConfig_elConfig
  f_a26OB
  (SegmentsConfig x1_a26OC x2_a26OD x3_a26OE x4_a26OF x5_a26OG)
  = fmap
      (\ y1_a26OH
         -> SegmentsConfig x1_a26OC x2_a26OD x3_a26OE x4_a26OF y1_a26OH)
      (f_a26OB x5_a26OG)
{-# INLINE segmentsConfig_elConfig #-}
segmentsConfig_horizontal ::
  forall t_a25Sk.
  Lens' (SegmentsConfig t_a25Sk) (Active t_a25Sk Bool)
segmentsConfig_horizontal
  f_a26OI
  (SegmentsConfig x1_a26OJ x2_a26OK x3_a26OL x4_a26OM x5_a26ON)
  = fmap
      (\ y1_a26OO
         -> SegmentsConfig y1_a26OO x2_a26OK x3_a26OL x4_a26OM x5_a26ON)
      (f_a26OI x1_a26OJ)
{-# INLINE segmentsConfig_horizontal #-}
segmentsConfig_raised ::
  forall t_a25Sk.
  Lens' (SegmentsConfig t_a25Sk) (Active t_a25Sk Bool)
segmentsConfig_raised
  f_a26OP
  (SegmentsConfig x1_a26OQ x2_a26OR x3_a26OS x4_a26OT x5_a26OU)
  = fmap
      (\ y1_a26OV
         -> SegmentsConfig x1_a26OQ y1_a26OV x3_a26OS x4_a26OT x5_a26OU)
      (f_a26OP x2_a26OR)
{-# INLINE segmentsConfig_raised #-}
segmentsConfig_stacked ::
  forall t_a25Sk.
  Lens' (SegmentsConfig t_a25Sk) (Active t_a25Sk (Maybe Stacked))
segmentsConfig_stacked
  f_a26OX
  (SegmentsConfig x1_a26OY x2_a26OZ x3_a26P0 x4_a26P1 x5_a26P2)
  = fmap
      (\ y1_a26P3
         -> SegmentsConfig x1_a26OY x2_a26OZ y1_a26P3 x4_a26P1 x5_a26P2)
      (f_a26OX x3_a26P0)
{-# INLINE segmentsConfig_stacked #-}
