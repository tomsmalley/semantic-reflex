-- src/Reflex/Dom/SemanticUI/Segment.hs:92:1-65: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''SegmentConfig
--   ======>
segmentConfig_aligned ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK (Maybe Aligned))
segmentConfig_aligned
  f_a49ob
  (SegmentConfig x1_a49oc
                 x2_a49od
                 x3_a49oe
                 x4_a49of
                 x5_a49og
                 x6_a49oh
                 x7_a49oi
                 x8_a49oj
                 x9_a49ok
                 x10_a49ol
                 x11_a49om
                 x12_a49on
                 x13_a49oo
                 x14_a49op
                 x15_a49oq
                 x16_a49or)
  = fmap
      (\ y1_a49os
         -> SegmentConfig
              x1_a49oc
              x2_a49od
              x3_a49oe
              x4_a49of
              x5_a49og
              x6_a49oh
              x7_a49oi
              x8_a49oj
              x9_a49ok
              x10_a49ol
              x11_a49om
              x12_a49on
              x13_a49oo
              y1_a49os
              x15_a49oq
              x16_a49or)
      (f_a49ob x14_a49op)
{-# INLINE segmentConfig_aligned #-}
segmentConfig_attached ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK (Maybe VerticalAttached))
segmentConfig_attached
  f_a49ot
  (SegmentConfig x1_a49ou
                 x2_a49ov
                 x3_a49ow
                 x4_a49ox
                 x5_a49oy
                 x6_a49oz
                 x7_a49oA
                 x8_a49oB
                 x9_a49oC
                 x10_a49oD
                 x11_a49oE
                 x12_a49oF
                 x13_a49oG
                 x14_a49oH
                 x15_a49oI
                 x16_a49oJ)
  = fmap
      (\ y1_a49oK
         -> SegmentConfig
              x1_a49ou
              x2_a49ov
              x3_a49ow
              x4_a49ox
              x5_a49oy
              x6_a49oz
              x7_a49oA
              x8_a49oB
              x9_a49oC
              y1_a49oK
              x11_a49oE
              x12_a49oF
              x13_a49oG
              x14_a49oH
              x15_a49oI
              x16_a49oJ)
      (f_a49ot x10_a49oD)
{-# INLINE segmentConfig_attached #-}
segmentConfig_basic ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK Bool)
segmentConfig_basic
  f_a49oL
  (SegmentConfig x1_a49oM
                 x2_a49oN
                 x3_a49oO
                 x4_a49oP
                 x5_a49oQ
                 x6_a49oR
                 x7_a49oS
                 x8_a49oT
                 x9_a49oU
                 x10_a49oV
                 x11_a49oW
                 x12_a49oX
                 x13_a49oY
                 x14_a49oZ
                 x15_a49p0
                 x16_a49p1)
  = fmap
      (\ y1_a49p2
         -> SegmentConfig
              x1_a49oM
              x2_a49oN
              x3_a49oO
              x4_a49oP
              x5_a49oQ
              x6_a49oR
              x7_a49oS
              y1_a49p2
              x9_a49oU
              x10_a49oV
              x11_a49oW
              x12_a49oX
              x13_a49oY
              x14_a49oZ
              x15_a49p0
              x16_a49p1)
      (f_a49oL x8_a49oT)
{-# INLINE segmentConfig_basic #-}
segmentConfig_circular ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK Bool)
segmentConfig_circular
  f_a49p3
  (SegmentConfig x1_a49p4
                 x2_a49p5
                 x3_a49p6
                 x4_a49p7
                 x5_a49p8
                 x6_a49p9
                 x7_a49pa
                 x8_a49pb
                 x9_a49pc
                 x10_a49pd
                 x11_a49pe
                 x12_a49pf
                 x13_a49pg
                 x14_a49ph
                 x15_a49pi
                 x16_a49pj)
  = fmap
      (\ y1_a49pk
         -> SegmentConfig
              x1_a49p4
              x2_a49p5
              x3_a49p6
              x4_a49p7
              x5_a49p8
              y1_a49pk
              x7_a49pa
              x8_a49pb
              x9_a49pc
              x10_a49pd
              x11_a49pe
              x12_a49pf
              x13_a49pg
              x14_a49ph
              x15_a49pi
              x16_a49pj)
      (f_a49p3 x6_a49p9)
{-# INLINE segmentConfig_circular #-}
segmentConfig_clearing ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK Bool)
segmentConfig_clearing
  f_a49pl
  (SegmentConfig x1_a49pm
                 x2_a49pn
                 x3_a49po
                 x4_a49pp
                 x5_a49pq
                 x6_a49pr
                 x7_a49ps
                 x8_a49pt
                 x9_a49pu
                 x10_a49pv
                 x11_a49pw
                 x12_a49px
                 x13_a49py
                 x14_a49pz
                 x15_a49pA
                 x16_a49pB)
  = fmap
      (\ y1_a49pC
         -> SegmentConfig
              x1_a49pm
              x2_a49pn
              x3_a49po
              x4_a49pp
              x5_a49pq
              x6_a49pr
              y1_a49pC
              x8_a49pt
              x9_a49pu
              x10_a49pv
              x11_a49pw
              x12_a49px
              x13_a49py
              x14_a49pz
              x15_a49pA
              x16_a49pB)
      (f_a49pl x7_a49ps)
{-# INLINE segmentConfig_clearing #-}
segmentConfig_color ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK (Maybe Color))
segmentConfig_color
  f_a49pD
  (SegmentConfig x1_a49pE
                 x2_a49pF
                 x3_a49pG
                 x4_a49pH
                 x5_a49pI
                 x6_a49pJ
                 x7_a49pK
                 x8_a49pL
                 x9_a49pM
                 x10_a49pN
                 x11_a49pO
                 x12_a49pP
                 x13_a49pQ
                 x14_a49pR
                 x15_a49pS
                 x16_a49pT)
  = fmap
      (\ y1_a49pU
         -> SegmentConfig
              x1_a49pE
              x2_a49pF
              x3_a49pG
              x4_a49pH
              x5_a49pI
              x6_a49pJ
              x7_a49pK
              x8_a49pL
              x9_a49pM
              x10_a49pN
              y1_a49pU
              x12_a49pP
              x13_a49pQ
              x14_a49pR
              x15_a49pS
              x16_a49pT)
      (f_a49pD x11_a49pO)
{-# INLINE segmentConfig_color #-}
segmentConfig_compact ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK Bool)
segmentConfig_compact
  f_a49pV
  (SegmentConfig x1_a49pW
                 x2_a49pX
                 x3_a49pY
                 x4_a49pZ
                 x5_a49q0
                 x6_a49q1
                 x7_a49q2
                 x8_a49q3
                 x9_a49q4
                 x10_a49q5
                 x11_a49q6
                 x12_a49q7
                 x13_a49q8
                 x14_a49q9
                 x15_a49qa
                 x16_a49qb)
  = fmap
      (\ y1_a49qc
         -> SegmentConfig
              x1_a49pW
              x2_a49pX
              x3_a49pY
              x4_a49pZ
              y1_a49qc
              x6_a49q1
              x7_a49q2
              x8_a49q3
              x9_a49q4
              x10_a49q5
              x11_a49q6
              x12_a49q7
              x13_a49q8
              x14_a49q9
              x15_a49qa
              x16_a49qb)
      (f_a49pV x5_a49q0)
{-# INLINE segmentConfig_compact #-}
segmentConfig_elConfig ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (ActiveElConfig t_a49lK)
segmentConfig_elConfig
  f_a49qd
  (SegmentConfig x1_a49qe
                 x2_a49qf
                 x3_a49qg
                 x4_a49qh
                 x5_a49qi
                 x6_a49qj
                 x7_a49qk
                 x8_a49ql
                 x9_a49qm
                 x10_a49qn
                 x11_a49qo
                 x12_a49qp
                 x13_a49qq
                 x14_a49qr
                 x15_a49qs
                 x16_a49qt)
  = fmap
      (\ y1_a49qu
         -> SegmentConfig
              x1_a49qe
              x2_a49qf
              x3_a49qg
              x4_a49qh
              x5_a49qi
              x6_a49qj
              x7_a49qk
              x8_a49ql
              x9_a49qm
              x10_a49qn
              x11_a49qo
              x12_a49qp
              x13_a49qq
              x14_a49qr
              x15_a49qs
              y1_a49qu)
      (f_a49qd x16_a49qt)
{-# INLINE segmentConfig_elConfig #-}
segmentConfig_emphasis ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK Emphasis)
segmentConfig_emphasis
  f_a49qv
  (SegmentConfig x1_a49qw
                 x2_a49qx
                 x3_a49qy
                 x4_a49qz
                 x5_a49qA
                 x6_a49qB
                 x7_a49qC
                 x8_a49qD
                 x9_a49qE
                 x10_a49qF
                 x11_a49qG
                 x12_a49qH
                 x13_a49qI
                 x14_a49qJ
                 x15_a49qK
                 x16_a49qL)
  = fmap
      (\ y1_a49qM
         -> SegmentConfig
              x1_a49qw
              x2_a49qx
              x3_a49qy
              x4_a49qz
              x5_a49qA
              x6_a49qB
              x7_a49qC
              x8_a49qD
              x9_a49qE
              x10_a49qF
              x11_a49qG
              y1_a49qM
              x13_a49qI
              x14_a49qJ
              x15_a49qK
              x16_a49qL)
      (f_a49qv x12_a49qH)
{-# INLINE segmentConfig_emphasis #-}
segmentConfig_floated ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK (Maybe Floated))
segmentConfig_floated
  f_a49qN
  (SegmentConfig x1_a49qO
                 x2_a49qP
                 x3_a49qQ
                 x4_a49qR
                 x5_a49qS
                 x6_a49qT
                 x7_a49qU
                 x8_a49qV
                 x9_a49qW
                 x10_a49qX
                 x11_a49qY
                 x12_a49qZ
                 x13_a49r0
                 x14_a49r1
                 x15_a49r2
                 x16_a49r3)
  = fmap
      (\ y1_a49r4
         -> SegmentConfig
              x1_a49qO
              x2_a49qP
              x3_a49qQ
              x4_a49qR
              x5_a49qS
              x6_a49qT
              x7_a49qU
              x8_a49qV
              x9_a49qW
              x10_a49qX
              x11_a49qY
              x12_a49qZ
              y1_a49r4
              x14_a49r1
              x15_a49r2
              x16_a49r3)
      (f_a49qN x13_a49r0)
{-# INLINE segmentConfig_floated #-}
segmentConfig_inverted ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK Bool)
segmentConfig_inverted
  f_a49r5
  (SegmentConfig x1_a49r6
                 x2_a49r7
                 x3_a49r8
                 x4_a49r9
                 x5_a49ra
                 x6_a49rb
                 x7_a49rc
                 x8_a49rd
                 x9_a49re
                 x10_a49rf
                 x11_a49rg
                 x12_a49rh
                 x13_a49ri
                 x14_a49rj
                 x15_a49rk
                 x16_a49rl)
  = fmap
      (\ y1_a49rm
         -> SegmentConfig
              x1_a49r6
              x2_a49r7
              y1_a49rm
              x4_a49r9
              x5_a49ra
              x6_a49rb
              x7_a49rc
              x8_a49rd
              x9_a49re
              x10_a49rf
              x11_a49rg
              x12_a49rh
              x13_a49ri
              x14_a49rj
              x15_a49rk
              x16_a49rl)
      (f_a49r5 x3_a49r8)
{-# INLINE segmentConfig_inverted #-}
segmentConfig_padded ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK Bool)
segmentConfig_padded
  f_a49rn
  (SegmentConfig x1_a49ro
                 x2_a49rp
                 x3_a49rq
                 x4_a49rr
                 x5_a49rs
                 x6_a49rt
                 x7_a49ru
                 x8_a49rv
                 x9_a49rw
                 x10_a49rx
                 x11_a49ry
                 x12_a49rz
                 x13_a49rA
                 x14_a49rB
                 x15_a49rC
                 x16_a49rD)
  = fmap
      (\ y1_a49rE
         -> SegmentConfig
              x1_a49ro
              x2_a49rp
              x3_a49rq
              y1_a49rE
              x5_a49rs
              x6_a49rt
              x7_a49ru
              x8_a49rv
              x9_a49rw
              x10_a49rx
              x11_a49ry
              x12_a49rz
              x13_a49rA
              x14_a49rB
              x15_a49rC
              x16_a49rD)
      (f_a49rn x4_a49rr)
{-# INLINE segmentConfig_padded #-}
segmentConfig_raised ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK Bool)
segmentConfig_raised
  f_a49rF
  (SegmentConfig x1_a49rG
                 x2_a49rH
                 x3_a49rI
                 x4_a49rJ
                 x5_a49rK
                 x6_a49rL
                 x7_a49rM
                 x8_a49rN
                 x9_a49rO
                 x10_a49rP
                 x11_a49rQ
                 x12_a49rR
                 x13_a49rS
                 x14_a49rT
                 x15_a49rU
                 x16_a49rV)
  = fmap
      (\ y1_a49rW
         -> SegmentConfig
              y1_a49rW
              x2_a49rH
              x3_a49rI
              x4_a49rJ
              x5_a49rK
              x6_a49rL
              x7_a49rM
              x8_a49rN
              x9_a49rO
              x10_a49rP
              x11_a49rQ
              x12_a49rR
              x13_a49rS
              x14_a49rT
              x15_a49rU
              x16_a49rV)
      (f_a49rF x1_a49rG)
{-# INLINE segmentConfig_raised #-}
segmentConfig_size ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK (Maybe Size))
segmentConfig_size
  f_a49rX
  (SegmentConfig x1_a49rY
                 x2_a49rZ
                 x3_a49s0
                 x4_a49s1
                 x5_a49s2
                 x6_a49s3
                 x7_a49s4
                 x8_a49s5
                 x9_a49s6
                 x10_a49s7
                 x11_a49s8
                 x12_a49s9
                 x13_a49sa
                 x14_a49sb
                 x15_a49sc
                 x16_a49sd)
  = fmap
      (\ y1_a49se
         -> SegmentConfig
              x1_a49rY
              x2_a49rZ
              x3_a49s0
              x4_a49s1
              x5_a49s2
              x6_a49s3
              x7_a49s4
              x8_a49s5
              x9_a49s6
              x10_a49s7
              x11_a49s8
              x12_a49s9
              x13_a49sa
              x14_a49sb
              y1_a49se
              x16_a49sd)
      (f_a49rX x15_a49sc)
{-# INLINE segmentConfig_size #-}
segmentConfig_stacked ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK (Maybe Stacked))
segmentConfig_stacked
  f_a49sf
  (SegmentConfig x1_a49sg
                 x2_a49sh
                 x3_a49si
                 x4_a49sj
                 x5_a49sk
                 x6_a49sl
                 x7_a49sm
                 x8_a49sn
                 x9_a49so
                 x10_a49sp
                 x11_a49sq
                 x12_a49sr
                 x13_a49ss
                 x14_a49st
                 x15_a49su
                 x16_a49sv)
  = fmap
      (\ y1_a49sw
         -> SegmentConfig
              x1_a49sg
              x2_a49sh
              x3_a49si
              x4_a49sj
              x5_a49sk
              x6_a49sl
              x7_a49sm
              x8_a49sn
              y1_a49sw
              x10_a49sp
              x11_a49sq
              x12_a49sr
              x13_a49ss
              x14_a49st
              x15_a49su
              x16_a49sv)
      (f_a49sf x9_a49so)
{-# INLINE segmentConfig_stacked #-}
segmentConfig_vertical ::
  forall t_a49lK.
  Control.Lens.Type.Lens' (SegmentConfig t_a49lK) (Active t_a49lK Bool)
segmentConfig_vertical
  f_a49sx
  (SegmentConfig x1_a49sy
                 x2_a49sz
                 x3_a49sA
                 x4_a49sB
                 x5_a49sC
                 x6_a49sD
                 x7_a49sE
                 x8_a49sF
                 x9_a49sG
                 x10_a49sH
                 x11_a49sI
                 x12_a49sJ
                 x13_a49sK
                 x14_a49sL
                 x15_a49sM
                 x16_a49sN)
  = fmap
      (\ y1_a49sO
         -> SegmentConfig
              x1_a49sy
              y1_a49sO
              x3_a49sA
              x4_a49sB
              x5_a49sC
              x6_a49sD
              x7_a49sE
              x8_a49sF
              x9_a49sG
              x10_a49sH
              x11_a49sI
              x12_a49sJ
              x13_a49sK
              x14_a49sL
              x15_a49sM
              x16_a49sN)
      (f_a49sx x2_a49sz)
{-# INLINE segmentConfig_vertical #-}
-- src/Reflex/Dom/SemanticUI/Segment.hs:163:1-66: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''SegmentsConfig
--   ======>
segmentsConfig_compact ::
  forall t_a49sP.
  Control.Lens.Type.Lens' (SegmentsConfig t_a49sP) (Active t_a49sP Bool)
segmentsConfig_compact
  f_a49O8
  (SegmentsConfig x1_a49O9 x2_a49Oa x3_a49Ob x4_a49Oc x5_a49Od)
  = fmap
      (\ y1_a49Oe
         -> SegmentsConfig x1_a49O9 x2_a49Oa x3_a49Ob y1_a49Oe x5_a49Od)
      (f_a49O8 x4_a49Oc)
{-# INLINE segmentsConfig_compact #-}
segmentsConfig_elConfig ::
  forall t_a49sP.
  Control.Lens.Type.Lens' (SegmentsConfig t_a49sP) (ActiveElConfig t_a49sP)
segmentsConfig_elConfig
  f_a49Of
  (SegmentsConfig x1_a49Og x2_a49Oh x3_a49Oi x4_a49Oj x5_a49Ok)
  = fmap
      (\ y1_a49Ol
         -> SegmentsConfig x1_a49Og x2_a49Oh x3_a49Oi x4_a49Oj y1_a49Ol)
      (f_a49Of x5_a49Ok)
{-# INLINE segmentsConfig_elConfig #-}
segmentsConfig_horizontal ::
  forall t_a49sP.
  Control.Lens.Type.Lens' (SegmentsConfig t_a49sP) (Active t_a49sP Bool)
segmentsConfig_horizontal
  f_a49Om
  (SegmentsConfig x1_a49On x2_a49Oo x3_a49Op x4_a49Oq x5_a49Or)
  = fmap
      (\ y1_a49Os
         -> SegmentsConfig y1_a49Os x2_a49Oo x3_a49Op x4_a49Oq x5_a49Or)
      (f_a49Om x1_a49On)
{-# INLINE segmentsConfig_horizontal #-}
segmentsConfig_raised ::
  forall t_a49sP.
  Control.Lens.Type.Lens' (SegmentsConfig t_a49sP) (Active t_a49sP Bool)
segmentsConfig_raised
  f_a49Ot
  (SegmentsConfig x1_a49Ou x2_a49Ov x3_a49Ow x4_a49Ox x5_a49Oy)
  = fmap
      (\ y1_a49Oz
         -> SegmentsConfig x1_a49Ou y1_a49Oz x3_a49Ow x4_a49Ox x5_a49Oy)
      (f_a49Ot x2_a49Ov)
{-# INLINE segmentsConfig_raised #-}
segmentsConfig_stacked ::
  forall t_a49sP.
  Control.Lens.Type.Lens' (SegmentsConfig t_a49sP) (Active t_a49sP (Maybe Stacked))
segmentsConfig_stacked
  f_a49OA
  (SegmentsConfig x1_a49OB x2_a49OC x3_a49OD x4_a49OE x5_a49OF)
  = fmap
      (\ y1_a49OG
         -> SegmentsConfig x1_a49OB x2_a49OC y1_a49OG x4_a49OE x5_a49OF)
      (f_a49OA x3_a49OD)
{-# INLINE segmentsConfig_stacked #-}

