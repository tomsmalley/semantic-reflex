-- src/Reflex/Dom/SemanticUI/Header.hs:104:1-64: Splicing declarations
headerConfig_aligned ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (Active t_a19r1 (Maybe Aligned))
headerConfig_aligned
  f_a19EA
  (HeaderConfig x1_a19EC
                x2_a19ED
                x3_a19EE
                x4_a19EF
                x5_a19EG
                x6_a19EH
                x7_a19EI
                x8_a19EJ
                x9_a19EK
                x10_a19EL
                x11_a19EM
                x12_a19EN
                x13_a19EO)
  = fmap
      (\ y1_a19EP
         -> HeaderConfig
              x1_a19EC
              x2_a19ED
              x3_a19EE
              x4_a19EF
              x5_a19EG
              x6_a19EH
              x7_a19EI
              x8_a19EJ
              y1_a19EP
              x10_a19EL
              x11_a19EM
              x12_a19EN
              x13_a19EO)
      (f_a19EA x9_a19EK)
{-# INLINE headerConfig_aligned #-}
headerConfig_attached ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (Active t_a19r1 (Maybe VerticalAttached))
headerConfig_attached
  f_a19EQ
  (HeaderConfig x1_a19ER
                x2_a19ES
                x3_a19ET
                x4_a19EU
                x5_a19EV
                x6_a19EW
                x7_a19EY
                x8_a19EZ
                x9_a19F0
                x10_a19F1
                x11_a19F2
                x12_a19F4
                x13_a19F5)
  = fmap
      (\ y1_a19F6
         -> HeaderConfig
              x1_a19ER
              x2_a19ES
              x3_a19ET
              x4_a19EU
              x5_a19EV
              x6_a19EW
              x7_a19EY
              x8_a19EZ
              x9_a19F0
              x10_a19F1
              y1_a19F6
              x12_a19F4
              x13_a19F5)
      (f_a19EQ x11_a19F2)
{-# INLINE headerConfig_attached #-}
headerConfig_block ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (Active t_a19r1 Bool)
headerConfig_block
  f_a19Fc
  (HeaderConfig x1_a19Fd
                x2_a19Fe
                x3_a19Ff
                x4_a19Fg
                x5_a19Fh
                x6_a19Fi
                x7_a19Fj
                x8_a19Fk
                x9_a19Fl
                x10_a19Fm
                x11_a19Fn
                x12_a19Fo
                x13_a19Fp)
  = fmap
      (\ y1_a19Fq
         -> HeaderConfig
              x1_a19Fd
              x2_a19Fe
              x3_a19Ff
              x4_a19Fg
              y1_a19Fq
              x6_a19Fi
              x7_a19Fj
              x8_a19Fk
              x9_a19Fl
              x10_a19Fm
              x11_a19Fn
              x12_a19Fo
              x13_a19Fp)
      (f_a19Fc x5_a19Fh)
{-# INLINE headerConfig_block #-}
headerConfig_color ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (Active t_a19r1 (Maybe Color))
headerConfig_color
  f_a19Fs
  (HeaderConfig x1_a19Ft
                x2_a19Fu
                x3_a19Fw
                x4_a19Fx
                x5_a19Fy
                x6_a19Fz
                x7_a19FA
                x8_a19FB
                x9_a19FC
                x10_a19FD
                x11_a19FE
                x12_a19FF
                x13_a19FG)
  = fmap
      (\ y1_a19FH
         -> HeaderConfig
              x1_a19Ft
              x2_a19Fu
              x3_a19Fw
              x4_a19Fx
              x5_a19Fy
              x6_a19Fz
              x7_a19FA
              x8_a19FB
              x9_a19FC
              y1_a19FH
              x11_a19FE
              x12_a19FF
              x13_a19FG)
      (f_a19Fs x10_a19FD)
{-# INLINE headerConfig_color #-}
headerConfig_disabled ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (Active t_a19r1 Bool)
headerConfig_disabled
  f_a19FJ
  (HeaderConfig x1_a19FK
                x2_a19FL
                x3_a19FM
                x4_a19FN
                x5_a19FO
                x6_a19FP
                x7_a19FQ
                x8_a19FR
                x9_a19FS
                x10_a19FT
                x11_a19FU
                x12_a19FV
                x13_a19FW)
  = fmap
      (\ y1_a19FX
         -> HeaderConfig
              x1_a19FK
              x2_a19FL
              x3_a19FM
              y1_a19FX
              x5_a19FO
              x6_a19FP
              x7_a19FQ
              x8_a19FR
              x9_a19FS
              x10_a19FT
              x11_a19FU
              x12_a19FV
              x13_a19FW)
      (f_a19FJ x4_a19FN)
{-# INLINE headerConfig_disabled #-}
headerConfig_dividing ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (Active t_a19r1 Bool)
headerConfig_dividing
  f_a19G0
  (HeaderConfig x1_a19G1
                x2_a19G2
                x3_a19G3
                x4_a19G4
                x5_a19G5
                x6_a19G6
                x7_a19G7
                x8_a19G8
                x9_a19G9
                x10_a19Ga
                x11_a19Gb
                x12_a19Gc
                x13_a19Gd)
  = fmap
      (\ y1_a19Ge
         -> HeaderConfig
              x1_a19G1
              y1_a19Ge
              x3_a19G3
              x4_a19G4
              x5_a19G5
              x6_a19G6
              x7_a19G7
              x8_a19G8
              x9_a19G9
              x10_a19Ga
              x11_a19Gb
              x12_a19Gc
              x13_a19Gd)
      (f_a19G0 x2_a19G2)
{-# INLINE headerConfig_dividing #-}
headerConfig_elConfig ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (ActiveElConfig t_a19r1)
headerConfig_elConfig
  f_a19Gf
  (HeaderConfig x1_a19Gg
                x2_a19Gh
                x3_a19Gi
                x4_a19Gj
                x5_a19Gk
                x6_a19Gl
                x7_a19Gm
                x8_a19Gn
                x9_a19Go
                x10_a19Gp
                x11_a19Gq
                x12_a19Gr
                x13_a19Gs)
  = fmap
      (\ y1_a19Gt
         -> HeaderConfig
              x1_a19Gg
              x2_a19Gh
              x3_a19Gi
              x4_a19Gj
              x5_a19Gk
              x6_a19Gl
              x7_a19Gm
              x8_a19Gn
              x9_a19Go
              x10_a19Gp
              x11_a19Gq
              x12_a19Gr
              y1_a19Gt)
      (f_a19Gf x13_a19Gs)
{-# INLINE headerConfig_elConfig #-}
headerConfig_floated ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (Active t_a19r1 (Maybe Floated))
headerConfig_floated
  f_a19Gv
  (HeaderConfig x1_a19Gw
                x2_a19Gx
                x3_a19Gy
                x4_a19Gz
                x5_a19GA
                x6_a19GB
                x7_a19GC
                x8_a19GD
                x9_a19GE
                x10_a19GF
                x11_a19GG
                x12_a19GH
                x13_a19GI)
  = fmap
      (\ y1_a19GJ
         -> HeaderConfig
              x1_a19Gw
              x2_a19Gx
              x3_a19Gy
              x4_a19Gz
              x5_a19GA
              x6_a19GB
              x7_a19GC
              y1_a19GJ
              x9_a19GE
              x10_a19GF
              x11_a19GG
              x12_a19GH
              x13_a19GI)
      (f_a19Gv x8_a19GD)
{-# INLINE headerConfig_floated #-}
headerConfig_inverted ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (Active t_a19r1 Bool)
headerConfig_inverted
  f_a19GL
  (HeaderConfig x1_a19GM
                x2_a19GN
                x3_a19GO
                x4_a19GP
                x5_a19GQ
                x6_a19GR
                x7_a19GS
                x8_a19GT
                x9_a19GU
                x10_a19GV
                x11_a19GW
                x12_a19GX
                x13_a19GY)
  = fmap
      (\ y1_a19GZ
         -> HeaderConfig
              x1_a19GM
              x2_a19GN
              x3_a19GO
              x4_a19GP
              x5_a19GQ
              y1_a19GZ
              x7_a19GS
              x8_a19GT
              x9_a19GU
              x10_a19GV
              x11_a19GW
              x12_a19GX
              x13_a19GY)
      (f_a19GL x6_a19GR)
{-# INLINE headerConfig_inverted #-}
headerConfig_largeIcon ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (Active t_a19r1 Bool)
headerConfig_largeIcon
  f_a19H1
  (HeaderConfig x1_a19H2
                x2_a19H3
                x3_a19H4
                x4_a19H5
                x5_a19H6
                x6_a19H7
                x7_a19H8
                x8_a19H9
                x9_a19Hb
                x10_a19Hc
                x11_a19Hd
                x12_a19He
                x13_a19Hf)
  = fmap
      (\ y1_a19Hg
         -> HeaderConfig
              y1_a19Hg
              x2_a19H3
              x3_a19H4
              x4_a19H5
              x5_a19H6
              x6_a19H7
              x7_a19H8
              x8_a19H9
              x9_a19Hb
              x10_a19Hc
              x11_a19Hd
              x12_a19He
              x13_a19Hf)
      (f_a19H1 x1_a19H2)
{-# INLINE headerConfig_largeIcon #-}
headerConfig_preContent ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (Maybe (m_a19r2 ()))
headerConfig_preContent
  f_a19Hi
  (HeaderConfig x1_a19Hj
                x2_a19Hk
                x3_a19Hl
                x4_a19Hm
                x5_a19Hn
                x6_a19Ho
                x7_a19Hp
                x8_a19Hq
                x9_a19Hr
                x10_a19Hs
                x11_a19Ht
                x12_a19Hu
                x13_a19Hv)
  = fmap
      (\ y1_a19Hw
         -> HeaderConfig
              x1_a19Hj
              x2_a19Hk
              x3_a19Hl
              x4_a19Hm
              x5_a19Hn
              x6_a19Ho
              x7_a19Hp
              x8_a19Hq
              x9_a19Hr
              x10_a19Hs
              x11_a19Ht
              y1_a19Hw
              x13_a19Hv)
      (f_a19Hi x12_a19Hu)
{-# INLINE headerConfig_preContent #-}
headerConfig_size ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (Active t_a19r1 (Maybe HeaderSize))
headerConfig_size
  f_a19Hy
  (HeaderConfig x1_a19Hz
                x2_a19HA
                x3_a19HC
                x4_a19HD
                x5_a19HE
                x6_a19HF
                x7_a19HG
                x8_a19HH
                x9_a19HI
                x10_a19HJ
                x11_a19HK
                x12_a19HL
                x13_a19HM)
  = fmap
      (\ y1_a19HN
         -> HeaderConfig
              x1_a19Hz
              x2_a19HA
              x3_a19HC
              x4_a19HD
              x5_a19HE
              x6_a19HF
              y1_a19HN
              x8_a19HH
              x9_a19HI
              x10_a19HJ
              x11_a19HK
              x12_a19HL
              x13_a19HM)
      (f_a19Hy x7_a19HG)
{-# INLINE headerConfig_size #-}
headerConfig_sub ::
  forall t_a19r1 m_a19r2.
  Lens' (HeaderConfig t_a19r1 m_a19r2) (Active t_a19r1 Bool)
headerConfig_sub
  f_a19HP
  (HeaderConfig x1_a19HQ
                x2_a19HR
                x3_a19HS
                x4_a19HT
                x5_a19HU
                x6_a19HV
                x7_a19HW
                x8_a19HX
                x9_a19HY
                x10_a19I0
                x11_a19I1
                x12_a19I2
                x13_a19I3)
  = fmap
      (\ y1_a19I4
         -> HeaderConfig
              x1_a19HQ
              x2_a19HR
              y1_a19I4
              x4_a19HT
              x5_a19HU
              x6_a19HV
              x7_a19HW
              x8_a19HX
              x9_a19HY
              x10_a19I0
              x11_a19I1
              x12_a19I2
              x13_a19I3)
      (f_a19HP x3_a19HS)
{-# INLINE headerConfig_sub #-}
