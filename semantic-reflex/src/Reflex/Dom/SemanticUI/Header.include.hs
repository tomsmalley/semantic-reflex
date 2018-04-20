-- src/Reflex/Dom/SemanticUI/Header.hs:100:1-64: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''HeaderConfig
--   ======>
headerConfig_aligned ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (Active t_a3eE8 (Maybe Aligned))
headerConfig_aligned
  f_a3eH7
  (HeaderConfig x1_a3eH8
                x2_a3eH9
                x3_a3eHa
                x4_a3eHb
                x5_a3eHc
                x6_a3eHd
                x7_a3eHe
                x8_a3eHf
                x9_a3eHg
                x10_a3eHh
                x11_a3eHi
                x12_a3eHj
                x13_a3eHk)
  = fmap
      (\ y1_a3eHl
         -> HeaderConfig
              x1_a3eH8
              x2_a3eH9
              x3_a3eHa
              x4_a3eHb
              x5_a3eHc
              x6_a3eHd
              x7_a3eHe
              x8_a3eHf
              y1_a3eHl
              x10_a3eHh
              x11_a3eHi
              x12_a3eHj
              x13_a3eHk)
      (f_a3eH7 x9_a3eHg)
{-# INLINE headerConfig_aligned #-}
headerConfig_attached ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (Active t_a3eE8 (Maybe VerticalAttached))
headerConfig_attached
  f_a3eHm
  (HeaderConfig x1_a3eHn
                x2_a3eHo
                x3_a3eHp
                x4_a3eHq
                x5_a3eHr
                x6_a3eHs
                x7_a3eHt
                x8_a3eHu
                x9_a3eHv
                x10_a3eHw
                x11_a3eHx
                x12_a3eHy
                x13_a3eHz)
  = fmap
      (\ y1_a3eHA
         -> HeaderConfig
              x1_a3eHn
              x2_a3eHo
              x3_a3eHp
              x4_a3eHq
              x5_a3eHr
              x6_a3eHs
              x7_a3eHt
              x8_a3eHu
              x9_a3eHv
              x10_a3eHw
              y1_a3eHA
              x12_a3eHy
              x13_a3eHz)
      (f_a3eHm x11_a3eHx)
{-# INLINE headerConfig_attached #-}
headerConfig_block ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (Active t_a3eE8 Bool)
headerConfig_block
  f_a3eHB
  (HeaderConfig x1_a3eHC
                x2_a3eHD
                x3_a3eHE
                x4_a3eHF
                x5_a3eHG
                x6_a3eHH
                x7_a3eHI
                x8_a3eHJ
                x9_a3eHK
                x10_a3eHL
                x11_a3eHM
                x12_a3eHN
                x13_a3eHO)
  = fmap
      (\ y1_a3eHP
         -> HeaderConfig
              x1_a3eHC
              x2_a3eHD
              x3_a3eHE
              x4_a3eHF
              y1_a3eHP
              x6_a3eHH
              x7_a3eHI
              x8_a3eHJ
              x9_a3eHK
              x10_a3eHL
              x11_a3eHM
              x12_a3eHN
              x13_a3eHO)
      (f_a3eHB x5_a3eHG)
{-# INLINE headerConfig_block #-}
headerConfig_color ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (Active t_a3eE8 (Maybe Color))
headerConfig_color
  f_a3eHQ
  (HeaderConfig x1_a3eHR
                x2_a3eHS
                x3_a3eHT
                x4_a3eHU
                x5_a3eHV
                x6_a3eHW
                x7_a3eHX
                x8_a3eHY
                x9_a3eHZ
                x10_a3eI0
                x11_a3eI1
                x12_a3eI2
                x13_a3eI3)
  = fmap
      (\ y1_a3eI4
         -> HeaderConfig
              x1_a3eHR
              x2_a3eHS
              x3_a3eHT
              x4_a3eHU
              x5_a3eHV
              x6_a3eHW
              x7_a3eHX
              x8_a3eHY
              x9_a3eHZ
              y1_a3eI4
              x11_a3eI1
              x12_a3eI2
              x13_a3eI3)
      (f_a3eHQ x10_a3eI0)
{-# INLINE headerConfig_color #-}
headerConfig_disabled ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (Active t_a3eE8 Bool)
headerConfig_disabled
  f_a3eI5
  (HeaderConfig x1_a3eI6
                x2_a3eI7
                x3_a3eI8
                x4_a3eI9
                x5_a3eIa
                x6_a3eIb
                x7_a3eIc
                x8_a3eId
                x9_a3eIe
                x10_a3eIf
                x11_a3eIg
                x12_a3eIh
                x13_a3eIi)
  = fmap
      (\ y1_a3eIj
         -> HeaderConfig
              x1_a3eI6
              x2_a3eI7
              x3_a3eI8
              y1_a3eIj
              x5_a3eIa
              x6_a3eIb
              x7_a3eIc
              x8_a3eId
              x9_a3eIe
              x10_a3eIf
              x11_a3eIg
              x12_a3eIh
              x13_a3eIi)
      (f_a3eI5 x4_a3eI9)
{-# INLINE headerConfig_disabled #-}
headerConfig_dividing ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (Active t_a3eE8 Bool)
headerConfig_dividing
  f_a3eIk
  (HeaderConfig x1_a3eIl
                x2_a3eIm
                x3_a3eIn
                x4_a3eIo
                x5_a3eIp
                x6_a3eIq
                x7_a3eIr
                x8_a3eIs
                x9_a3eIt
                x10_a3eIu
                x11_a3eIv
                x12_a3eIw
                x13_a3eIx)
  = fmap
      (\ y1_a3eIy
         -> HeaderConfig
              x1_a3eIl
              y1_a3eIy
              x3_a3eIn
              x4_a3eIo
              x5_a3eIp
              x6_a3eIq
              x7_a3eIr
              x8_a3eIs
              x9_a3eIt
              x10_a3eIu
              x11_a3eIv
              x12_a3eIw
              x13_a3eIx)
      (f_a3eIk x2_a3eIm)
{-# INLINE headerConfig_dividing #-}
headerConfig_elConfig ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (ActiveElConfig t_a3eE8)
headerConfig_elConfig
  f_a3eIz
  (HeaderConfig x1_a3eIA
                x2_a3eIB
                x3_a3eIC
                x4_a3eID
                x5_a3eIE
                x6_a3eIF
                x7_a3eIG
                x8_a3eIH
                x9_a3eII
                x10_a3eIJ
                x11_a3eIK
                x12_a3eIL
                x13_a3eIM)
  = fmap
      (\ y1_a3eIN
         -> HeaderConfig
              x1_a3eIA
              x2_a3eIB
              x3_a3eIC
              x4_a3eID
              x5_a3eIE
              x6_a3eIF
              x7_a3eIG
              x8_a3eIH
              x9_a3eII
              x10_a3eIJ
              x11_a3eIK
              x12_a3eIL
              y1_a3eIN)
      (f_a3eIz x13_a3eIM)
{-# INLINE headerConfig_elConfig #-}
headerConfig_floated ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (Active t_a3eE8 (Maybe Floated))
headerConfig_floated
  f_a3eIO
  (HeaderConfig x1_a3eIP
                x2_a3eIQ
                x3_a3eIR
                x4_a3eIS
                x5_a3eIT
                x6_a3eIU
                x7_a3eIV
                x8_a3eIW
                x9_a3eIX
                x10_a3eIY
                x11_a3eIZ
                x12_a3eJ0
                x13_a3eJ1)
  = fmap
      (\ y1_a3eJ2
         -> HeaderConfig
              x1_a3eIP
              x2_a3eIQ
              x3_a3eIR
              x4_a3eIS
              x5_a3eIT
              x6_a3eIU
              x7_a3eIV
              y1_a3eJ2
              x9_a3eIX
              x10_a3eIY
              x11_a3eIZ
              x12_a3eJ0
              x13_a3eJ1)
      (f_a3eIO x8_a3eIW)
{-# INLINE headerConfig_floated #-}
headerConfig_inverted ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (Active t_a3eE8 Bool)
headerConfig_inverted
  f_a3eJ3
  (HeaderConfig x1_a3eJ4
                x2_a3eJ5
                x3_a3eJ6
                x4_a3eJ7
                x5_a3eJ8
                x6_a3eJ9
                x7_a3eJa
                x8_a3eJb
                x9_a3eJc
                x10_a3eJd
                x11_a3eJe
                x12_a3eJf
                x13_a3eJg)
  = fmap
      (\ y1_a3eJh
         -> HeaderConfig
              x1_a3eJ4
              x2_a3eJ5
              x3_a3eJ6
              x4_a3eJ7
              x5_a3eJ8
              y1_a3eJh
              x7_a3eJa
              x8_a3eJb
              x9_a3eJc
              x10_a3eJd
              x11_a3eJe
              x12_a3eJf
              x13_a3eJg)
      (f_a3eJ3 x6_a3eJ9)
{-# INLINE headerConfig_inverted #-}
headerConfig_largeIcon ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (Active t_a3eE8 Bool)
headerConfig_largeIcon
  f_a3eJi
  (HeaderConfig x1_a3eJj
                x2_a3eJk
                x3_a3eJl
                x4_a3eJm
                x5_a3eJn
                x6_a3eJo
                x7_a3eJp
                x8_a3eJq
                x9_a3eJr
                x10_a3eJs
                x11_a3eJt
                x12_a3eJu
                x13_a3eJv)
  = fmap
      (\ y1_a3eJw
         -> HeaderConfig
              y1_a3eJw
              x2_a3eJk
              x3_a3eJl
              x4_a3eJm
              x5_a3eJn
              x6_a3eJo
              x7_a3eJp
              x8_a3eJq
              x9_a3eJr
              x10_a3eJs
              x11_a3eJt
              x12_a3eJu
              x13_a3eJv)
      (f_a3eJi x1_a3eJj)
{-# INLINE headerConfig_largeIcon #-}
headerConfig_preContent ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (Maybe (m_a3eE9 ()))
headerConfig_preContent
  f_a3eJx
  (HeaderConfig x1_a3eJy
                x2_a3eJz
                x3_a3eJA
                x4_a3eJB
                x5_a3eJC
                x6_a3eJD
                x7_a3eJE
                x8_a3eJF
                x9_a3eJG
                x10_a3eJH
                x11_a3eJI
                x12_a3eJJ
                x13_a3eJK)
  = fmap
      (\ y1_a3eJL
         -> HeaderConfig
              x1_a3eJy
              x2_a3eJz
              x3_a3eJA
              x4_a3eJB
              x5_a3eJC
              x6_a3eJD
              x7_a3eJE
              x8_a3eJF
              x9_a3eJG
              x10_a3eJH
              x11_a3eJI
              y1_a3eJL
              x13_a3eJK)
      (f_a3eJx x12_a3eJJ)
{-# INLINE headerConfig_preContent #-}
headerConfig_size ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (Active t_a3eE8 (Maybe HeaderSize))
headerConfig_size
  f_a3eJM
  (HeaderConfig x1_a3eJN
                x2_a3eJO
                x3_a3eJP
                x4_a3eJQ
                x5_a3eJR
                x6_a3eJS
                x7_a3eJT
                x8_a3eJU
                x9_a3eJV
                x10_a3eJW
                x11_a3eJX
                x12_a3eJY
                x13_a3eJZ)
  = fmap
      (\ y1_a3eK0
         -> HeaderConfig
              x1_a3eJN
              x2_a3eJO
              x3_a3eJP
              x4_a3eJQ
              x5_a3eJR
              x6_a3eJS
              y1_a3eK0
              x8_a3eJU
              x9_a3eJV
              x10_a3eJW
              x11_a3eJX
              x12_a3eJY
              x13_a3eJZ)
      (f_a3eJM x7_a3eJT)
{-# INLINE headerConfig_size #-}
headerConfig_sub ::
  forall t_a3eE8 m_a3eE9.
  Control.Lens.Type.Lens' (HeaderConfig t_a3eE8 m_a3eE9) (Active t_a3eE8 Bool)
headerConfig_sub
  f_a3eK1
  (HeaderConfig x1_a3eK2
                x2_a3eK3
                x3_a3eK4
                x4_a3eK5
                x5_a3eK6
                x6_a3eK7
                x7_a3eK8
                x8_a3eK9
                x9_a3eKa
                x10_a3eKb
                x11_a3eKc
                x12_a3eKd
                x13_a3eKe)
  = fmap
      (\ y1_a3eKf
         -> HeaderConfig
              x1_a3eK2
              x2_a3eK3
              y1_a3eKf
              x4_a3eK5
              x5_a3eK6
              x6_a3eK7
              x7_a3eK8
              x8_a3eK9
              x9_a3eKa
              x10_a3eKb
              x11_a3eKc
              x12_a3eKd
              x13_a3eKe)
      (f_a3eK1 x3_a3eK4)
{-# INLINE headerConfig_sub #-}

