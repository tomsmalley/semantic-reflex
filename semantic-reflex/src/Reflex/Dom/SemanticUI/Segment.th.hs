-- src/Reflex/Dom/SemanticUI/Segment.hs:96:1-65: Splicing declarations
segmentConfig_aligned ::
  forall t_a2Atd.
  Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd (Maybe Aligned))
segmentConfig_aligned
  f_a2AFZ
  (SegmentConfig x1_a2AG0
                 x2_a2AG1
                 x3_a2AG2
                 x4_a2AG3
                 x5_a2AG4
                 x6_a2AG5
                 x7_a2AG6
                 x8_a2AG7
                 x9_a2AG9
                 x10_a2AGa
                 x11_a2AGb
                 x12_a2AGc
                 x13_a2AGd
                 x14_a2AGe
                 x15_a2AGf
                 x16_a2AGg)
  = fmap
      (\ y1_a2AGh
         -> SegmentConfig
              x1_a2AG0
              x2_a2AG1
              x3_a2AG2
              x4_a2AG3
              x5_a2AG4
              x6_a2AG5
              x7_a2AG6
              x8_a2AG7
              x9_a2AG9
              x10_a2AGa
              x11_a2AGb
              x12_a2AGc
              x13_a2AGd
              y1_a2AGh
              x15_a2AGf
              x16_a2AGg)
      (f_a2AFZ x14_a2AGe)
{-# INLINE segmentConfig_aligned #-}
segmentConfig_attached ::
  forall t_a2Atd.
  Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd (Maybe VerticalAttached))
segmentConfig_attached
  f_a2AGj
  (SegmentConfig x1_a2AGk
                 x2_a2AGl
                 x3_a2AGm
                 x4_a2AGn
                 x5_a2AGo
                 x6_a2AGp
                 x7_a2AGq
                 x8_a2AGr
                 x9_a2AGs
                 x10_a2AGt
                 x11_a2AGu
                 x12_a2AGv
                 x13_a2AGw
                 x14_a2AGx
                 x15_a2AGy
                 x16_a2AGz)
  = fmap
      (\ y1_a2AGA
         -> SegmentConfig
              x1_a2AGk
              x2_a2AGl
              x3_a2AGm
              x4_a2AGn
              x5_a2AGo
              x6_a2AGp
              x7_a2AGq
              x8_a2AGr
              x9_a2AGs
              y1_a2AGA
              x11_a2AGu
              x12_a2AGv
              x13_a2AGw
              x14_a2AGx
              x15_a2AGy
              x16_a2AGz)
      (f_a2AGj x10_a2AGt)
{-# INLINE segmentConfig_attached #-}
segmentConfig_basic ::
  forall t_a2Atd. Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd Bool)
segmentConfig_basic
  f_a2AGE
  (SegmentConfig x1_a2AGG
                 x2_a2AGI
                 x3_a2AGJ
                 x4_a2AGK
                 x5_a2AGL
                 x6_a2AGM
                 x7_a2AGN
                 x8_a2AGO
                 x9_a2AGP
                 x10_a2AGQ
                 x11_a2AGR
                 x12_a2AGS
                 x13_a2AGT
                 x14_a2AGU
                 x15_a2AGV
                 x16_a2AGW)
  = fmap
      (\ y1_a2AGX
         -> SegmentConfig
              x1_a2AGG
              x2_a2AGI
              x3_a2AGJ
              x4_a2AGK
              x5_a2AGL
              x6_a2AGM
              x7_a2AGN
              y1_a2AGX
              x9_a2AGP
              x10_a2AGQ
              x11_a2AGR
              x12_a2AGS
              x13_a2AGT
              x14_a2AGU
              x15_a2AGV
              x16_a2AGW)
      (f_a2AGE x8_a2AGO)
{-# INLINE segmentConfig_basic #-}
segmentConfig_circular ::
  forall t_a2Atd. Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd Bool)
segmentConfig_circular
  f_a2AH0
  (SegmentConfig x1_a2AH1
                 x2_a2AH2
                 x3_a2AH3
                 x4_a2AH4
                 x5_a2AH5
                 x6_a2AH6
                 x7_a2AH7
                 x8_a2AH9
                 x9_a2AHa
                 x10_a2AHb
                 x11_a2AHc
                 x12_a2AHd
                 x13_a2AHe
                 x14_a2AHf
                 x15_a2AHg
                 x16_a2AHh)
  = fmap
      (\ y1_a2AHi
         -> SegmentConfig
              x1_a2AH1
              x2_a2AH2
              x3_a2AH3
              x4_a2AH4
              x5_a2AH5
              y1_a2AHi
              x7_a2AH7
              x8_a2AH9
              x9_a2AHa
              x10_a2AHb
              x11_a2AHc
              x12_a2AHd
              x13_a2AHe
              x14_a2AHf
              x15_a2AHg
              x16_a2AHh)
      (f_a2AH0 x6_a2AH6)
{-# INLINE segmentConfig_circular #-}
segmentConfig_clearing ::
  forall t_a2Atd. Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd Bool)
segmentConfig_clearing
  f_a2AHl
  (SegmentConfig x1_a2AHm
                 x2_a2AHo
                 x3_a2AHp
                 x4_a2AHq
                 x5_a2AHr
                 x6_a2AHs
                 x7_a2AHt
                 x8_a2AHu
                 x9_a2AHv
                 x10_a2AHw
                 x11_a2AHx
                 x12_a2AHy
                 x13_a2AHz
                 x14_a2AHA
                 x15_a2AHB
                 x16_a2AHC)
  = fmap
      (\ y1_a2AHD
         -> SegmentConfig
              x1_a2AHm
              x2_a2AHo
              x3_a2AHp
              x4_a2AHq
              x5_a2AHr
              x6_a2AHs
              y1_a2AHD
              x8_a2AHu
              x9_a2AHv
              x10_a2AHw
              x11_a2AHx
              x12_a2AHy
              x13_a2AHz
              x14_a2AHA
              x15_a2AHB
              x16_a2AHC)
      (f_a2AHl x7_a2AHt)
{-# INLINE segmentConfig_clearing #-}
segmentConfig_color ::
  forall t_a2Atd.
  Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd (Maybe Color))
segmentConfig_color
  f_a2AHE
  (SegmentConfig x1_a2AHF
                 x2_a2AHG
                 x3_a2AHH
                 x4_a2AHI
                 x5_a2AHJ
                 x6_a2AHK
                 x7_a2AHM
                 x8_a2AHN
                 x9_a2AHO
                 x10_a2AHP
                 x11_a2AHQ
                 x12_a2AHR
                 x13_a2AHS
                 x14_a2AHT
                 x15_a2AHU
                 x16_a2AHV)
  = fmap
      (\ y1_a2AHW
         -> SegmentConfig
              x1_a2AHF
              x2_a2AHG
              x3_a2AHH
              x4_a2AHI
              x5_a2AHJ
              x6_a2AHK
              x7_a2AHM
              x8_a2AHN
              x9_a2AHO
              x10_a2AHP
              y1_a2AHW
              x12_a2AHR
              x13_a2AHS
              x14_a2AHT
              x15_a2AHU
              x16_a2AHV)
      (f_a2AHE x11_a2AHQ)
{-# INLINE segmentConfig_color #-}
segmentConfig_compact ::
  forall t_a2Atd. Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd Bool)
segmentConfig_compact
  f_a2AHY
  (SegmentConfig x1_a2AHZ
                 x2_a2AI0
                 x3_a2AI1
                 x4_a2AI2
                 x5_a2AI3
                 x6_a2AI4
                 x7_a2AI5
                 x8_a2AI6
                 x9_a2AI7
                 x10_a2AI8
                 x11_a2AI9
                 x12_a2AIa
                 x13_a2AIb
                 x14_a2AIc
                 x15_a2AId
                 x16_a2AIe)
  = fmap
      (\ y1_a2AIf
         -> SegmentConfig
              x1_a2AHZ
              x2_a2AI0
              x3_a2AI1
              x4_a2AI2
              y1_a2AIf
              x6_a2AI4
              x7_a2AI5
              x8_a2AI6
              x9_a2AI7
              x10_a2AI8
              x11_a2AI9
              x12_a2AIa
              x13_a2AIb
              x14_a2AIc
              x15_a2AId
              x16_a2AIe)
      (f_a2AHY x5_a2AI3)
{-# INLINE segmentConfig_compact #-}
segmentConfig_elConfig ::
  forall t_a2Atd.
  Lens' (SegmentConfig t_a2Atd) (ActiveElConfig t_a2Atd)
segmentConfig_elConfig
  f_a2AIg
  (SegmentConfig x1_a2AIh
                 x2_a2AIi
                 x3_a2AIj
                 x4_a2AIk
                 x5_a2AIl
                 x6_a2AIm
                 x7_a2AIn
                 x8_a2AIo
                 x9_a2AIp
                 x10_a2AIq
                 x11_a2AIr
                 x12_a2AIs
                 x13_a2AIt
                 x14_a2AIu
                 x15_a2AIv
                 x16_a2AIw)
  = fmap
      (\ y1_a2AIx
         -> SegmentConfig
              x1_a2AIh
              x2_a2AIi
              x3_a2AIj
              x4_a2AIk
              x5_a2AIl
              x6_a2AIm
              x7_a2AIn
              x8_a2AIo
              x9_a2AIp
              x10_a2AIq
              x11_a2AIr
              x12_a2AIs
              x13_a2AIt
              x14_a2AIu
              x15_a2AIv
              y1_a2AIx)
      (f_a2AIg x16_a2AIw)
{-# INLINE segmentConfig_elConfig #-}
segmentConfig_emphasis ::
  forall t_a2Atd.
  Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd Emphasis)
segmentConfig_emphasis
  f_a2AIy
  (SegmentConfig x1_a2AIz
                 x2_a2AIA
                 x3_a2AIB
                 x4_a2AIC
                 x5_a2AID
                 x6_a2AIE
                 x7_a2AIF
                 x8_a2AIG
                 x9_a2AIH
                 x10_a2AII
                 x11_a2AIJ
                 x12_a2AIK
                 x13_a2AIL
                 x14_a2AIM
                 x15_a2AIN
                 x16_a2AIO)
  = fmap
      (\ y1_a2AIP
         -> SegmentConfig
              x1_a2AIz
              x2_a2AIA
              x3_a2AIB
              x4_a2AIC
              x5_a2AID
              x6_a2AIE
              x7_a2AIF
              x8_a2AIG
              x9_a2AIH
              x10_a2AII
              x11_a2AIJ
              y1_a2AIP
              x13_a2AIL
              x14_a2AIM
              x15_a2AIN
              x16_a2AIO)
      (f_a2AIy x12_a2AIK)
{-# INLINE segmentConfig_emphasis #-}
segmentConfig_floated ::
  forall t_a2Atd.
  Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd (Maybe Floated))
segmentConfig_floated
  f_a2AIQ
  (SegmentConfig x1_a2AIR
                 x2_a2AIS
                 x3_a2AIT
                 x4_a2AIU
                 x5_a2AIV
                 x6_a2AIW
                 x7_a2AIX
                 x8_a2AIY
                 x9_a2AIZ
                 x10_a2AJ0
                 x11_a2AJ1
                 x12_a2AJ2
                 x13_a2AJ3
                 x14_a2AJ4
                 x15_a2AJ5
                 x16_a2AJ7)
  = fmap
      (\ y1_a2AJ8
         -> SegmentConfig
              x1_a2AIR
              x2_a2AIS
              x3_a2AIT
              x4_a2AIU
              x5_a2AIV
              x6_a2AIW
              x7_a2AIX
              x8_a2AIY
              x9_a2AIZ
              x10_a2AJ0
              x11_a2AJ1
              x12_a2AJ2
              y1_a2AJ8
              x14_a2AJ4
              x15_a2AJ5
              x16_a2AJ7)
      (f_a2AIQ x13_a2AJ3)
{-# INLINE segmentConfig_floated #-}
segmentConfig_inverted ::
  forall t_a2Atd. Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd Bool)
segmentConfig_inverted
  f_a2AJa
  (SegmentConfig x1_a2AJc
                 x2_a2AJd
                 x3_a2AJe
                 x4_a2AJf
                 x5_a2AJh
                 x6_a2AJi
                 x7_a2AJk
                 x8_a2AJl
                 x9_a2AJm
                 x10_a2AJo
                 x11_a2AJp
                 x12_a2AJq
                 x13_a2AJr
                 x14_a2AJs
                 x15_a2AJt
                 x16_a2AJu)
  = fmap
      (\ y1_a2AJv
         -> SegmentConfig
              x1_a2AJc
              x2_a2AJd
              y1_a2AJv
              x4_a2AJf
              x5_a2AJh
              x6_a2AJi
              x7_a2AJk
              x8_a2AJl
              x9_a2AJm
              x10_a2AJo
              x11_a2AJp
              x12_a2AJq
              x13_a2AJr
              x14_a2AJs
              x15_a2AJt
              x16_a2AJu)
      (f_a2AJa x3_a2AJe)
{-# INLINE segmentConfig_inverted #-}
segmentConfig_padded ::
  forall t_a2Atd. Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd Bool)
segmentConfig_padded
  f_a2AJw
  (SegmentConfig x1_a2AJx
                 x2_a2AJy
                 x3_a2AJz
                 x4_a2AJA
                 x5_a2AJB
                 x6_a2AJC
                 x7_a2AJD
                 x8_a2AJF
                 x9_a2AJG
                 x10_a2AJH
                 x11_a2AJJ
                 x12_a2AJK
                 x13_a2AJL
                 x14_a2AJM
                 x15_a2AJN
                 x16_a2AJO)
  = fmap
      (\ y1_a2AJP
         -> SegmentConfig
              x1_a2AJx
              x2_a2AJy
              x3_a2AJz
              y1_a2AJP
              x5_a2AJB
              x6_a2AJC
              x7_a2AJD
              x8_a2AJF
              x9_a2AJG
              x10_a2AJH
              x11_a2AJJ
              x12_a2AJK
              x13_a2AJL
              x14_a2AJM
              x15_a2AJN
              x16_a2AJO)
      (f_a2AJw x4_a2AJA)
{-# INLINE segmentConfig_padded #-}
segmentConfig_raised ::
  forall t_a2Atd. Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd Bool)
segmentConfig_raised
  f_a2AJX
  (SegmentConfig x1_a2AJY
                 x2_a2AJZ
                 x3_a2AK0
                 x4_a2AK1
                 x5_a2AK2
                 x6_a2AK3
                 x7_a2AK4
                 x8_a2AK6
                 x9_a2AK8
                 x10_a2AK9
                 x11_a2AKa
                 x12_a2AKb
                 x13_a2AKc
                 x14_a2AKd
                 x15_a2AKe
                 x16_a2AKf)
  = fmap
      (\ y1_a2AKg
         -> SegmentConfig
              y1_a2AKg
              x2_a2AJZ
              x3_a2AK0
              x4_a2AK1
              x5_a2AK2
              x6_a2AK3
              x7_a2AK4
              x8_a2AK6
              x9_a2AK8
              x10_a2AK9
              x11_a2AKa
              x12_a2AKb
              x13_a2AKc
              x14_a2AKd
              x15_a2AKe
              x16_a2AKf)
      (f_a2AJX x1_a2AJY)
{-# INLINE segmentConfig_raised #-}
segmentConfig_size ::
  forall t_a2Atd.
  Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd (Maybe Size))
segmentConfig_size
  f_a2AKh
  (SegmentConfig x1_a2AKi
                 x2_a2AKj
                 x3_a2AKk
                 x4_a2AKl
                 x5_a2AKm
                 x6_a2AKn
                 x7_a2AKo
                 x8_a2AKt
                 x9_a2AKx
                 x10_a2AKy
                 x11_a2AKz
                 x12_a2AKA
                 x13_a2AKB
                 x14_a2AKC
                 x15_a2AKD
                 x16_a2AKE)
  = fmap
      (\ y1_a2AKI
         -> SegmentConfig
              x1_a2AKi
              x2_a2AKj
              x3_a2AKk
              x4_a2AKl
              x5_a2AKm
              x6_a2AKn
              x7_a2AKo
              x8_a2AKt
              x9_a2AKx
              x10_a2AKy
              x11_a2AKz
              x12_a2AKA
              x13_a2AKB
              x14_a2AKC
              y1_a2AKI
              x16_a2AKE)
      (f_a2AKh x15_a2AKD)
{-# INLINE segmentConfig_size #-}
segmentConfig_stacked ::
  forall t_a2Atd.
  Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd (Maybe Stacked))
segmentConfig_stacked
  f_a2AKM
  (SegmentConfig x1_a2AKN
                 x2_a2AKP
                 x3_a2AKQ
                 x4_a2AKR
                 x5_a2AKS
                 x6_a2AKT
                 x7_a2AKW
                 x8_a2AL2
                 x9_a2AL3
                 x10_a2AL4
                 x11_a2AL5
                 x12_a2AL6
                 x13_a2AL7
                 x14_a2AL8
                 x15_a2AL9
                 x16_a2ALa)
  = fmap
      (\ y1_a2ALc
         -> SegmentConfig
              x1_a2AKN
              x2_a2AKP
              x3_a2AKQ
              x4_a2AKR
              x5_a2AKS
              x6_a2AKT
              x7_a2AKW
              x8_a2AL2
              y1_a2ALc
              x10_a2AL4
              x11_a2AL5
              x12_a2AL6
              x13_a2AL7
              x14_a2AL8
              x15_a2AL9
              x16_a2ALa)
      (f_a2AKM x9_a2AL3)
{-# INLINE segmentConfig_stacked #-}
segmentConfig_vertical ::
  forall t_a2Atd. Lens' (SegmentConfig t_a2Atd) (Active t_a2Atd Bool)
segmentConfig_vertical
  f_a2ALs
  (SegmentConfig x1_a2ALt
                 x2_a2ALu
                 x3_a2ALv
                 x4_a2ALw
                 x5_a2ALx
                 x6_a2ALy
                 x7_a2ALz
                 x8_a2ALA
                 x9_a2ALB
                 x10_a2ALC
                 x11_a2ALD
                 x12_a2ALE
                 x13_a2ALF
                 x14_a2ALJ
                 x15_a2ALK
                 x16_a2ALL)
  = fmap
      (\ y1_a2ALM
         -> SegmentConfig
              x1_a2ALt
              y1_a2ALM
              x3_a2ALv
              x4_a2ALw
              x5_a2ALx
              x6_a2ALy
              x7_a2ALz
              x8_a2ALA
              x9_a2ALB
              x10_a2ALC
              x11_a2ALD
              x12_a2ALE
              x13_a2ALF
              x14_a2ALJ
              x15_a2ALK
              x16_a2ALL)
      (f_a2ALs x2_a2ALu)
{-# INLINE segmentConfig_vertical #-}
-- src/Reflex/Dom/SemanticUI/Segment.hs:169:1-66: Splicing declarations
segmentsConfig_compact ::
  forall t_a2B5a.
  Lens' (SegmentsConfig t_a2B5a) (Active t_a2B5a Bool)
segmentsConfig_compact
  f_a2Cpf
  (SegmentsConfig x1_a2Cpg x2_a2Cph x3_a2Cpi x4_a2Cpj x5_a2Cpk)
  = fmap
      (\ y1_a2Cpl
         -> SegmentsConfig x1_a2Cpg x2_a2Cph x3_a2Cpi y1_a2Cpl x5_a2Cpk)
      (f_a2Cpf x4_a2Cpj)
{-# INLINE segmentsConfig_compact #-}
segmentsConfig_elConfig ::
  forall t_a2B5a.
  Lens' (SegmentsConfig t_a2B5a) (ActiveElConfig t_a2B5a)
segmentsConfig_elConfig
  f_a2Cpm
  (SegmentsConfig x1_a2Cpn x2_a2Cpo x3_a2Cpp x4_a2Cpq x5_a2Cpr)
  = fmap
      (\ y1_a2Cps
         -> SegmentsConfig x1_a2Cpn x2_a2Cpo x3_a2Cpp x4_a2Cpq y1_a2Cps)
      (f_a2Cpm x5_a2Cpr)
{-# INLINE segmentsConfig_elConfig #-}
segmentsConfig_horizontal ::
  forall t_a2B5a.
  Lens' (SegmentsConfig t_a2B5a) (Active t_a2B5a Bool)
segmentsConfig_horizontal
  f_a2Cpt
  (SegmentsConfig x1_a2Cpu x2_a2Cpv x3_a2Cpw x4_a2Cpx x5_a2Cpy)
  = fmap
      (\ y1_a2Cpz
         -> SegmentsConfig y1_a2Cpz x2_a2Cpv x3_a2Cpw x4_a2Cpx x5_a2Cpy)
      (f_a2Cpt x1_a2Cpu)
{-# INLINE segmentsConfig_horizontal #-}
segmentsConfig_raised ::
  forall t_a2B5a.
  Lens' (SegmentsConfig t_a2B5a) (Active t_a2B5a Bool)
segmentsConfig_raised
  f_a2CpO
  (SegmentsConfig x1_a2CpP x2_a2CpQ x3_a2CpR x4_a2CpS x5_a2CpT)
  = fmap
      (\ y1_a2CpU
         -> SegmentsConfig x1_a2CpP y1_a2CpU x3_a2CpR x4_a2CpS x5_a2CpT)
      (f_a2CpO x2_a2CpQ)
{-# INLINE segmentsConfig_raised #-}
segmentsConfig_stacked ::
  forall t_a2B5a.
  Lens' (SegmentsConfig t_a2B5a) (Active t_a2B5a (Maybe Stacked))
segmentsConfig_stacked
  f_a2CpY
  (SegmentsConfig x1_a2CpZ x2_a2Cq0 x3_a2Cq1 x4_a2Cq2 x5_a2Cq3)
  = fmap
      (\ y1_a2Cq4
         -> SegmentsConfig x1_a2CpZ x2_a2Cq0 y1_a2Cq4 x4_a2Cq2 x5_a2Cq3)
      (f_a2CpY x3_a2Cq1)
{-# INLINE segmentsConfig_stacked #-}
