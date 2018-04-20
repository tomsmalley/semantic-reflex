-- src/Reflex/Dom/SemanticUI/Button.hs:93:1-65: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''ButtonsConfig
--   ======>
buttonsConfig_attached ::
  forall t_a1ZnF.
  Lens' (ButtonsConfig t_a1ZnF) (Active t_a1ZnF (Maybe VerticalAttached))
buttonsConfig_attached
  f_a1ZoO
  (ButtonsConfig x1_a1ZoP
                 x2_a1ZoQ
                 x3_a1ZoR
                 x4_a1ZoS
                 x5_a1ZoT
                 x6_a1ZoU
                 x7_a1ZoV
                 x8_a1ZoW
                 x9_a1ZoX
                 x10_a1ZoY
                 x11_a1ZoZ)
  = fmap
      (\ y1_a1Zp0
         -> ButtonsConfig
              x1_a1ZoP
              x2_a1ZoQ
              x3_a1ZoR
              x4_a1ZoS
              x5_a1ZoT
              x6_a1ZoU
              x7_a1ZoV
              y1_a1Zp0
              x9_a1ZoX
              x10_a1ZoY
              x11_a1ZoZ)
      (f_a1ZoO x8_a1ZoW)
{-# INLINE buttonsConfig_attached #-}
buttonsConfig_basic ::
  forall t_a1ZnF. Lens' (ButtonsConfig t_a1ZnF) (Active t_a1ZnF Bool)
buttonsConfig_basic
  f_a1Zp1
  (ButtonsConfig x1_a1Zp2
                 x2_a1Zp3
                 x3_a1Zp4
                 x4_a1Zp5
                 x5_a1Zp6
                 x6_a1Zp7
                 x7_a1Zp8
                 x8_a1Zp9
                 x9_a1Zpa
                 x10_a1Zpb
                 x11_a1Zpc)
  = fmap
      (\ y1_a1Zpd
         -> ButtonsConfig
              y1_a1Zpd
              x2_a1Zp3
              x3_a1Zp4
              x4_a1Zp5
              x5_a1Zp6
              x6_a1Zp7
              x7_a1Zp8
              x8_a1Zp9
              x9_a1Zpa
              x10_a1Zpb
              x11_a1Zpc)
      (f_a1Zp1 x1_a1Zp2)
{-# INLINE buttonsConfig_basic #-}
buttonsConfig_color ::
  forall t_a1ZnF.
  Lens' (ButtonsConfig t_a1ZnF) (Active t_a1ZnF (Maybe Color))
buttonsConfig_color
  f_a1Zpe
  (ButtonsConfig x1_a1Zpf
                 x2_a1Zpg
                 x3_a1Zph
                 x4_a1Zpi
                 x5_a1Zpj
                 x6_a1Zpk
                 x7_a1Zpl
                 x8_a1Zpm
                 x9_a1Zpn
                 x10_a1Zpo
                 x11_a1Zpp)
  = fmap
      (\ y1_a1Zpq
         -> ButtonsConfig
              x1_a1Zpf
              x2_a1Zpg
              x3_a1Zph
              x4_a1Zpi
              x5_a1Zpj
              y1_a1Zpq
              x7_a1Zpl
              x8_a1Zpm
              x9_a1Zpn
              x10_a1Zpo
              x11_a1Zpp)
      (f_a1Zpe x6_a1Zpk)
{-# INLINE buttonsConfig_color #-}
buttonsConfig_compact ::
  forall t_a1ZnF. Lens' (ButtonsConfig t_a1ZnF) (Active t_a1ZnF Bool)
buttonsConfig_compact
  f_a1Zpr
  (ButtonsConfig x1_a1Zps
                 x2_a1Zpt
                 x3_a1Zpu
                 x4_a1Zpv
                 x5_a1Zpw
                 x6_a1Zpx
                 x7_a1Zpy
                 x8_a1Zpz
                 x9_a1ZpA
                 x10_a1ZpB
                 x11_a1ZpC)
  = fmap
      (\ y1_a1ZpD
         -> ButtonsConfig
              x1_a1Zps
              x2_a1Zpt
              x3_a1Zpu
              x4_a1Zpv
              y1_a1ZpD
              x6_a1Zpx
              x7_a1Zpy
              x8_a1Zpz
              x9_a1ZpA
              x10_a1ZpB
              x11_a1ZpC)
      (f_a1Zpr x5_a1Zpw)
{-# INLINE buttonsConfig_compact #-}
buttonsConfig_elConfig ::
  forall t_a1ZnF.
  Lens' (ButtonsConfig t_a1ZnF) (ActiveElConfig t_a1ZnF)
buttonsConfig_elConfig
  f_a1ZpE
  (ButtonsConfig x1_a1ZpF
                 x2_a1ZpG
                 x3_a1ZpH
                 x4_a1ZpI
                 x5_a1ZpJ
                 x6_a1ZpK
                 x7_a1ZpL
                 x8_a1ZpM
                 x9_a1ZpN
                 x10_a1ZpO
                 x11_a1ZpP)
  = fmap
      (\ y1_a1ZpQ
         -> ButtonsConfig
              x1_a1ZpF
              x2_a1ZpG
              x3_a1ZpH
              x4_a1ZpI
              x5_a1ZpJ
              x6_a1ZpK
              x7_a1ZpL
              x8_a1ZpM
              x9_a1ZpN
              x10_a1ZpO
              y1_a1ZpQ)
      (f_a1ZpE x11_a1ZpP)
{-# INLINE buttonsConfig_elConfig #-}
buttonsConfig_floated ::
  forall t_a1ZnF.
  Lens' (ButtonsConfig t_a1ZnF) (Active t_a1ZnF (Maybe Floated))
buttonsConfig_floated
  f_a1ZpR
  (ButtonsConfig x1_a1ZpS
                 x2_a1ZpT
                 x3_a1ZpU
                 x4_a1ZpV
                 x5_a1ZpW
                 x6_a1ZpX
                 x7_a1ZpY
                 x8_a1ZpZ
                 x9_a1Zq0
                 x10_a1Zq1
                 x11_a1Zq2)
  = fmap
      (\ y1_a1Zq3
         -> ButtonsConfig
              x1_a1ZpS
              x2_a1ZpT
              x3_a1ZpU
              x4_a1ZpV
              x5_a1ZpW
              x6_a1ZpX
              x7_a1ZpY
              x8_a1ZpZ
              x9_a1Zq0
              y1_a1Zq3
              x11_a1Zq2)
      (f_a1ZpR x10_a1Zq1)
{-# INLINE buttonsConfig_floated #-}
buttonsConfig_icon ::
  forall t_a1ZnF. Lens' (ButtonsConfig t_a1ZnF) (Active t_a1ZnF Bool)
buttonsConfig_icon
  f_a1Zq4
  (ButtonsConfig x1_a1Zq5
                 x2_a1Zq6
                 x3_a1Zq7
                 x4_a1Zq8
                 x5_a1Zq9
                 x6_a1Zqa
                 x7_a1Zqb
                 x8_a1Zqc
                 x9_a1Zqd
                 x10_a1Zqe
                 x11_a1Zqf)
  = fmap
      (\ y1_a1Zqg
         -> ButtonsConfig
              x1_a1Zq5
              y1_a1Zqg
              x3_a1Zq7
              x4_a1Zq8
              x5_a1Zq9
              x6_a1Zqa
              x7_a1Zqb
              x8_a1Zqc
              x9_a1Zqd
              x10_a1Zqe
              x11_a1Zqf)
      (f_a1Zq4 x2_a1Zq6)
{-# INLINE buttonsConfig_icon #-}
buttonsConfig_labeledIcon ::
  forall t_a1ZnF. Lens' (ButtonsConfig t_a1ZnF) (Active t_a1ZnF Bool)
buttonsConfig_labeledIcon
  f_a1Zqh
  (ButtonsConfig x1_a1Zqi
                 x2_a1Zqj
                 x3_a1Zqk
                 x4_a1Zql
                 x5_a1Zqm
                 x6_a1Zqn
                 x7_a1Zqo
                 x8_a1Zqp
                 x9_a1Zqq
                 x10_a1Zqr
                 x11_a1Zqs)
  = fmap
      (\ y1_a1Zqt
         -> ButtonsConfig
              x1_a1Zqi
              x2_a1Zqj
              y1_a1Zqt
              x4_a1Zql
              x5_a1Zqm
              x6_a1Zqn
              x7_a1Zqo
              x8_a1Zqp
              x9_a1Zqq
              x10_a1Zqr
              x11_a1Zqs)
      (f_a1Zqh x3_a1Zqk)
{-# INLINE buttonsConfig_labeledIcon #-}
buttonsConfig_size ::
  forall t_a1ZnF.
  Lens' (ButtonsConfig t_a1ZnF) (Active t_a1ZnF (Maybe Size))
buttonsConfig_size
  f_a1Zqu
  (ButtonsConfig x1_a1Zqv
                 x2_a1Zqw
                 x3_a1Zqx
                 x4_a1Zqy
                 x5_a1Zqz
                 x6_a1ZqA
                 x7_a1ZqB
                 x8_a1ZqC
                 x9_a1ZqD
                 x10_a1ZqE
                 x11_a1ZqF)
  = fmap
      (\ y1_a1ZqG
         -> ButtonsConfig
              x1_a1Zqv
              x2_a1Zqw
              x3_a1Zqx
              x4_a1Zqy
              x5_a1Zqz
              x6_a1ZqA
              y1_a1ZqG
              x8_a1ZqC
              x9_a1ZqD
              x10_a1ZqE
              x11_a1ZqF)
      (f_a1Zqu x7_a1ZqB)
{-# INLINE buttonsConfig_size #-}
buttonsConfig_vertical ::
  forall t_a1ZnF. Lens' (ButtonsConfig t_a1ZnF) (Active t_a1ZnF Bool)
buttonsConfig_vertical
  f_a1ZqH
  (ButtonsConfig x1_a1ZqI
                 x2_a1ZqJ
                 x3_a1ZqK
                 x4_a1ZqL
                 x5_a1ZqM
                 x6_a1ZqN
                 x7_a1ZqO
                 x8_a1ZqP
                 x9_a1ZqQ
                 x10_a1ZqR
                 x11_a1ZqS)
  = fmap
      (\ y1_a1ZqT
         -> ButtonsConfig
              x1_a1ZqI
              x2_a1ZqJ
              x3_a1ZqK
              y1_a1ZqT
              x5_a1ZqM
              x6_a1ZqN
              x7_a1ZqO
              x8_a1ZqP
              x9_a1ZqQ
              x10_a1ZqR
              x11_a1ZqS)
      (f_a1ZqH x4_a1ZqL)
{-# INLINE buttonsConfig_vertical #-}
buttonsConfig_width ::
  forall t_a1ZnF.
  Lens' (ButtonsConfig t_a1ZnF) (Active t_a1ZnF (Maybe Width))
buttonsConfig_width
  f_a1ZqU
  (ButtonsConfig x1_a1ZqV
                 x2_a1ZqW
                 x3_a1ZqX
                 x4_a1ZqY
                 x5_a1ZqZ
                 x6_a1Zr0
                 x7_a1Zr1
                 x8_a1Zr2
                 x9_a1Zr3
                 x10_a1Zr4
                 x11_a1Zr5)
  = fmap
      (\ y1_a1Zr6
         -> ButtonsConfig
              x1_a1ZqV
              x2_a1ZqW
              x3_a1ZqX
              x4_a1ZqY
              x5_a1ZqZ
              x6_a1Zr0
              x7_a1Zr1
              x8_a1Zr2
              y1_a1Zr6
              x10_a1Zr4
              x11_a1Zr5)
      (f_a1ZqU x9_a1Zr3)
{-# INLINE buttonsConfig_width #-}
-- src/Reflex/Dom/SemanticUI/Button.hs:137:1-71: Splicing declarations
--     makeLensesWith
--       (lensRules & simpleLenses .~ True) ''LabeledButtonConfig
--   ======>
labeledButtonConfig_elConfig ::
  forall t_a1Zr7.
  Lens' (LabeledButtonConfig t_a1Zr7) (ActiveElConfig t_a1Zr7)
labeledButtonConfig_elConfig
  f_a1ZAb
  (LabeledButtonConfig x1_a1ZAc x2_a1ZAd)
  = fmap
      (\ y1_a1ZAe -> LabeledButtonConfig x1_a1ZAc y1_a1ZAe)
      (f_a1ZAb x2_a1ZAd)
{-# INLINE labeledButtonConfig_elConfig #-}
labeledButtonConfig_side ::
  forall t_a1Zr7.
  Lens' (LabeledButtonConfig t_a1Zr7) (Active t_a1Zr7 Labeled)
labeledButtonConfig_side
  f_a1ZAf
  (LabeledButtonConfig x1_a1ZAg x2_a1ZAh)
  = fmap
      (\ y1_a1ZAi -> LabeledButtonConfig y1_a1ZAi x2_a1ZAh)
      (f_a1ZAf x1_a1ZAg)
{-# INLINE labeledButtonConfig_side #-}
-- src/Reflex/Dom/SemanticUI/Button.hs:168:1-66: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''AnimatedButton
--   ======>
animatedButton_hiddenContent ::
  forall t_a1ZAj m_a1ZAk.
  Lens' (AnimatedButton t_a1ZAj m_a1ZAk) (m_a1ZAk ())
animatedButton_hiddenContent
  f_a1ZIL
  (AnimatedButton x1_a1ZIM x2_a1ZIN)
  = fmap
      (\ y1_a1ZIO -> AnimatedButton x1_a1ZIM y1_a1ZIO) (f_a1ZIL x2_a1ZIN)
{-# INLINE animatedButton_hiddenContent #-}
animatedButton_type ::
  forall t_a1ZAj m_a1ZAk.
  Lens' (AnimatedButton t_a1ZAj m_a1ZAk) (Active t_a1ZAj AnimatedButtonType)
animatedButton_type f_a1ZIP (AnimatedButton x1_a1ZIQ x2_a1ZIR)
  = fmap
      (\ y1_a1ZIS -> AnimatedButton y1_a1ZIS x2_a1ZIR) (f_a1ZIP x1_a1ZIQ)
{-# INLINE animatedButton_type #-}
-- src/Reflex/Dom/SemanticUI/Button.hs:216:1-64: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''ButtonConfig
--   ======>
buttonConfig_animated ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Maybe (AnimatedButton t_a1ZIT m_a1ZIU))
buttonConfig_animated
  f_a1ZMI
  (ButtonConfig x1_a1ZMJ
                x2_a1ZMK
                x3_a1ZML
                x4_a1ZMM
                x5_a1ZMN
                x6_a1ZMO
                x7_a1ZMP
                x8_a1ZMQ
                x9_a1ZMR
                x10_a1ZMS
                x11_a1ZMT
                x12_a1ZMU
                x13_a1ZMV
                x14_a1ZMW
                x15_a1ZMX
                x16_a1ZMY
                x17_a1ZMZ
                x18_a1ZN0
                x19_a1ZN1)
  = fmap
      (\ y1_a1ZN2
         -> ButtonConfig
              x1_a1ZMJ
              x2_a1ZMK
              x3_a1ZML
              x4_a1ZMM
              x5_a1ZMN
              x6_a1ZMO
              x7_a1ZMP
              x8_a1ZMQ
              x9_a1ZMR
              x10_a1ZMS
              x11_a1ZMT
              x12_a1ZMU
              x13_a1ZMV
              x14_a1ZMW
              x15_a1ZMX
              x16_a1ZMY
              y1_a1ZN2
              x18_a1ZN0
              x19_a1ZN1)
      (f_a1ZMI x17_a1ZMZ)
{-# INLINE buttonConfig_animated #-}
buttonConfig_attached ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT (Maybe ExclusiveAttached))
buttonConfig_attached
  f_a1ZN3
  (ButtonConfig x1_a1ZN4
                x2_a1ZN5
                x3_a1ZN6
                x4_a1ZN7
                x5_a1ZN8
                x6_a1ZN9
                x7_a1ZNa
                x8_a1ZNb
                x9_a1ZNc
                x10_a1ZNd
                x11_a1ZNe
                x12_a1ZNf
                x13_a1ZNg
                x14_a1ZNh
                x15_a1ZNi
                x16_a1ZNj
                x17_a1ZNk
                x18_a1ZNl
                x19_a1ZNm)
  = fmap
      (\ y1_a1ZNn
         -> ButtonConfig
              x1_a1ZN4
              x2_a1ZN5
              x3_a1ZN6
              x4_a1ZN7
              x5_a1ZN8
              x6_a1ZN9
              x7_a1ZNa
              x8_a1ZNb
              x9_a1ZNc
              x10_a1ZNd
              x11_a1ZNe
              x12_a1ZNf
              x13_a1ZNg
              x14_a1ZNh
              x15_a1ZNi
              y1_a1ZNn
              x17_a1ZNk
              x18_a1ZNl
              x19_a1ZNm)
      (f_a1ZN3 x16_a1ZNj)
{-# INLINE buttonConfig_attached #-}
buttonConfig_basic ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT Bool)
buttonConfig_basic
  f_a1ZNo
  (ButtonConfig x1_a1ZNp
                x2_a1ZNq
                x3_a1ZNr
                x4_a1ZNs
                x5_a1ZNt
                x6_a1ZNu
                x7_a1ZNv
                x8_a1ZNw
                x9_a1ZNx
                x10_a1ZNy
                x11_a1ZNz
                x12_a1ZNA
                x13_a1ZNB
                x14_a1ZNC
                x15_a1ZND
                x16_a1ZNE
                x17_a1ZNF
                x18_a1ZNG
                x19_a1ZNH)
  = fmap
      (\ y1_a1ZNI
         -> ButtonConfig
              x1_a1ZNp
              x2_a1ZNq
              y1_a1ZNI
              x4_a1ZNs
              x5_a1ZNt
              x6_a1ZNu
              x7_a1ZNv
              x8_a1ZNw
              x9_a1ZNx
              x10_a1ZNy
              x11_a1ZNz
              x12_a1ZNA
              x13_a1ZNB
              x14_a1ZNC
              x15_a1ZND
              x16_a1ZNE
              x17_a1ZNF
              x18_a1ZNG
              x19_a1ZNH)
      (f_a1ZNo x3_a1ZNr)
{-# INLINE buttonConfig_basic #-}
buttonConfig_circular ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT Bool)
buttonConfig_circular
  f_a1ZNJ
  (ButtonConfig x1_a1ZNK
                x2_a1ZNL
                x3_a1ZNM
                x4_a1ZNN
                x5_a1ZNO
                x6_a1ZNP
                x7_a1ZNQ
                x8_a1ZNR
                x9_a1ZNS
                x10_a1ZNT
                x11_a1ZNU
                x12_a1ZNV
                x13_a1ZNW
                x14_a1ZNX
                x15_a1ZNY
                x16_a1ZNZ
                x17_a1ZO0
                x18_a1ZO1
                x19_a1ZO2)
  = fmap
      (\ y1_a1ZO3
         -> ButtonConfig
              x1_a1ZNK
              x2_a1ZNL
              x3_a1ZNM
              x4_a1ZNN
              x5_a1ZNO
              x6_a1ZNP
              x7_a1ZNQ
              y1_a1ZO3
              x9_a1ZNS
              x10_a1ZNT
              x11_a1ZNU
              x12_a1ZNV
              x13_a1ZNW
              x14_a1ZNX
              x15_a1ZNY
              x16_a1ZNZ
              x17_a1ZO0
              x18_a1ZO1
              x19_a1ZO2)
      (f_a1ZNJ x8_a1ZNR)
{-# INLINE buttonConfig_circular #-}
buttonConfig_color ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT (Maybe Color))
buttonConfig_color
  f_a1ZO4
  (ButtonConfig x1_a1ZO5
                x2_a1ZO6
                x3_a1ZO7
                x4_a1ZO8
                x5_a1ZO9
                x6_a1ZOa
                x7_a1ZOb
                x8_a1ZOc
                x9_a1ZOd
                x10_a1ZOe
                x11_a1ZOf
                x12_a1ZOg
                x13_a1ZOh
                x14_a1ZOi
                x15_a1ZOj
                x16_a1ZOk
                x17_a1ZOl
                x18_a1ZOm
                x19_a1ZOn)
  = fmap
      (\ y1_a1ZOo
         -> ButtonConfig
              x1_a1ZO5
              x2_a1ZO6
              x3_a1ZO7
              x4_a1ZO8
              x5_a1ZO9
              x6_a1ZOa
              x7_a1ZOb
              x8_a1ZOc
              y1_a1ZOo
              x10_a1ZOe
              x11_a1ZOf
              x12_a1ZOg
              x13_a1ZOh
              x14_a1ZOi
              x15_a1ZOj
              x16_a1ZOk
              x17_a1ZOl
              x18_a1ZOm
              x19_a1ZOn)
      (f_a1ZO4 x9_a1ZOd)
{-# INLINE buttonConfig_color #-}
buttonConfig_compact ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT Bool)
buttonConfig_compact
  f_a1ZOp
  (ButtonConfig x1_a1ZOq
                x2_a1ZOr
                x3_a1ZOs
                x4_a1ZOt
                x5_a1ZOu
                x6_a1ZOv
                x7_a1ZOw
                x8_a1ZOx
                x9_a1ZOy
                x10_a1ZOz
                x11_a1ZOA
                x12_a1ZOB
                x13_a1ZOC
                x14_a1ZOD
                x15_a1ZOE
                x16_a1ZOF
                x17_a1ZOG
                x18_a1ZOH
                x19_a1ZOI)
  = fmap
      (\ y1_a1ZOJ
         -> ButtonConfig
              x1_a1ZOq
              y1_a1ZOJ
              x3_a1ZOs
              x4_a1ZOt
              x5_a1ZOu
              x6_a1ZOv
              x7_a1ZOw
              x8_a1ZOx
              x9_a1ZOy
              x10_a1ZOz
              x11_a1ZOA
              x12_a1ZOB
              x13_a1ZOC
              x14_a1ZOD
              x15_a1ZOE
              x16_a1ZOF
              x17_a1ZOG
              x18_a1ZOH
              x19_a1ZOI)
      (f_a1ZOp x2_a1ZOr)
{-# INLINE buttonConfig_compact #-}
buttonConfig_disabled ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT Bool)
buttonConfig_disabled
  f_a1ZOK
  (ButtonConfig x1_a1ZOL
                x2_a1ZOM
                x3_a1ZON
                x4_a1ZOO
                x5_a1ZOP
                x6_a1ZOQ
                x7_a1ZOR
                x8_a1ZOS
                x9_a1ZOT
                x10_a1ZOU
                x11_a1ZOV
                x12_a1ZOW
                x13_a1ZOX
                x14_a1ZOY
                x15_a1ZOZ
                x16_a1ZP0
                x17_a1ZP1
                x18_a1ZP2
                x19_a1ZP3)
  = fmap
      (\ y1_a1ZP4
         -> ButtonConfig
              y1_a1ZP4
              x2_a1ZOM
              x3_a1ZON
              x4_a1ZOO
              x5_a1ZOP
              x6_a1ZOQ
              x7_a1ZOR
              x8_a1ZOS
              x9_a1ZOT
              x10_a1ZOU
              x11_a1ZOV
              x12_a1ZOW
              x13_a1ZOX
              x14_a1ZOY
              x15_a1ZOZ
              x16_a1ZP0
              x17_a1ZP1
              x18_a1ZP2
              x19_a1ZP3)
      (f_a1ZOK x1_a1ZOL)
{-# INLINE buttonConfig_disabled #-}
buttonConfig_elConfig ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (ActiveElConfig t_a1ZIT)
buttonConfig_elConfig
  f_a1ZP5
  (ButtonConfig x1_a1ZP6
                x2_a1ZP7
                x3_a1ZP8
                x4_a1ZP9
                x5_a1ZPa
                x6_a1ZPb
                x7_a1ZPc
                x8_a1ZPd
                x9_a1ZPe
                x10_a1ZPf
                x11_a1ZPg
                x12_a1ZPh
                x13_a1ZPi
                x14_a1ZPj
                x15_a1ZPk
                x16_a1ZPl
                x17_a1ZPm
                x18_a1ZPn
                x19_a1ZPo)
  = fmap
      (\ y1_a1ZPp
         -> ButtonConfig
              x1_a1ZP6
              x2_a1ZP7
              x3_a1ZP8
              x4_a1ZP9
              x5_a1ZPa
              x6_a1ZPb
              x7_a1ZPc
              x8_a1ZPd
              x9_a1ZPe
              x10_a1ZPf
              x11_a1ZPg
              x12_a1ZPh
              x13_a1ZPi
              x14_a1ZPj
              x15_a1ZPk
              x16_a1ZPl
              x17_a1ZPm
              x18_a1ZPn
              y1_a1ZPp)
      (f_a1ZP5 x19_a1ZPo)
{-# INLINE buttonConfig_elConfig #-}
buttonConfig_emphasis ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT (Maybe Emphasis))
buttonConfig_emphasis
  f_a1ZPq
  (ButtonConfig x1_a1ZPr
                x2_a1ZPs
                x3_a1ZPt
                x4_a1ZPu
                x5_a1ZPv
                x6_a1ZPw
                x7_a1ZPx
                x8_a1ZPy
                x9_a1ZPz
                x10_a1ZPA
                x11_a1ZPB
                x12_a1ZPC
                x13_a1ZPD
                x14_a1ZPE
                x15_a1ZPF
                x16_a1ZPG
                x17_a1ZPH
                x18_a1ZPI
                x19_a1ZPJ)
  = fmap
      (\ y1_a1ZPK
         -> ButtonConfig
              x1_a1ZPr
              x2_a1ZPs
              x3_a1ZPt
              x4_a1ZPu
              x5_a1ZPv
              x6_a1ZPw
              x7_a1ZPx
              x8_a1ZPy
              x9_a1ZPz
              x10_a1ZPA
              y1_a1ZPK
              x12_a1ZPC
              x13_a1ZPD
              x14_a1ZPE
              x15_a1ZPF
              x16_a1ZPG
              x17_a1ZPH
              x18_a1ZPI
              x19_a1ZPJ)
      (f_a1ZPq x11_a1ZPB)
{-# INLINE buttonConfig_emphasis #-}
buttonConfig_floated ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT (Maybe Floated))
buttonConfig_floated
  f_a1ZPL
  (ButtonConfig x1_a1ZPM
                x2_a1ZPN
                x3_a1ZPO
                x4_a1ZPP
                x5_a1ZPQ
                x6_a1ZPR
                x7_a1ZPS
                x8_a1ZPT
                x9_a1ZPU
                x10_a1ZPV
                x11_a1ZPW
                x12_a1ZPX
                x13_a1ZPY
                x14_a1ZPZ
                x15_a1ZQ0
                x16_a1ZQ1
                x17_a1ZQ2
                x18_a1ZQ3
                x19_a1ZQ4)
  = fmap
      (\ y1_a1ZQ5
         -> ButtonConfig
              x1_a1ZPM
              x2_a1ZPN
              x3_a1ZPO
              x4_a1ZPP
              x5_a1ZPQ
              x6_a1ZPR
              x7_a1ZPS
              x8_a1ZPT
              x9_a1ZPU
              x10_a1ZPV
              x11_a1ZPW
              x12_a1ZPX
              x13_a1ZPY
              y1_a1ZQ5
              x15_a1ZQ0
              x16_a1ZQ1
              x17_a1ZQ2
              x18_a1ZQ3
              x19_a1ZQ4)
      (f_a1ZPL x14_a1ZPZ)
{-# INLINE buttonConfig_floated #-}
buttonConfig_fluid ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT Bool)
buttonConfig_fluid
  f_a1ZQ6
  (ButtonConfig x1_a1ZQ7
                x2_a1ZQ8
                x3_a1ZQ9
                x4_a1ZQa
                x5_a1ZQb
                x6_a1ZQc
                x7_a1ZQd
                x8_a1ZQe
                x9_a1ZQf
                x10_a1ZQg
                x11_a1ZQh
                x12_a1ZQi
                x13_a1ZQj
                x14_a1ZQk
                x15_a1ZQl
                x16_a1ZQm
                x17_a1ZQn
                x18_a1ZQo
                x19_a1ZQp)
  = fmap
      (\ y1_a1ZQq
         -> ButtonConfig
              x1_a1ZQ7
              x2_a1ZQ8
              x3_a1ZQ9
              x4_a1ZQa
              x5_a1ZQb
              x6_a1ZQc
              y1_a1ZQq
              x8_a1ZQe
              x9_a1ZQf
              x10_a1ZQg
              x11_a1ZQh
              x12_a1ZQi
              x13_a1ZQj
              x14_a1ZQk
              x15_a1ZQl
              x16_a1ZQm
              x17_a1ZQn
              x18_a1ZQo
              x19_a1ZQp)
      (f_a1ZQ6 x7_a1ZQd)
{-# INLINE buttonConfig_fluid #-}
buttonConfig_icon ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT Bool)
buttonConfig_icon
  f_a1ZQr
  (ButtonConfig x1_a1ZQs
                x2_a1ZQt
                x3_a1ZQu
                x4_a1ZQv
                x5_a1ZQw
                x6_a1ZQx
                x7_a1ZQy
                x8_a1ZQz
                x9_a1ZQA
                x10_a1ZQB
                x11_a1ZQC
                x12_a1ZQD
                x13_a1ZQE
                x14_a1ZQF
                x15_a1ZQG
                x16_a1ZQH
                x17_a1ZQI
                x18_a1ZQJ
                x19_a1ZQK)
  = fmap
      (\ y1_a1ZQL
         -> ButtonConfig
              x1_a1ZQs
              x2_a1ZQt
              x3_a1ZQu
              y1_a1ZQL
              x5_a1ZQw
              x6_a1ZQx
              x7_a1ZQy
              x8_a1ZQz
              x9_a1ZQA
              x10_a1ZQB
              x11_a1ZQC
              x12_a1ZQD
              x13_a1ZQE
              x14_a1ZQF
              x15_a1ZQG
              x16_a1ZQH
              x17_a1ZQI
              x18_a1ZQJ
              x19_a1ZQK)
      (f_a1ZQr x4_a1ZQv)
{-# INLINE buttonConfig_icon #-}
buttonConfig_inverted ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT Bool)
buttonConfig_inverted
  f_a1ZQM
  (ButtonConfig x1_a1ZQN
                x2_a1ZQO
                x3_a1ZQP
                x4_a1ZQQ
                x5_a1ZQR
                x6_a1ZQS
                x7_a1ZQT
                x8_a1ZQU
                x9_a1ZQV
                x10_a1ZQW
                x11_a1ZQX
                x12_a1ZQY
                x13_a1ZQZ
                x14_a1ZR0
                x15_a1ZR1
                x16_a1ZR2
                x17_a1ZR3
                x18_a1ZR4
                x19_a1ZR5)
  = fmap
      (\ y1_a1ZR6
         -> ButtonConfig
              x1_a1ZQN
              x2_a1ZQO
              x3_a1ZQP
              x4_a1ZQQ
              y1_a1ZR6
              x6_a1ZQS
              x7_a1ZQT
              x8_a1ZQU
              x9_a1ZQV
              x10_a1ZQW
              x11_a1ZQX
              x12_a1ZQY
              x13_a1ZQZ
              x14_a1ZR0
              x15_a1ZR1
              x16_a1ZR2
              x17_a1ZR3
              x18_a1ZR4
              x19_a1ZR5)
      (f_a1ZQM x5_a1ZQR)
{-# INLINE buttonConfig_inverted #-}
buttonConfig_labeledIcon ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT (Maybe Labeled))
buttonConfig_labeledIcon
  f_a1ZR7
  (ButtonConfig x1_a1ZR8
                x2_a1ZR9
                x3_a1ZRa
                x4_a1ZRb
                x5_a1ZRc
                x6_a1ZRd
                x7_a1ZRe
                x8_a1ZRf
                x9_a1ZRg
                x10_a1ZRh
                x11_a1ZRi
                x12_a1ZRj
                x13_a1ZRk
                x14_a1ZRl
                x15_a1ZRm
                x16_a1ZRn
                x17_a1ZRo
                x18_a1ZRp
                x19_a1ZRq)
  = fmap
      (\ y1_a1ZRr
         -> ButtonConfig
              x1_a1ZR8
              x2_a1ZR9
              x3_a1ZRa
              x4_a1ZRb
              x5_a1ZRc
              x6_a1ZRd
              x7_a1ZRe
              x8_a1ZRf
              x9_a1ZRg
              x10_a1ZRh
              x11_a1ZRi
              x12_a1ZRj
              x13_a1ZRk
              x14_a1ZRl
              y1_a1ZRr
              x16_a1ZRn
              x17_a1ZRo
              x18_a1ZRp
              x19_a1ZRq)
      (f_a1ZR7 x15_a1ZRm)
{-# INLINE buttonConfig_labeledIcon #-}
buttonConfig_loading ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT Bool)
buttonConfig_loading
  f_a1ZRs
  (ButtonConfig x1_a1ZRt
                x2_a1ZRu
                x3_a1ZRv
                x4_a1ZRw
                x5_a1ZRx
                x6_a1ZRy
                x7_a1ZRz
                x8_a1ZRA
                x9_a1ZRB
                x10_a1ZRC
                x11_a1ZRD
                x12_a1ZRE
                x13_a1ZRF
                x14_a1ZRG
                x15_a1ZRH
                x16_a1ZRI
                x17_a1ZRJ
                x18_a1ZRK
                x19_a1ZRL)
  = fmap
      (\ y1_a1ZRM
         -> ButtonConfig
              x1_a1ZRt
              x2_a1ZRu
              x3_a1ZRv
              x4_a1ZRw
              x5_a1ZRx
              y1_a1ZRM
              x7_a1ZRz
              x8_a1ZRA
              x9_a1ZRB
              x10_a1ZRC
              x11_a1ZRD
              x12_a1ZRE
              x13_a1ZRF
              x14_a1ZRG
              x15_a1ZRH
              x16_a1ZRI
              x17_a1ZRJ
              x18_a1ZRK
              x19_a1ZRL)
      (f_a1ZRs x6_a1ZRy)
{-# INLINE buttonConfig_loading #-}
buttonConfig_positive ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT (Maybe Positive))
buttonConfig_positive
  f_a1ZRN
  (ButtonConfig x1_a1ZRO
                x2_a1ZRP
                x3_a1ZRQ
                x4_a1ZRR
                x5_a1ZRS
                x6_a1ZRT
                x7_a1ZRU
                x8_a1ZRV
                x9_a1ZRW
                x10_a1ZRX
                x11_a1ZRY
                x12_a1ZRZ
                x13_a1ZS0
                x14_a1ZS1
                x15_a1ZS2
                x16_a1ZS3
                x17_a1ZS4
                x18_a1ZS5
                x19_a1ZS6)
  = fmap
      (\ y1_a1ZS7
         -> ButtonConfig
              x1_a1ZRO
              x2_a1ZRP
              x3_a1ZRQ
              x4_a1ZRR
              x5_a1ZRS
              x6_a1ZRT
              x7_a1ZRU
              x8_a1ZRV
              x9_a1ZRW
              x10_a1ZRX
              x11_a1ZRY
              y1_a1ZS7
              x13_a1ZS0
              x14_a1ZS1
              x15_a1ZS2
              x16_a1ZS3
              x17_a1ZS4
              x18_a1ZS5
              x19_a1ZS6)
      (f_a1ZRN x12_a1ZRZ)
{-# INLINE buttonConfig_positive #-}
buttonConfig_size ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT (Maybe Size))
buttonConfig_size
  f_a1ZS8
  (ButtonConfig x1_a1ZS9
                x2_a1ZSa
                x3_a1ZSb
                x4_a1ZSc
                x5_a1ZSd
                x6_a1ZSe
                x7_a1ZSf
                x8_a1ZSg
                x9_a1ZSh
                x10_a1ZSi
                x11_a1ZSj
                x12_a1ZSk
                x13_a1ZSl
                x14_a1ZSm
                x15_a1ZSn
                x16_a1ZSo
                x17_a1ZSp
                x18_a1ZSq
                x19_a1ZSr)
  = fmap
      (\ y1_a1ZSs
         -> ButtonConfig
              x1_a1ZS9
              x2_a1ZSa
              x3_a1ZSb
              x4_a1ZSc
              x5_a1ZSd
              x6_a1ZSe
              x7_a1ZSf
              x8_a1ZSg
              x9_a1ZSh
              y1_a1ZSs
              x11_a1ZSj
              x12_a1ZSk
              x13_a1ZSl
              x14_a1ZSm
              x15_a1ZSn
              x16_a1ZSo
              x17_a1ZSp
              x18_a1ZSq
              x19_a1ZSr)
      (f_a1ZS8 x10_a1ZSi)
{-# INLINE buttonConfig_size #-}
buttonConfig_social ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) (Active t_a1ZIT (Maybe Social))
buttonConfig_social
  f_a1ZSt
  (ButtonConfig x1_a1ZSu
                x2_a1ZSv
                x3_a1ZSw
                x4_a1ZSx
                x5_a1ZSy
                x6_a1ZSz
                x7_a1ZSA
                x8_a1ZSB
                x9_a1ZSC
                x10_a1ZSD
                x11_a1ZSE
                x12_a1ZSF
                x13_a1ZSG
                x14_a1ZSH
                x15_a1ZSI
                x16_a1ZSJ
                x17_a1ZSK
                x18_a1ZSL
                x19_a1ZSM)
  = fmap
      (\ y1_a1ZSN
         -> ButtonConfig
              x1_a1ZSu
              x2_a1ZSv
              x3_a1ZSw
              x4_a1ZSx
              x5_a1ZSy
              x6_a1ZSz
              x7_a1ZSA
              x8_a1ZSB
              x9_a1ZSC
              x10_a1ZSD
              x11_a1ZSE
              x12_a1ZSF
              y1_a1ZSN
              x14_a1ZSH
              x15_a1ZSI
              x16_a1ZSJ
              x17_a1ZSK
              x18_a1ZSL
              x19_a1ZSM)
      (f_a1ZSt x13_a1ZSG)
{-# INLINE buttonConfig_social #-}
buttonConfig_type ::
  forall t_a1ZIT m_a1ZIU.
  Lens' (ButtonConfig t_a1ZIT m_a1ZIU) ButtonType
buttonConfig_type
  f_a1ZSO
  (ButtonConfig x1_a1ZSP
                x2_a1ZSQ
                x3_a1ZSR
                x4_a1ZSS
                x5_a1ZST
                x6_a1ZSU
                x7_a1ZSV
                x8_a1ZSW
                x9_a1ZSX
                x10_a1ZSY
                x11_a1ZSZ
                x12_a1ZT0
                x13_a1ZT1
                x14_a1ZT2
                x15_a1ZT3
                x16_a1ZT4
                x17_a1ZT5
                x18_a1ZT6
                x19_a1ZT7)
  = fmap
      (\ y1_a1ZT8
         -> ButtonConfig
              x1_a1ZSP
              x2_a1ZSQ
              x3_a1ZSR
              x4_a1ZSS
              x5_a1ZST
              x6_a1ZSU
              x7_a1ZSV
              x8_a1ZSW
              x9_a1ZSX
              x10_a1ZSY
              x11_a1ZSZ
              x12_a1ZT0
              x13_a1ZT1
              x14_a1ZT2
              x15_a1ZT3
              x16_a1ZT4
              x17_a1ZT5
              y1_a1ZT8
              x19_a1ZT7)
      (f_a1ZSO x18_a1ZT6)
{-# INLINE buttonConfig_type #-}

