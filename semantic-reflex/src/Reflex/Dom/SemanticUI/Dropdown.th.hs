-- src/Reflex/Dom/SemanticUI/Dropdown.hs:87:1-66: Splicing declarations
dropdownConfig_as ::
  forall t_a2E2K.
  Lens' (DropdownConfig t_a2E2K) (Dynamic t_a2E2K (Maybe DropdownStyle))
dropdownConfig_as
  f_a2EmA
  (DropdownConfig x1_a2EmB
                  x2_a2EmC
                  x3_a2EmD
                  x4_a2EmE
                  x5_a2EmF
                  x6_a2EmG
                  x7_a2EmH
                  x8_a2EmI
                  x9_a2EmJ
                  x10_a2EmK
                  x11_a2EmL
                  x12_a2EmM)
  = fmap
      (\ y1_a2EmN
         -> DropdownConfig
              x1_a2EmB
              x2_a2EmC
              x3_a2EmD
              x4_a2EmE
              x5_a2EmF
              x6_a2EmG
              y1_a2EmN
              x8_a2EmI
              x9_a2EmJ
              x10_a2EmK
              x11_a2EmL
              x12_a2EmM)
      (f_a2EmA x7_a2EmH)
{-# INLINE dropdownConfig_as #-}
dropdownConfig_closeOnClickSelection ::
  forall t_a2E2K. Lens' (DropdownConfig t_a2E2K) Bool
dropdownConfig_closeOnClickSelection
  f_a2EmO
  (DropdownConfig x1_a2EmP
                  x2_a2EmQ
                  x3_a2EmR
                  x4_a2EmS
                  x5_a2EmT
                  x6_a2EmU
                  x7_a2EmV
                  x8_a2EmW
                  x9_a2EmX
                  x10_a2EmY
                  x11_a2EmZ
                  x12_a2En0)
  = fmap
      (\ y1_a2En1
         -> DropdownConfig
              x1_a2EmP
              x2_a2EmQ
              x3_a2EmR
              x4_a2EmS
              x5_a2EmT
              x6_a2EmU
              x7_a2EmV
              x8_a2EmW
              y1_a2En1
              x10_a2EmY
              x11_a2EmZ
              x12_a2En0)
      (f_a2EmO x9_a2EmX)
{-# INLINE dropdownConfig_closeOnClickSelection #-}
dropdownConfig_compact ::
  forall t_a2E2K.
  Lens' (DropdownConfig t_a2E2K) (Dynamic t_a2E2K Bool)
dropdownConfig_compact
  f_a2En2
  (DropdownConfig x1_a2En3
                  x2_a2En4
                  x3_a2En5
                  x4_a2En6
                  x5_a2En7
                  x6_a2En8
                  x7_a2En9
                  x8_a2Ena
                  x9_a2Enb
                  x10_a2Enc
                  x11_a2End
                  x12_a2Ene)
  = fmap
      (\ y1_a2Enf
         -> DropdownConfig
              x1_a2En3
              x2_a2En4
              y1_a2Enf
              x4_a2En6
              x5_a2En7
              x6_a2En8
              x7_a2En9
              x8_a2Ena
              x9_a2Enb
              x10_a2Enc
              x11_a2End
              x12_a2Ene)
      (f_a2En2 x3_a2En5)
{-# INLINE dropdownConfig_compact #-}
dropdownConfig_elConfig ::
  forall t_a2E2K.
  Lens' (DropdownConfig t_a2E2K) (ActiveElConfig t_a2E2K)
dropdownConfig_elConfig
  f_a2Eng
  (DropdownConfig x1_a2Enh
                  x2_a2Eni
                  x3_a2Enj
                  x4_a2Enk
                  x5_a2Enl
                  x6_a2Enm
                  x7_a2Enn
                  x8_a2Eno
                  x9_a2Enp
                  x10_a2Enq
                  x11_a2Enr
                  x12_a2Ens)
  = fmap
      (\ y1_a2Ent
         -> DropdownConfig
              x1_a2Enh
              x2_a2Eni
              x3_a2Enj
              x4_a2Enk
              x5_a2Enl
              x6_a2Enm
              x7_a2Enn
              x8_a2Eno
              x9_a2Enp
              x10_a2Enq
              x11_a2Enr
              y1_a2Ent)
      (f_a2Eng x12_a2Ens)
{-# INLINE dropdownConfig_elConfig #-}
dropdownConfig_fluid ::
  forall t_a2E2K.
  Lens' (DropdownConfig t_a2E2K) (Dynamic t_a2E2K Bool)
dropdownConfig_fluid
  f_a2Enu
  (DropdownConfig x1_a2Env
                  x2_a2Enw
                  x3_a2Enx
                  x4_a2Eny
                  x5_a2Enz
                  x6_a2EnA
                  x7_a2EnB
                  x8_a2EnC
                  x9_a2EnD
                  x10_a2EnE
                  x11_a2EnF
                  x12_a2EnG)
  = fmap
      (\ y1_a2EnH
         -> DropdownConfig
              x1_a2Env
              x2_a2Enw
              x3_a2Enx
              y1_a2EnH
              x5_a2Enz
              x6_a2EnA
              x7_a2EnB
              x8_a2EnC
              x9_a2EnD
              x10_a2EnE
              x11_a2EnF
              x12_a2EnG)
      (f_a2Enu x4_a2Eny)
{-# INLINE dropdownConfig_fluid #-}
dropdownConfig_inline ::
  forall t_a2E2K.
  Lens' (DropdownConfig t_a2E2K) (Dynamic t_a2E2K Bool)
dropdownConfig_inline
  f_a2EnI
  (DropdownConfig x1_a2EnJ
                  x2_a2EnK
                  x3_a2EnL
                  x4_a2EnM
                  x5_a2EnN
                  x6_a2EnO
                  x7_a2EnP
                  x8_a2EnQ
                  x9_a2EnR
                  x10_a2EnS
                  x11_a2EnT
                  x12_a2EnU)
  = fmap
      (\ y1_a2EnV
         -> DropdownConfig
              x1_a2EnJ
              x2_a2EnK
              x3_a2EnL
              x4_a2EnM
              x5_a2EnN
              y1_a2EnV
              x7_a2EnP
              x8_a2EnQ
              x9_a2EnR
              x10_a2EnS
              x11_a2EnT
              x12_a2EnU)
      (f_a2EnI x6_a2EnO)
{-# INLINE dropdownConfig_inline #-}
dropdownConfig_item ::
  forall t_a2E2K.
  Lens' (DropdownConfig t_a2E2K) (Dynamic t_a2E2K Bool)
dropdownConfig_item
  f_a2EnW
  (DropdownConfig x1_a2EnX
                  x2_a2EnY
                  x3_a2EnZ
                  x4_a2Eo0
                  x5_a2Eo1
                  x6_a2Eo2
                  x7_a2Eo3
                  x8_a2Eo4
                  x9_a2Eo5
                  x10_a2Eo6
                  x11_a2Eo7
                  x12_a2Eo8)
  = fmap
      (\ y1_a2Eo9
         -> DropdownConfig
              x1_a2EnX
              x2_a2EnY
              x3_a2EnZ
              x4_a2Eo0
              y1_a2Eo9
              x6_a2Eo2
              x7_a2Eo3
              x8_a2Eo4
              x9_a2Eo5
              x10_a2Eo6
              x11_a2Eo7
              x12_a2Eo8)
      (f_a2EnW x5_a2Eo1)
{-# INLINE dropdownConfig_item #-}
dropdownConfig_placeholder ::
  forall t_a2E2K.
  Lens' (DropdownConfig t_a2E2K) (Dynamic t_a2E2K Text)
dropdownConfig_placeholder
  f_a2Eoa
  (DropdownConfig x1_a2Eob
                  x2_a2Eoc
                  x3_a2Eod
                  x4_a2Eoe
                  x5_a2Eof
                  x6_a2Eog
                  x7_a2Eoh
                  x8_a2Eoi
                  x9_a2Eoj
                  x10_a2Eok
                  x11_a2Eol
                  x12_a2Eom)
  = fmap
      (\ y1_a2Eon
         -> DropdownConfig
              y1_a2Eon
              x2_a2Eoc
              x3_a2Eod
              x4_a2Eoe
              x5_a2Eof
              x6_a2Eog
              x7_a2Eoh
              x8_a2Eoi
              x9_a2Eoj
              x10_a2Eok
              x11_a2Eol
              x12_a2Eom)
      (f_a2Eoa x1_a2Eob)
{-# INLINE dropdownConfig_placeholder #-}
dropdownConfig_searchFunction ::
  forall t_a2E2K.
  Lens' (DropdownConfig t_a2E2K) (Text -> Text -> Bool)
dropdownConfig_searchFunction
  f_a2Eoo
  (DropdownConfig x1_a2Eop
                  x2_a2Eoq
                  x3_a2Eor
                  x4_a2Eos
                  x5_a2Eot
                  x6_a2Eou
                  x7_a2Eov
                  x8_a2Eow
                  x9_a2Eox
                  x10_a2Eoy
                  x11_a2Eoz
                  x12_a2EoA)
  = fmap
      (\ y1_a2EoB
         -> DropdownConfig
              x1_a2Eop
              x2_a2Eoq
              x3_a2Eor
              x4_a2Eos
              x5_a2Eot
              x6_a2Eou
              x7_a2Eov
              x8_a2Eow
              x9_a2Eox
              x10_a2Eoy
              y1_a2EoB
              x12_a2EoA)
      (f_a2Eoo x11_a2Eoz)
{-# INLINE dropdownConfig_searchFunction #-}
dropdownConfig_searchValue ::
  forall t_a2E2K. Lens' (DropdownConfig t_a2E2K) (Event t_a2E2K Text)
dropdownConfig_searchValue
  f_a2EoC
  (DropdownConfig x1_a2EoD
                  x2_a2EoE
                  x3_a2EoF
                  x4_a2EoG
                  x5_a2EoH
                  x6_a2EoI
                  x7_a2EoJ
                  x8_a2EoK
                  x9_a2EoL
                  x10_a2EoM
                  x11_a2EoN
                  x12_a2EoO)
  = fmap
      (\ y1_a2EoP
         -> DropdownConfig
              x1_a2EoD
              x2_a2EoE
              x3_a2EoF
              x4_a2EoG
              x5_a2EoH
              x6_a2EoI
              x7_a2EoJ
              x8_a2EoK
              x9_a2EoL
              y1_a2EoP
              x11_a2EoN
              x12_a2EoO)
      (f_a2EoC x10_a2EoM)
{-# INLINE dropdownConfig_searchValue #-}
dropdownConfig_selection ::
  forall t_a2E2K.
  Lens' (DropdownConfig t_a2E2K) (Dynamic t_a2E2K Bool)
dropdownConfig_selection
  f_a2EoQ
  (DropdownConfig x1_a2EoR
                  x2_a2EoS
                  x3_a2EoT
                  x4_a2EoU
                  x5_a2EoV
                  x6_a2EoW
                  x7_a2EoX
                  x8_a2EoY
                  x9_a2EoZ
                  x10_a2Ep0
                  x11_a2Ep1
                  x12_a2Ep2)
  = fmap
      (\ y1_a2Ep3
         -> DropdownConfig
              x1_a2EoR
              y1_a2Ep3
              x3_a2EoT
              x4_a2EoU
              x5_a2EoV
              x6_a2EoW
              x7_a2EoX
              x8_a2EoY
              x9_a2EoZ
              x10_a2Ep0
              x11_a2Ep1
              x12_a2Ep2)
      (f_a2EoQ x2_a2EoS)
{-# INLINE dropdownConfig_selection #-}
dropdownConfig_unselectable ::
  forall t_a2E2K. Lens' (DropdownConfig t_a2E2K) Bool
dropdownConfig_unselectable
  f_a2Ep4
  (DropdownConfig x1_a2Ep5
                  x2_a2Ep6
                  x3_a2Ep7
                  x4_a2Ep8
                  x5_a2Ep9
                  x6_a2Epa
                  x7_a2Epb
                  x8_a2Epc
                  x9_a2Epd
                  x10_a2Epe
                  x11_a2Epf
                  x12_a2Epg)
  = fmap
      (\ y1_a2Eph
         -> DropdownConfig
              x1_a2Ep5
              x2_a2Ep6
              x3_a2Ep7
              x4_a2Ep8
              x5_a2Ep9
              x6_a2Epa
              x7_a2Epb
              y1_a2Eph
              x9_a2Epd
              x10_a2Epe
              x11_a2Epf
              x12_a2Epg)
      (f_a2Ep4 x8_a2Epc)
{-# INLINE dropdownConfig_unselectable #-}
-- src/Reflex/Dom/SemanticUI/Dropdown.hs:146:1-60: Splicing declarations
dropdown_blur ::
  forall t_a2Er7 a_a2Er8.
  Lens' (Dropdown t_a2Er7 a_a2Er8) (Event t_a2Er7 ())
dropdown_blur f_a2ES2 (Dropdown x1_a2ES3 x2_a2ES4 x3_a2ES5)
  = fmap
      (\ y1_a2ES6 -> Dropdown x1_a2ES3 y1_a2ES6 x3_a2ES5)
      (f_a2ES2 x2_a2ES4)
{-# INLINE dropdown_blur #-}
dropdown_element ::
  forall t_a2Er7 a_a2Er8.
  Lens' (Dropdown t_a2Er7 a_a2Er8) (El t_a2Er7)
dropdown_element f_a2ES9 (Dropdown x1_a2ESa x2_a2ESb x3_a2ESc)
  = fmap
      (\ y1_a2ESd -> Dropdown x1_a2ESa x2_a2ESb y1_a2ESd)
      (f_a2ES9 x3_a2ESc)
{-# INLINE dropdown_element #-}
dropdown_value ::
  forall t_a2Er7 a_a2Er8.
  Lens' (Dropdown t_a2Er7 a_a2Er8) (Dynamic t_a2Er7 a_a2Er8)
dropdown_value f_a2ESf (Dropdown x1_a2ESg x2_a2ESh x3_a2ESi)
  = fmap
      (\ y1_a2ESj -> Dropdown y1_a2ESj x2_a2ESh x3_a2ESi)
      (f_a2ESf x1_a2ESg)
{-# INLINE dropdown_value #-}
