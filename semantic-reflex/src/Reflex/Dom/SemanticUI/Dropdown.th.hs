-- src/Reflex/Dom/SemanticUI/Dropdown.hs:86:1-66: Splicing declarations
dropdownConfig_as ::
  forall t_a2PaO.
  Lens' (DropdownConfig t_a2PaO) (Dynamic t_a2PaO (Maybe DropdownStyle))
dropdownConfig_as
  f_a2Pqm
  (DropdownConfig x1_a2Pqn
                  x2_a2Pqo
                  x3_a2Pqp
                  x4_a2Pqq
                  x5_a2Pqr
                  x6_a2Pqs
                  x7_a2Pqt
                  x8_a2Pqu
                  x9_a2Pqv
                  x10_a2Pqw
                  x11_a2Pqx)
  = fmap
      (\ y1_a2Pqy
         -> DropdownConfig
              x1_a2Pqn
              x2_a2Pqo
              x3_a2Pqp
              x4_a2Pqq
              x5_a2Pqr
              x6_a2Pqs
              y1_a2Pqy
              x8_a2Pqu
              x9_a2Pqv
              x10_a2Pqw
              x11_a2Pqx)
      (f_a2Pqm x7_a2Pqt)
{-# INLINE dropdownConfig_as #-}
dropdownConfig_closeOnClickSelection ::
  forall t_a2PaO. Lens' (DropdownConfig t_a2PaO) Bool
dropdownConfig_closeOnClickSelection
  f_a2Pqz
  (DropdownConfig x1_a2PqA
                  x2_a2PqB
                  x3_a2PqC
                  x4_a2PqD
                  x5_a2PqE
                  x6_a2PqF
                  x7_a2PqG
                  x8_a2PqH
                  x9_a2PqI
                  x10_a2PqJ
                  x11_a2PqK)
  = fmap
      (\ y1_a2PqL
         -> DropdownConfig
              x1_a2PqA
              x2_a2PqB
              x3_a2PqC
              x4_a2PqD
              x5_a2PqE
              x6_a2PqF
              x7_a2PqG
              x8_a2PqH
              y1_a2PqL
              x10_a2PqJ
              x11_a2PqK)
      (f_a2Pqz x9_a2PqI)
{-# INLINE dropdownConfig_closeOnClickSelection #-}
dropdownConfig_compact ::
  forall t_a2PaO.
  Lens' (DropdownConfig t_a2PaO) (Dynamic t_a2PaO Bool)
dropdownConfig_compact
  f_a2PqM
  (DropdownConfig x1_a2PqN
                  x2_a2PqO
                  x3_a2PqP
                  x4_a2PqQ
                  x5_a2PqR
                  x6_a2PqS
                  x7_a2PqT
                  x8_a2PqU
                  x9_a2PqV
                  x10_a2PqW
                  x11_a2PqX)
  = fmap
      (\ y1_a2PqY
         -> DropdownConfig
              x1_a2PqN
              x2_a2PqO
              y1_a2PqY
              x4_a2PqQ
              x5_a2PqR
              x6_a2PqS
              x7_a2PqT
              x8_a2PqU
              x9_a2PqV
              x10_a2PqW
              x11_a2PqX)
      (f_a2PqM x3_a2PqP)
{-# INLINE dropdownConfig_compact #-}
dropdownConfig_elConfig ::
  forall t_a2PaO.
  Lens' (DropdownConfig t_a2PaO) (ActiveElConfig t_a2PaO)
dropdownConfig_elConfig
  f_a2PqZ
  (DropdownConfig x1_a2Pr0
                  x2_a2Pr1
                  x3_a2Pr2
                  x4_a2Pr3
                  x5_a2Pr4
                  x6_a2Pr5
                  x7_a2Pr6
                  x8_a2Pr7
                  x9_a2Pr8
                  x10_a2Pr9
                  x11_a2Pra)
  = fmap
      (\ y1_a2Prb
         -> DropdownConfig
              x1_a2Pr0
              x2_a2Pr1
              x3_a2Pr2
              x4_a2Pr3
              x5_a2Pr4
              x6_a2Pr5
              x7_a2Pr6
              x8_a2Pr7
              x9_a2Pr8
              x10_a2Pr9
              y1_a2Prb)
      (f_a2PqZ x11_a2Pra)
{-# INLINE dropdownConfig_elConfig #-}
dropdownConfig_fluid ::
  forall t_a2PaO.
  Lens' (DropdownConfig t_a2PaO) (Dynamic t_a2PaO Bool)
dropdownConfig_fluid
  f_a2Prc
  (DropdownConfig x1_a2Prd
                  x2_a2Pre
                  x3_a2Prf
                  x4_a2Prg
                  x5_a2Prh
                  x6_a2Pri
                  x7_a2Prj
                  x8_a2Prk
                  x9_a2Prl
                  x10_a2Prm
                  x11_a2Prn)
  = fmap
      (\ y1_a2Pro
         -> DropdownConfig
              x1_a2Prd
              x2_a2Pre
              x3_a2Prf
              y1_a2Pro
              x5_a2Prh
              x6_a2Pri
              x7_a2Prj
              x8_a2Prk
              x9_a2Prl
              x10_a2Prm
              x11_a2Prn)
      (f_a2Prc x4_a2Prg)
{-# INLINE dropdownConfig_fluid #-}
dropdownConfig_inline ::
  forall t_a2PaO.
  Lens' (DropdownConfig t_a2PaO) (Dynamic t_a2PaO Bool)
dropdownConfig_inline
  f_a2Prp
  (DropdownConfig x1_a2Prq
                  x2_a2Prr
                  x3_a2Prs
                  x4_a2Prt
                  x5_a2Pru
                  x6_a2Prv
                  x7_a2Prw
                  x8_a2Prx
                  x9_a2Pry
                  x10_a2Prz
                  x11_a2PrA)
  = fmap
      (\ y1_a2PrB
         -> DropdownConfig
              x1_a2Prq
              x2_a2Prr
              x3_a2Prs
              x4_a2Prt
              x5_a2Pru
              y1_a2PrB
              x7_a2Prw
              x8_a2Prx
              x9_a2Pry
              x10_a2Prz
              x11_a2PrA)
      (f_a2Prp x6_a2Prv)
{-# INLINE dropdownConfig_inline #-}
dropdownConfig_item ::
  forall t_a2PaO.
  Lens' (DropdownConfig t_a2PaO) (Dynamic t_a2PaO Bool)
dropdownConfig_item
  f_a2PrC
  (DropdownConfig x1_a2PrD
                  x2_a2PrE
                  x3_a2PrF
                  x4_a2PrG
                  x5_a2PrH
                  x6_a2PrI
                  x7_a2PrJ
                  x8_a2PrK
                  x9_a2PrL
                  x10_a2PrM
                  x11_a2PrN)
  = fmap
      (\ y1_a2PrO
         -> DropdownConfig
              x1_a2PrD
              x2_a2PrE
              x3_a2PrF
              x4_a2PrG
              y1_a2PrO
              x6_a2PrI
              x7_a2PrJ
              x8_a2PrK
              x9_a2PrL
              x10_a2PrM
              x11_a2PrN)
      (f_a2PrC x5_a2PrH)
{-# INLINE dropdownConfig_item #-}
dropdownConfig_placeholder ::
  forall t_a2PaO.
  Lens' (DropdownConfig t_a2PaO) (Dynamic t_a2PaO Text)
dropdownConfig_placeholder
  f_a2PrP
  (DropdownConfig x1_a2PrQ
                  x2_a2PrR
                  x3_a2PrS
                  x4_a2PrT
                  x5_a2PrU
                  x6_a2PrV
                  x7_a2PrW
                  x8_a2PrX
                  x9_a2PrY
                  x10_a2PrZ
                  x11_a2Ps0)
  = fmap
      (\ y1_a2Ps1
         -> DropdownConfig
              y1_a2Ps1
              x2_a2PrR
              x3_a2PrS
              x4_a2PrT
              x5_a2PrU
              x6_a2PrV
              x7_a2PrW
              x8_a2PrX
              x9_a2PrY
              x10_a2PrZ
              x11_a2Ps0)
      (f_a2PrP x1_a2PrQ)
{-# INLINE dropdownConfig_placeholder #-}
dropdownConfig_searchFunction ::
  forall t_a2PaO.
  Lens' (DropdownConfig t_a2PaO) (Text -> Text -> Bool)
dropdownConfig_searchFunction
  f_a2Ps2
  (DropdownConfig x1_a2Ps3
                  x2_a2Ps4
                  x3_a2Ps5
                  x4_a2Ps6
                  x5_a2Ps7
                  x6_a2Ps8
                  x7_a2Ps9
                  x8_a2Psa
                  x9_a2Psb
                  x10_a2Psc
                  x11_a2Psd)
  = fmap
      (\ y1_a2Pse
         -> DropdownConfig
              x1_a2Ps3
              x2_a2Ps4
              x3_a2Ps5
              x4_a2Ps6
              x5_a2Ps7
              x6_a2Ps8
              x7_a2Ps9
              x8_a2Psa
              x9_a2Psb
              y1_a2Pse
              x11_a2Psd)
      (f_a2Ps2 x10_a2Psc)
{-# INLINE dropdownConfig_searchFunction #-}
dropdownConfig_selection ::
  forall t_a2PaO.
  Lens' (DropdownConfig t_a2PaO) (Dynamic t_a2PaO Bool)
dropdownConfig_selection
  f_a2Psf
  (DropdownConfig x1_a2Psg
                  x2_a2Psh
                  x3_a2Psi
                  x4_a2Psj
                  x5_a2Psk
                  x6_a2Psl
                  x7_a2Psm
                  x8_a2Psn
                  x9_a2Pso
                  x10_a2Psp
                  x11_a2Psq)
  = fmap
      (\ y1_a2Psr
         -> DropdownConfig
              x1_a2Psg
              y1_a2Psr
              x3_a2Psi
              x4_a2Psj
              x5_a2Psk
              x6_a2Psl
              x7_a2Psm
              x8_a2Psn
              x9_a2Pso
              x10_a2Psp
              x11_a2Psq)
      (f_a2Psf x2_a2Psh)
{-# INLINE dropdownConfig_selection #-}
dropdownConfig_unselectable ::
  forall t_a2PaO. Lens' (DropdownConfig t_a2PaO) Bool
dropdownConfig_unselectable
  f_a2Pss
  (DropdownConfig x1_a2Pst
                  x2_a2Psu
                  x3_a2Psv
                  x4_a2Psw
                  x5_a2Psx
                  x6_a2Psy
                  x7_a2Psz
                  x8_a2PsA
                  x9_a2PsB
                  x10_a2PsC
                  x11_a2PsD)
  = fmap
      (\ y1_a2PsE
         -> DropdownConfig
              x1_a2Pst
              x2_a2Psu
              x3_a2Psv
              x4_a2Psw
              x5_a2Psx
              x6_a2Psy
              x7_a2Psz
              y1_a2PsE
              x9_a2PsB
              x10_a2PsC
              x11_a2PsD)
      (f_a2Pss x8_a2PsA)
{-# INLINE dropdownConfig_unselectable #-}
-- src/Reflex/Dom/SemanticUI/Dropdown.hs:144:1-60: Splicing declarations
dropdown_blur ::
  forall t_a2Pu8 a_a2Pu9.
  Lens' (Dropdown t_a2Pu8 a_a2Pu9) (Event t_a2Pu8 ())
dropdown_blur f_a2PQg (Dropdown x1_a2PQh x2_a2PQi x3_a2PQj)
  = fmap
      (\ y1_a2PQk -> Dropdown x1_a2PQh y1_a2PQk x3_a2PQj)
      (f_a2PQg x2_a2PQi)
{-# INLINE dropdown_blur #-}
dropdown_element ::
  forall t_a2Pu8 a_a2Pu9.
  Lens' (Dropdown t_a2Pu8 a_a2Pu9) (El t_a2Pu8)
dropdown_element f_a2PQm (Dropdown x1_a2PQn x2_a2PQo x3_a2PQp)
  = fmap
      (\ y1_a2PQq -> Dropdown x1_a2PQn x2_a2PQo y1_a2PQq)
      (f_a2PQm x3_a2PQp)
{-# INLINE dropdown_element #-}
dropdown_value ::
  forall t_a2Pu8 a_a2Pu9.
  Lens' (Dropdown t_a2Pu8 a_a2Pu9) (Dynamic t_a2Pu8 a_a2Pu9)
dropdown_value f_a2PQs (Dropdown x1_a2PQt x2_a2PQu x3_a2PQv)
  = fmap
      (\ y1_a2PQw -> Dropdown y1_a2PQw x2_a2PQu x3_a2PQv)
      (f_a2PQs x1_a2PQt)
{-# INLINE dropdown_value #-}
