-- src/Reflex/Dom/SemanticUI/Dropdown.hs:81:1-66: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''DropdownConfig
--   ======>
dropdownConfig_as ::
  forall t_a2PjZ.
  Control.Lens.Type.Lens' (DropdownConfig t_a2PjZ) (Dynamic t_a2PjZ (Maybe DropdownStyle))
dropdownConfig_as
  f_a2Pqc
  (DropdownConfig x1_a2Pqd
                  x2_a2Pqe
                  x3_a2Pqf
                  x4_a2Pqg
                  x5_a2Pqh
                  x6_a2Pqi
                  x7_a2Pqj
                  x8_a2Pqk
                  x9_a2Pql
                  x10_a2Pqm
                  x11_a2Pqn)
  = fmap
      (\ y1_a2Pqo
         -> DropdownConfig
              x1_a2Pqd
              x2_a2Pqe
              x3_a2Pqf
              x4_a2Pqg
              x5_a2Pqh
              x6_a2Pqi
              y1_a2Pqo
              x8_a2Pqk
              x9_a2Pql
              x10_a2Pqm
              x11_a2Pqn)
      (f_a2Pqc x7_a2Pqj)
{-# INLINE dropdownConfig_as #-}
dropdownConfig_closeOnClickSelection ::
  forall t_a2PjZ.
  Control.Lens.Type.Lens' (DropdownConfig t_a2PjZ) Bool
dropdownConfig_closeOnClickSelection
  f_a2Pqp
  (DropdownConfig x1_a2Pqq
                  x2_a2Pqr
                  x3_a2Pqs
                  x4_a2Pqt
                  x5_a2Pqu
                  x6_a2Pqv
                  x7_a2Pqw
                  x8_a2Pqx
                  x9_a2Pqy
                  x10_a2Pqz
                  x11_a2PqA)
  = fmap
      (\ y1_a2PqB
         -> DropdownConfig
              x1_a2Pqq
              x2_a2Pqr
              x3_a2Pqs
              x4_a2Pqt
              x5_a2Pqu
              x6_a2Pqv
              x7_a2Pqw
              x8_a2Pqx
              y1_a2PqB
              x10_a2Pqz
              x11_a2PqA)
      (f_a2Pqp x9_a2Pqy)
{-# INLINE dropdownConfig_closeOnClickSelection #-}
dropdownConfig_compact ::
  forall t_a2PjZ.
  Control.Lens.Type.Lens' (DropdownConfig t_a2PjZ) (Dynamic t_a2PjZ Bool)
dropdownConfig_compact
  f_a2PqC
  (DropdownConfig x1_a2PqD
                  x2_a2PqE
                  x3_a2PqF
                  x4_a2PqG
                  x5_a2PqH
                  x6_a2PqI
                  x7_a2PqJ
                  x8_a2PqK
                  x9_a2PqL
                  x10_a2PqM
                  x11_a2PqN)
  = fmap
      (\ y1_a2PqO
         -> DropdownConfig
              x1_a2PqD
              x2_a2PqE
              y1_a2PqO
              x4_a2PqG
              x5_a2PqH
              x6_a2PqI
              x7_a2PqJ
              x8_a2PqK
              x9_a2PqL
              x10_a2PqM
              x11_a2PqN)
      (f_a2PqC x3_a2PqF)
{-# INLINE dropdownConfig_compact #-}
dropdownConfig_elConfig ::
  forall t_a2PjZ.
  Control.Lens.Type.Lens' (DropdownConfig t_a2PjZ) (ActiveElConfig t_a2PjZ)
dropdownConfig_elConfig
  f_a2PqP
  (DropdownConfig x1_a2PqQ
                  x2_a2PqR
                  x3_a2PqS
                  x4_a2PqT
                  x5_a2PqU
                  x6_a2PqV
                  x7_a2PqW
                  x8_a2PqX
                  x9_a2PqY
                  x10_a2PqZ
                  x11_a2Pr0)
  = fmap
      (\ y1_a2Pr1
         -> DropdownConfig
              x1_a2PqQ
              x2_a2PqR
              x3_a2PqS
              x4_a2PqT
              x5_a2PqU
              x6_a2PqV
              x7_a2PqW
              x8_a2PqX
              x9_a2PqY
              x10_a2PqZ
              y1_a2Pr1)
      (f_a2PqP x11_a2Pr0)
{-# INLINE dropdownConfig_elConfig #-}
dropdownConfig_fluid ::
  forall t_a2PjZ.
  Control.Lens.Type.Lens' (DropdownConfig t_a2PjZ) (Dynamic t_a2PjZ Bool)
dropdownConfig_fluid
  f_a2Pr2
  (DropdownConfig x1_a2Pr3
                  x2_a2Pr4
                  x3_a2Pr5
                  x4_a2Pr6
                  x5_a2Pr7
                  x6_a2Pr8
                  x7_a2Pr9
                  x8_a2Pra
                  x9_a2Prb
                  x10_a2Prc
                  x11_a2Prd)
  = fmap
      (\ y1_a2Pre
         -> DropdownConfig
              x1_a2Pr3
              x2_a2Pr4
              x3_a2Pr5
              y1_a2Pre
              x5_a2Pr7
              x6_a2Pr8
              x7_a2Pr9
              x8_a2Pra
              x9_a2Prb
              x10_a2Prc
              x11_a2Prd)
      (f_a2Pr2 x4_a2Pr6)
{-# INLINE dropdownConfig_fluid #-}
dropdownConfig_inline ::
  forall t_a2PjZ.
  Control.Lens.Type.Lens' (DropdownConfig t_a2PjZ) (Dynamic t_a2PjZ Bool)
dropdownConfig_inline
  f_a2Prf
  (DropdownConfig x1_a2Prg
                  x2_a2Prh
                  x3_a2Pri
                  x4_a2Prj
                  x5_a2Prk
                  x6_a2Prl
                  x7_a2Prm
                  x8_a2Prn
                  x9_a2Pro
                  x10_a2Prp
                  x11_a2Prq)
  = fmap
      (\ y1_a2Prr
         -> DropdownConfig
              x1_a2Prg
              x2_a2Prh
              x3_a2Pri
              x4_a2Prj
              x5_a2Prk
              y1_a2Prr
              x7_a2Prm
              x8_a2Prn
              x9_a2Pro
              x10_a2Prp
              x11_a2Prq)
      (f_a2Prf x6_a2Prl)
{-# INLINE dropdownConfig_inline #-}
dropdownConfig_item ::
  forall t_a2PjZ.
  Control.Lens.Type.Lens' (DropdownConfig t_a2PjZ) (Dynamic t_a2PjZ Bool)
dropdownConfig_item
  f_a2Prs
  (DropdownConfig x1_a2Prt
                  x2_a2Pru
                  x3_a2Prv
                  x4_a2Prw
                  x5_a2Prx
                  x6_a2Pry
                  x7_a2Prz
                  x8_a2PrA
                  x9_a2PrB
                  x10_a2PrC
                  x11_a2PrD)
  = fmap
      (\ y1_a2PrE
         -> DropdownConfig
              x1_a2Prt
              x2_a2Pru
              x3_a2Prv
              x4_a2Prw
              y1_a2PrE
              x6_a2Pry
              x7_a2Prz
              x8_a2PrA
              x9_a2PrB
              x10_a2PrC
              x11_a2PrD)
      (f_a2Prs x5_a2Prx)
{-# INLINE dropdownConfig_item #-}
dropdownConfig_placeholder ::
  forall t_a2PjZ.
  Control.Lens.Type.Lens' (DropdownConfig t_a2PjZ) (Dynamic t_a2PjZ Text)
dropdownConfig_placeholder
  f_a2PrF
  (DropdownConfig x1_a2PrG
                  x2_a2PrH
                  x3_a2PrI
                  x4_a2PrJ
                  x5_a2PrK
                  x6_a2PrL
                  x7_a2PrM
                  x8_a2PrN
                  x9_a2PrO
                  x10_a2PrP
                  x11_a2PrQ)
  = fmap
      (\ y1_a2PrR
         -> DropdownConfig
              y1_a2PrR
              x2_a2PrH
              x3_a2PrI
              x4_a2PrJ
              x5_a2PrK
              x6_a2PrL
              x7_a2PrM
              x8_a2PrN
              x9_a2PrO
              x10_a2PrP
              x11_a2PrQ)
      (f_a2PrF x1_a2PrG)
{-# INLINE dropdownConfig_placeholder #-}
dropdownConfig_searchFunction ::
  forall t_a2PjZ.
  Control.Lens.Type.Lens' (DropdownConfig t_a2PjZ) (Text
                                                    -> Text -> Bool)
dropdownConfig_searchFunction
  f_a2PrS
  (DropdownConfig x1_a2PrT
                  x2_a2PrU
                  x3_a2PrV
                  x4_a2PrW
                  x5_a2PrX
                  x6_a2PrY
                  x7_a2PrZ
                  x8_a2Ps0
                  x9_a2Ps1
                  x10_a2Ps2
                  x11_a2Ps3)
  = fmap
      (\ y1_a2Ps4
         -> DropdownConfig
              x1_a2PrT
              x2_a2PrU
              x3_a2PrV
              x4_a2PrW
              x5_a2PrX
              x6_a2PrY
              x7_a2PrZ
              x8_a2Ps0
              x9_a2Ps1
              y1_a2Ps4
              x11_a2Ps3)
      (f_a2PrS x10_a2Ps2)
{-# INLINE dropdownConfig_searchFunction #-}
dropdownConfig_selection ::
  forall t_a2PjZ.
  Control.Lens.Type.Lens' (DropdownConfig t_a2PjZ) (Dynamic t_a2PjZ Bool)
dropdownConfig_selection
  f_a2Ps5
  (DropdownConfig x1_a2Ps6
                  x2_a2Ps7
                  x3_a2Ps8
                  x4_a2Ps9
                  x5_a2Psa
                  x6_a2Psb
                  x7_a2Psc
                  x8_a2Psd
                  x9_a2Pse
                  x10_a2Psf
                  x11_a2Psg)
  = fmap
      (\ y1_a2Psh
         -> DropdownConfig
              x1_a2Ps6
              y1_a2Psh
              x3_a2Ps8
              x4_a2Ps9
              x5_a2Psa
              x6_a2Psb
              x7_a2Psc
              x8_a2Psd
              x9_a2Pse
              x10_a2Psf
              x11_a2Psg)
      (f_a2Ps5 x2_a2Ps7)
{-# INLINE dropdownConfig_selection #-}
dropdownConfig_unselectable ::
  forall t_a2PjZ.
  Control.Lens.Type.Lens' (DropdownConfig t_a2PjZ) Bool
dropdownConfig_unselectable
  f_a2Psi
  (DropdownConfig x1_a2Psj
                  x2_a2Psk
                  x3_a2Psl
                  x4_a2Psm
                  x5_a2Psn
                  x6_a2Pso
                  x7_a2Psp
                  x8_a2Psq
                  x9_a2Psr
                  x10_a2Pss
                  x11_a2Pst)
  = fmap
      (\ y1_a2Psu
         -> DropdownConfig
              x1_a2Psj
              x2_a2Psk
              x3_a2Psl
              x4_a2Psm
              x5_a2Psn
              x6_a2Pso
              x7_a2Psp
              y1_a2Psu
              x9_a2Psr
              x10_a2Pss
              x11_a2Pst)
      (f_a2Psi x8_a2Psq)
{-# INLINE dropdownConfig_unselectable #-}
-- src/Reflex/Dom/SemanticUI/Dropdown.hs:137:1-60: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''Dropdown
--   ======>
dropdown_blur ::
  forall t_a2Psv a_a2Psw.
  Control.Lens.Type.Lens' (Dropdown t_a2Psv a_a2Psw) (Event t_a2Psv ())
dropdown_blur f_a2PD4 (Dropdown x1_a2PD5 x2_a2PD6 x3_a2PD7)
  = fmap
      (\ y1_a2PD8 -> Dropdown x1_a2PD5 y1_a2PD8 x3_a2PD7)
      (f_a2PD4 x2_a2PD6)
{-# INLINE dropdown_blur #-}
dropdown_element ::
  forall t_a2Psv a_a2Psw.
  Control.Lens.Type.Lens' (Dropdown t_a2Psv a_a2Psw) (El t_a2Psv)
dropdown_element f_a2PD9 (Dropdown x1_a2PDa x2_a2PDb x3_a2PDc)
  = fmap
      (\ y1_a2PDd -> Dropdown x1_a2PDa x2_a2PDb y1_a2PDd)
      (f_a2PD9 x3_a2PDc)
{-# INLINE dropdown_element #-}
dropdown_value ::
  forall t_a2Psv a_a2Psw.
  Control.Lens.Type.Lens' (Dropdown t_a2Psv a_a2Psw) (Dynamic t_a2Psv a_a2Psw)
dropdown_value f_a2PDe (Dropdown x1_a2PDf x2_a2PDg x3_a2PDh)
  = fmap
      (\ y1_a2PDi -> Dropdown y1_a2PDi x2_a2PDg x3_a2PDh)
      (f_a2PDe x1_a2PDf)
{-# INLINE dropdown_value #-}

