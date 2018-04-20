-- src/Reflex/Dom/SemanticUI/Checkbox.hs:66:1-66: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''CheckboxConfig
--   ======>
checkboxConfig_disabled ::
  forall t_a29L1.
  Lens' (CheckboxConfig t_a29L1) (Active t_a29L1 Bool)
checkboxConfig_disabled
  f_a29MJ
  (CheckboxConfig x1_a29MK
                  x2_a29ML
                  x3_a29MM
                  x4_a29MN
                  x5_a29MO
                  x6_a29MP)
  = fmap
      (\ y1_a29MQ
         -> CheckboxConfig
              x1_a29MK x2_a29ML x3_a29MM x4_a29MN y1_a29MQ x6_a29MP)
      (f_a29MJ x5_a29MO)
{-# INLINE checkboxConfig_disabled #-}
checkboxConfig_elConfig ::
  forall t_a29L1.
  Lens' (CheckboxConfig t_a29L1) (ActiveElConfig t_a29L1)
checkboxConfig_elConfig
  f_a29MR
  (CheckboxConfig x1_a29MS
                  x2_a29MT
                  x3_a29MU
                  x4_a29MV
                  x5_a29MW
                  x6_a29MX)
  = fmap
      (\ y1_a29MY
         -> CheckboxConfig
              x1_a29MS x2_a29MT x3_a29MU x4_a29MV x5_a29MW y1_a29MY)
      (f_a29MR x6_a29MX)
{-# INLINE checkboxConfig_elConfig #-}
checkboxConfig_fitted ::
  forall t_a29L1.
  Lens' (CheckboxConfig t_a29L1) (Active t_a29L1 Bool)
checkboxConfig_fitted
  f_a29MZ
  (CheckboxConfig x1_a29N0
                  x2_a29N1
                  x3_a29N2
                  x4_a29N3
                  x5_a29N4
                  x6_a29N5)
  = fmap
      (\ y1_a29N6
         -> CheckboxConfig
              x1_a29N0 x2_a29N1 x3_a29N2 y1_a29N6 x5_a29N4 x6_a29N5)
      (f_a29MZ x4_a29N3)
{-# INLINE checkboxConfig_fitted #-}
checkboxConfig_setIndeterminate ::
  forall t_a29L1.
  Lens' (CheckboxConfig t_a29L1) (SetValue t_a29L1 Bool)
checkboxConfig_setIndeterminate
  f_a29N7
  (CheckboxConfig x1_a29N8
                  x2_a29N9
                  x3_a29Na
                  x4_a29Nb
                  x5_a29Nc
                  x6_a29Nd)
  = fmap
      (\ y1_a29Ne
         -> CheckboxConfig
              x1_a29N8 y1_a29Ne x3_a29Na x4_a29Nb x5_a29Nc x6_a29Nd)
      (f_a29N7 x2_a29N9)
{-# INLINE checkboxConfig_setIndeterminate #-}
checkboxConfig_setValue ::
  forall t_a29L1.
  Lens' (CheckboxConfig t_a29L1) (SetValue t_a29L1 Bool)
checkboxConfig_setValue
  f_a29Nf
  (CheckboxConfig x1_a29Ng
                  x2_a29Nh
                  x3_a29Ni
                  x4_a29Nj
                  x5_a29Nk
                  x6_a29Nl)
  = fmap
      (\ y1_a29Nm
         -> CheckboxConfig
              y1_a29Nm x2_a29Nh x3_a29Ni x4_a29Nj x5_a29Nk x6_a29Nl)
      (f_a29Nf x1_a29Ng)
{-# INLINE checkboxConfig_setValue #-}
checkboxConfig_type ::
  forall t_a29L1.
  Lens' (CheckboxConfig t_a29L1) (Active t_a29L1 (Maybe CheckboxType))
checkboxConfig_type
  f_a29Nn
  (CheckboxConfig x1_a29No
                  x2_a29Np
                  x3_a29Nq
                  x4_a29Nr
                  x5_a29Ns
                  x6_a29Nt)
  = fmap
      (\ y1_a29Nu
         -> CheckboxConfig
              x1_a29No x2_a29Np y1_a29Nu x4_a29Nr x5_a29Ns x6_a29Nt)
      (f_a29Nn x3_a29Nq)
{-# INLINE checkboxConfig_type #-}
-- src/Reflex/Dom/SemanticUI/Checkbox.hs:109:1-60: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''Checkbox
--   ======>
checkbox_change ::
  forall t_a29Nv. Lens' (Checkbox t_a29Nv) (Event t_a29Nv Bool)
checkbox_change
  f_a29SK
  (Checkbox x1_a29SL x2_a29SM x3_a29SN x4_a29SO x5_a29SP x6_a29SQ)
  = fmap
      (\ y1_a29SR
         -> Checkbox x1_a29SL y1_a29SR x3_a29SN x4_a29SO x5_a29SP x6_a29SQ)
      (f_a29SK x2_a29SM)
{-# INLINE checkbox_change #-}
checkbox_divElement ::
  forall t_a29Nv. Lens' (Checkbox t_a29Nv) (El t_a29Nv)
checkbox_divElement
  f_a29SS
  (Checkbox x1_a29ST x2_a29SU x3_a29SV x4_a29SW x5_a29SX x6_a29SY)
  = fmap
      (\ y1_a29SZ
         -> Checkbox x1_a29ST x2_a29SU x3_a29SV x4_a29SW y1_a29SZ x6_a29SY)
      (f_a29SS x5_a29SX)
{-# INLINE checkbox_divElement #-}
checkbox_hasFocus ::
  forall t_a29Nv. Lens' (Checkbox t_a29Nv) (Dynamic t_a29Nv Bool)
checkbox_hasFocus
  f_a29T0
  (Checkbox x1_a29T1 x2_a29T2 x3_a29T3 x4_a29T4 x5_a29T5 x6_a29T6)
  = fmap
      (\ y1_a29T7
         -> Checkbox x1_a29T1 x2_a29T2 x3_a29T3 y1_a29T7 x5_a29T5 x6_a29T6)
      (f_a29T0 x4_a29T4)
{-# INLINE checkbox_hasFocus #-}
checkbox_indeterminate ::
  forall t_a29Nv. Lens' (Checkbox t_a29Nv) (Dynamic t_a29Nv Bool)
checkbox_indeterminate
  f_a29T8
  (Checkbox x1_a29T9 x2_a29Ta x3_a29Tb x4_a29Tc x5_a29Td x6_a29Te)
  = fmap
      (\ y1_a29Tf
         -> Checkbox x1_a29T9 x2_a29Ta y1_a29Tf x4_a29Tc x5_a29Td x6_a29Te)
      (f_a29T8 x3_a29Tb)
{-# INLINE checkbox_indeterminate #-}
checkbox_inputElement ::
  forall t_a29Nv. Lens' (Checkbox t_a29Nv) (El t_a29Nv)
checkbox_inputElement
  f_a29Tg
  (Checkbox x1_a29Th x2_a29Ti x3_a29Tj x4_a29Tk x5_a29Tl x6_a29Tm)
  = fmap
      (\ y1_a29Tn
         -> Checkbox x1_a29Th x2_a29Ti x3_a29Tj x4_a29Tk x5_a29Tl y1_a29Tn)
      (f_a29Tg x6_a29Tm)
{-# INLINE checkbox_inputElement #-}
checkbox_value ::
  forall t_a29Nv. Lens' (Checkbox t_a29Nv) (Dynamic t_a29Nv Bool)
checkbox_value
  f_a29To
  (Checkbox x1_a29Tp x2_a29Tq x3_a29Tr x4_a29Ts x5_a29Tt x6_a29Tu)
  = fmap
      (\ y1_a29Tv
         -> Checkbox y1_a29Tv x2_a29Tq x3_a29Tr x4_a29Ts x5_a29Tt x6_a29Tu)
      (f_a29To x1_a29Tp)
{-# INLINE checkbox_value #-}

