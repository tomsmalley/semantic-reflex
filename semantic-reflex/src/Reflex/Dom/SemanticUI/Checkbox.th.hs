-- src/Reflex/Dom/SemanticUI/Checkbox.hs:71:1-66: Splicing declarations
checkboxConfig_disabled ::
  forall t_a2kJJ.
  Lens' (CheckboxConfig t_a2kJJ) (Active t_a2kJJ Bool)
checkboxConfig_disabled
  f_a2kY4
  (CheckboxConfig x1_a2kY6
                  x2_a2kY7
                  x3_a2kY8
                  x4_a2kYa
                  x5_a2kYb
                  x6_a2kYc)
  = fmap
      (\ y1_a2kYd
         -> CheckboxConfig
              x1_a2kY6 x2_a2kY7 x3_a2kY8 x4_a2kYa y1_a2kYd x6_a2kYc)
      (f_a2kY4 x5_a2kYb)
{-# INLINE checkboxConfig_disabled #-}
checkboxConfig_elConfig ::
  forall t_a2kJJ.
  Lens' (CheckboxConfig t_a2kJJ) (ActiveElConfig t_a2kJJ)
checkboxConfig_elConfig
  f_a2kYj
  (CheckboxConfig x1_a2kYk
                  x2_a2kYl
                  x3_a2kYn
                  x4_a2kYo
                  x5_a2kYp
                  x6_a2kYq)
  = fmap
      (\ y1_a2kYr
         -> CheckboxConfig
              x1_a2kYk x2_a2kYl x3_a2kYn x4_a2kYo x5_a2kYp y1_a2kYr)
      (f_a2kYj x6_a2kYq)
{-# INLINE checkboxConfig_elConfig #-}
checkboxConfig_fitted ::
  forall t_a2kJJ.
  Lens' (CheckboxConfig t_a2kJJ) (Active t_a2kJJ Bool)
checkboxConfig_fitted
  f_a2kYv
  (CheckboxConfig x1_a2kYw
                  x2_a2kYy
                  x3_a2kYz
                  x4_a2kYA
                  x5_a2kYB
                  x6_a2kYC)
  = fmap
      (\ y1_a2kYE
         -> CheckboxConfig
              x1_a2kYw x2_a2kYy x3_a2kYz y1_a2kYE x5_a2kYB x6_a2kYC)
      (f_a2kYv x4_a2kYA)
{-# INLINE checkboxConfig_fitted #-}
checkboxConfig_setIndeterminate ::
  forall t_a2kJJ.
  Lens' (CheckboxConfig t_a2kJJ) (SetValue t_a2kJJ Bool)
checkboxConfig_setIndeterminate
  f_a2kYH
  (CheckboxConfig x1_a2kYJ
                  x2_a2kYK
                  x3_a2kYL
                  x4_a2kYM
                  x5_a2kYN
                  x6_a2kYO)
  = fmap
      (\ y1_a2kYP
         -> CheckboxConfig
              x1_a2kYJ y1_a2kYP x3_a2kYL x4_a2kYM x5_a2kYN x6_a2kYO)
      (f_a2kYH x2_a2kYK)
{-# INLINE checkboxConfig_setIndeterminate #-}
checkboxConfig_setValue ::
  forall t_a2kJJ.
  Lens' (CheckboxConfig t_a2kJJ) (SetValue t_a2kJJ Bool)
checkboxConfig_setValue
  f_a2kYT
  (CheckboxConfig x1_a2kYU
                  x2_a2kYV
                  x3_a2kYX
                  x4_a2kYY
                  x5_a2kYZ
                  x6_a2kZ0)
  = fmap
      (\ y1_a2kZ1
         -> CheckboxConfig
              y1_a2kZ1 x2_a2kYV x3_a2kYX x4_a2kYY x5_a2kYZ x6_a2kZ0)
      (f_a2kYT x1_a2kYU)
{-# INLINE checkboxConfig_setValue #-}
checkboxConfig_type ::
  forall t_a2kJJ.
  Lens' (CheckboxConfig t_a2kJJ) (Active t_a2kJJ (Maybe CheckboxType))
checkboxConfig_type
  f_a2kZ5
  (CheckboxConfig x1_a2kZ6
                  x2_a2kZ7
                  x3_a2kZ8
                  x4_a2kZa
                  x5_a2kZb
                  x6_a2kZc)
  = fmap
      (\ y1_a2kZd
         -> CheckboxConfig
              x1_a2kZ6 x2_a2kZ7 y1_a2kZd x4_a2kZa x5_a2kZb x6_a2kZc)
      (f_a2kZ5 x3_a2kZ8)
{-# INLINE checkboxConfig_type #-}
-- src/Reflex/Dom/SemanticUI/Checkbox.hs:116:1-60: Splicing declarations
checkbox_change ::
  forall t_a2lpR. Lens' (Checkbox t_a2lpR) (Event t_a2lpR Bool)
checkbox_change
  f_a2lJn
  (Checkbox x1_a2lJp x2_a2lJq x3_a2lJr x4_a2lJt x5_a2lJu x6_a2lJv)
  = fmap
      (\ y1_a2lJw
         -> Checkbox x1_a2lJp y1_a2lJw x3_a2lJr x4_a2lJt x5_a2lJu x6_a2lJv)
      (f_a2lJn x2_a2lJq)
{-# INLINE checkbox_change #-}
checkbox_divElement ::
  forall t_a2lpR. Lens' (Checkbox t_a2lpR) (El t_a2lpR)
checkbox_divElement
  f_a2lJC
  (Checkbox x1_a2lJE x2_a2lJF x3_a2lJG x4_a2lJH x5_a2lJI x6_a2lJJ)
  = fmap
      (\ y1_a2lJL
         -> Checkbox x1_a2lJE x2_a2lJF x3_a2lJG x4_a2lJH y1_a2lJL x6_a2lJJ)
      (f_a2lJC x5_a2lJI)
{-# INLINE checkbox_divElement #-}
checkbox_hasFocus ::
  forall t_a2lpR. Lens' (Checkbox t_a2lpR) (Dynamic t_a2lpR Bool)
checkbox_hasFocus
  f_a2lJP
  (Checkbox x1_a2lJQ x2_a2lJS x3_a2lJT x4_a2lJU x5_a2lJV x6_a2lJW)
  = fmap
      (\ y1_a2lJX
         -> Checkbox x1_a2lJQ x2_a2lJS x3_a2lJT y1_a2lJX x5_a2lJV x6_a2lJW)
      (f_a2lJP x4_a2lJU)
{-# INLINE checkbox_hasFocus #-}
checkbox_indeterminate ::
  forall t_a2lpR. Lens' (Checkbox t_a2lpR) (Dynamic t_a2lpR Bool)
checkbox_indeterminate
  f_a2lK1
  (Checkbox x1_a2lK3 x2_a2lK4 x3_a2lK5 x4_a2lK6 x5_a2lK7 x6_a2lK8)
  = fmap
      (\ y1_a2lKa
         -> Checkbox x1_a2lK3 x2_a2lK4 y1_a2lKa x4_a2lK6 x5_a2lK7 x6_a2lK8)
      (f_a2lK1 x3_a2lK5)
{-# INLINE checkbox_indeterminate #-}
checkbox_inputElement ::
  forall t_a2lpR. Lens' (Checkbox t_a2lpR) (El t_a2lpR)
checkbox_inputElement
  f_a2lKe
  (Checkbox x1_a2lKf x2_a2lKg x3_a2lKh x4_a2lKi x5_a2lKj x6_a2lKl)
  = fmap
      (\ y1_a2lKm
         -> Checkbox x1_a2lKf x2_a2lKg x3_a2lKh x4_a2lKi x5_a2lKj y1_a2lKm)
      (f_a2lKe x6_a2lKl)
{-# INLINE checkbox_inputElement #-}
checkbox_value ::
  forall t_a2lpR. Lens' (Checkbox t_a2lpR) (Dynamic t_a2lpR Bool)
checkbox_value
  f_a2lKq
  (Checkbox x1_a2lKr x2_a2lKs x3_a2lKt x4_a2lKu x5_a2lKv x6_a2lKw)
  = fmap
      (\ y1_a2lKy
         -> Checkbox y1_a2lKy x2_a2lKs x3_a2lKt x4_a2lKu x5_a2lKv x6_a2lKw)
      (f_a2lKq x1_a2lKr)
{-# INLINE checkbox_value #-}
