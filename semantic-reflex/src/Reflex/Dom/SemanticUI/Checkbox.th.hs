-- src/Reflex/Dom/SemanticUI/Checkbox.hs:71:1-66: Splicing declarations
checkboxConfig_disabled ::
  forall t_a1CjM.
  Lens' (CheckboxConfig t_a1CjM) (Active t_a1CjM Bool)
checkboxConfig_disabled
  f_a1CGK
  (CheckboxConfig x1_a1CGL
                  x2_a1CGM
                  x3_a1CGN
                  x4_a1CGO
                  x5_a1CGP
                  x6_a1CGQ)
  = fmap
      (\ y1_a1CGR
         -> CheckboxConfig
              x1_a1CGL x2_a1CGM x3_a1CGN x4_a1CGO y1_a1CGR x6_a1CGQ)
      (f_a1CGK x5_a1CGP)
{-# INLINE checkboxConfig_disabled #-}
checkboxConfig_elConfig ::
  forall t_a1CjM.
  Lens' (CheckboxConfig t_a1CjM) (ActiveElConfig t_a1CjM)
checkboxConfig_elConfig
  f_a1CGS
  (CheckboxConfig x1_a1CGT
                  x2_a1CGU
                  x3_a1CGV
                  x4_a1CGW
                  x5_a1CGX
                  x6_a1CGY)
  = fmap
      (\ y1_a1CGZ
         -> CheckboxConfig
              x1_a1CGT x2_a1CGU x3_a1CGV x4_a1CGW x5_a1CGX y1_a1CGZ)
      (f_a1CGS x6_a1CGY)
{-# INLINE checkboxConfig_elConfig #-}
checkboxConfig_fitted ::
  forall t_a1CjM.
  Lens' (CheckboxConfig t_a1CjM) (Active t_a1CjM Bool)
checkboxConfig_fitted
  f_a1CH0
  (CheckboxConfig x1_a1CH1
                  x2_a1CH2
                  x3_a1CH3
                  x4_a1CH4
                  x5_a1CH5
                  x6_a1CH6)
  = fmap
      (\ y1_a1CH7
         -> CheckboxConfig
              x1_a1CH1 x2_a1CH2 x3_a1CH3 y1_a1CH7 x5_a1CH5 x6_a1CH6)
      (f_a1CH0 x4_a1CH4)
{-# INLINE checkboxConfig_fitted #-}
checkboxConfig_setIndeterminate ::
  forall t_a1CjM.
  Lens' (CheckboxConfig t_a1CjM) (SetValue t_a1CjM Bool)
checkboxConfig_setIndeterminate
  f_a1CH8
  (CheckboxConfig x1_a1CH9
                  x2_a1CHa
                  x3_a1CHb
                  x4_a1CHc
                  x5_a1CHd
                  x6_a1CHe)
  = fmap
      (\ y1_a1CHf
         -> CheckboxConfig
              x1_a1CH9 y1_a1CHf x3_a1CHb x4_a1CHc x5_a1CHd x6_a1CHe)
      (f_a1CH8 x2_a1CHa)
{-# INLINE checkboxConfig_setIndeterminate #-}
checkboxConfig_setValue ::
  forall t_a1CjM.
  Lens' (CheckboxConfig t_a1CjM) (SetValue t_a1CjM Bool)
checkboxConfig_setValue
  f_a1CHg
  (CheckboxConfig x1_a1CHh
                  x2_a1CHi
                  x3_a1CHj
                  x4_a1CHk
                  x5_a1CHl
                  x6_a1CHm)
  = fmap
      (\ y1_a1CHn
         -> CheckboxConfig
              y1_a1CHn x2_a1CHi x3_a1CHj x4_a1CHk x5_a1CHl x6_a1CHm)
      (f_a1CHg x1_a1CHh)
{-# INLINE checkboxConfig_setValue #-}
checkboxConfig_type ::
  forall t_a1CjM.
  Lens' (CheckboxConfig t_a1CjM) (Active t_a1CjM (Maybe CheckboxType))
checkboxConfig_type
  f_a1CHo
  (CheckboxConfig x1_a1CHp
                  x2_a1CHq
                  x3_a1CHr
                  x4_a1CHs
                  x5_a1CHt
                  x6_a1CHu)
  = fmap
      (\ y1_a1CHv
         -> CheckboxConfig
              x1_a1CHp x2_a1CHq y1_a1CHv x4_a1CHs x5_a1CHt x6_a1CHu)
      (f_a1CHo x3_a1CHr)
{-# INLINE checkboxConfig_type #-}
-- src/Reflex/Dom/SemanticUI/Checkbox.hs:116:1-60: Splicing declarations
checkbox_change ::
  forall t_a1CLK. Lens' (Checkbox t_a1CLK) (Event t_a1CLK Bool)
checkbox_change
  f_a1DI0
  (Checkbox x1_a1DI1 x2_a1DI2 x3_a1DI3 x4_a1DI4 x5_a1DI5 x6_a1DI6)
  = fmap
      (\ y1_a1DI7
         -> Checkbox x1_a1DI1 y1_a1DI7 x3_a1DI3 x4_a1DI4 x5_a1DI5 x6_a1DI6)
      (f_a1DI0 x2_a1DI2)
{-# INLINE checkbox_change #-}
checkbox_divElement ::
  forall t_a1CLK. Lens' (Checkbox t_a1CLK) (El t_a1CLK)
checkbox_divElement
  f_a1DIa
  (Checkbox x1_a1DIb x2_a1DIc x3_a1DId x4_a1DIe x5_a1DIf x6_a1DIg)
  = fmap
      (\ y1_a1DIh
         -> Checkbox x1_a1DIb x2_a1DIc x3_a1DId x4_a1DIe y1_a1DIh x6_a1DIg)
      (f_a1DIa x5_a1DIf)
{-# INLINE checkbox_divElement #-}
checkbox_hasFocus ::
  forall t_a1CLK. Lens' (Checkbox t_a1CLK) (Dynamic t_a1CLK Bool)
checkbox_hasFocus
  f_a1DIi
  (Checkbox x1_a1DIj x2_a1DIk x3_a1DIl x4_a1DIm x5_a1DIn x6_a1DIo)
  = fmap
      (\ y1_a1DIp
         -> Checkbox x1_a1DIj x2_a1DIk x3_a1DIl y1_a1DIp x5_a1DIn x6_a1DIo)
      (f_a1DIi x4_a1DIm)
{-# INLINE checkbox_hasFocus #-}
checkbox_indeterminate ::
  forall t_a1CLK. Lens' (Checkbox t_a1CLK) (Dynamic t_a1CLK Bool)
checkbox_indeterminate
  f_a1DIr
  (Checkbox x1_a1DIs x2_a1DIt x3_a1DIu x4_a1DIv x5_a1DIw x6_a1DIx)
  = fmap
      (\ y1_a1DIy
         -> Checkbox x1_a1DIs x2_a1DIt y1_a1DIy x4_a1DIv x5_a1DIw x6_a1DIx)
      (f_a1DIr x3_a1DIu)
{-# INLINE checkbox_indeterminate #-}
checkbox_inputElement ::
  forall t_a1CLK. Lens' (Checkbox t_a1CLK) (El t_a1CLK)
checkbox_inputElement
  f_a1DIz
  (Checkbox x1_a1DIA x2_a1DIB x3_a1DIC x4_a1DID x5_a1DIE x6_a1DIG)
  = fmap
      (\ y1_a1DIN
         -> Checkbox x1_a1DIA x2_a1DIB x3_a1DIC x4_a1DID x5_a1DIE y1_a1DIN)
      (f_a1DIz x6_a1DIG)
{-# INLINE checkbox_inputElement #-}
checkbox_value ::
  forall t_a1CLK. Lens' (Checkbox t_a1CLK) (Dynamic t_a1CLK Bool)
checkbox_value
  f_a1DIP
  (Checkbox x1_a1DIQ x2_a1DIR x3_a1DIS x4_a1DIT x5_a1DIU x6_a1DIV)
  = fmap
      (\ y1_a1DIW
         -> Checkbox y1_a1DIW x2_a1DIR x3_a1DIS x4_a1DIT x5_a1DIU x6_a1DIV)
      (f_a1DIP x1_a1DIQ)
{-# INLINE checkbox_value #-}
