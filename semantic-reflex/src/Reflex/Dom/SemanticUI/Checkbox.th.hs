-- src/Reflex/Dom/SemanticUI/Checkbox.hs:71:1-66: Splicing declarations
checkboxConfig_disabled ::
  forall t_a1V1E.
  Lens' (CheckboxConfig t_a1V1E) (Active t_a1V1E Bool)
checkboxConfig_disabled
  f_a1V8t
  (CheckboxConfig x1_a1V8u
                  x2_a1V8v
                  x3_a1V8w
                  x4_a1V8x
                  x5_a1V8y
                  x6_a1V8z)
  = fmap
      (\ y1_a1V8A
         -> CheckboxConfig
              x1_a1V8u x2_a1V8v x3_a1V8w x4_a1V8x y1_a1V8A x6_a1V8z)
      (f_a1V8t x5_a1V8y)
{-# INLINE checkboxConfig_disabled #-}
checkboxConfig_elConfig ::
  forall t_a1V1E.
  Lens' (CheckboxConfig t_a1V1E) (ActiveElConfig t_a1V1E)
checkboxConfig_elConfig
  f_a1V8B
  (CheckboxConfig x1_a1V8C
                  x2_a1V8D
                  x3_a1V8E
                  x4_a1V8F
                  x5_a1V8G
                  x6_a1V8H)
  = fmap
      (\ y1_a1V8I
         -> CheckboxConfig
              x1_a1V8C x2_a1V8D x3_a1V8E x4_a1V8F x5_a1V8G y1_a1V8I)
      (f_a1V8B x6_a1V8H)
{-# INLINE checkboxConfig_elConfig #-}
checkboxConfig_fitted ::
  forall t_a1V1E.
  Lens' (CheckboxConfig t_a1V1E) (Active t_a1V1E Bool)
checkboxConfig_fitted
  f_a1V8J
  (CheckboxConfig x1_a1V8K
                  x2_a1V8L
                  x3_a1V8M
                  x4_a1V8N
                  x5_a1V8O
                  x6_a1V8P)
  = fmap
      (\ y1_a1V8Q
         -> CheckboxConfig
              x1_a1V8K x2_a1V8L x3_a1V8M y1_a1V8Q x5_a1V8O x6_a1V8P)
      (f_a1V8J x4_a1V8N)
{-# INLINE checkboxConfig_fitted #-}
checkboxConfig_setIndeterminate ::
  forall t_a1V1E.
  Lens' (CheckboxConfig t_a1V1E) (SetValue t_a1V1E Bool)
checkboxConfig_setIndeterminate
  f_a1V8R
  (CheckboxConfig x1_a1V8S
                  x2_a1V8T
                  x3_a1V8U
                  x4_a1V8V
                  x5_a1V8W
                  x6_a1V8X)
  = fmap
      (\ y1_a1V8Y
         -> CheckboxConfig
              x1_a1V8S y1_a1V8Y x3_a1V8U x4_a1V8V x5_a1V8W x6_a1V8X)
      (f_a1V8R x2_a1V8T)
{-# INLINE checkboxConfig_setIndeterminate #-}
checkboxConfig_setValue ::
  forall t_a1V1E.
  Lens' (CheckboxConfig t_a1V1E) (SetValue t_a1V1E Bool)
checkboxConfig_setValue
  f_a1V8Z
  (CheckboxConfig x1_a1V90
                  x2_a1V91
                  x3_a1V92
                  x4_a1V93
                  x5_a1V94
                  x6_a1V95)
  = fmap
      (\ y1_a1V96
         -> CheckboxConfig
              y1_a1V96 x2_a1V91 x3_a1V92 x4_a1V93 x5_a1V94 x6_a1V95)
      (f_a1V8Z x1_a1V90)
{-# INLINE checkboxConfig_setValue #-}
checkboxConfig_type ::
  forall t_a1V1E.
  Lens' (CheckboxConfig t_a1V1E) (Active t_a1V1E (Maybe CheckboxType))
checkboxConfig_type
  f_a1V97
  (CheckboxConfig x1_a1V98
                  x2_a1V99
                  x3_a1V9a
                  x4_a1V9b
                  x5_a1V9c
                  x6_a1V9d)
  = fmap
      (\ y1_a1V9e
         -> CheckboxConfig
              x1_a1V98 x2_a1V99 y1_a1V9e x4_a1V9b x5_a1V9c x6_a1V9d)
      (f_a1V97 x3_a1V9a)
{-# INLINE checkboxConfig_type #-}
-- src/Reflex/Dom/SemanticUI/Checkbox.hs:116:1-60: Splicing declarations
checkbox_change ::
  forall t_a1Vez. Lens' (Checkbox t_a1Vez) (Event t_a1Vez Bool)
checkbox_change
  f_a1Vk2
  (Checkbox x1_a1Vk3 x2_a1Vk4 x3_a1Vk5 x4_a1Vk6 x5_a1Vk7 x6_a1Vk8)
  = fmap
      (\ y1_a1Vk9
         -> Checkbox x1_a1Vk3 y1_a1Vk9 x3_a1Vk5 x4_a1Vk6 x5_a1Vk7 x6_a1Vk8)
      (f_a1Vk2 x2_a1Vk4)
{-# INLINE checkbox_change #-}
checkbox_divElement ::
  forall t_a1Vez. Lens' (Checkbox t_a1Vez) (El t_a1Vez)
checkbox_divElement
  f_a1Vka
  (Checkbox x1_a1Vkb x2_a1Vkc x3_a1Vkd x4_a1Vke x5_a1Vkf x6_a1Vkg)
  = fmap
      (\ y1_a1Vkh
         -> Checkbox x1_a1Vkb x2_a1Vkc x3_a1Vkd x4_a1Vke y1_a1Vkh x6_a1Vkg)
      (f_a1Vka x5_a1Vkf)
{-# INLINE checkbox_divElement #-}
checkbox_hasFocus ::
  forall t_a1Vez. Lens' (Checkbox t_a1Vez) (Dynamic t_a1Vez Bool)
checkbox_hasFocus
  f_a1Vki
  (Checkbox x1_a1Vkj x2_a1Vkk x3_a1Vkl x4_a1Vkm x5_a1Vkn x6_a1Vko)
  = fmap
      (\ y1_a1Vkp
         -> Checkbox x1_a1Vkj x2_a1Vkk x3_a1Vkl y1_a1Vkp x5_a1Vkn x6_a1Vko)
      (f_a1Vki x4_a1Vkm)
{-# INLINE checkbox_hasFocus #-}
checkbox_indeterminate ::
  forall t_a1Vez. Lens' (Checkbox t_a1Vez) (Dynamic t_a1Vez Bool)
checkbox_indeterminate
  f_a1Vkq
  (Checkbox x1_a1Vkr x2_a1Vks x3_a1Vkt x4_a1Vku x5_a1Vkv x6_a1Vkw)
  = fmap
      (\ y1_a1Vkx
         -> Checkbox x1_a1Vkr x2_a1Vks y1_a1Vkx x4_a1Vku x5_a1Vkv x6_a1Vkw)
      (f_a1Vkq x3_a1Vkt)
{-# INLINE checkbox_indeterminate #-}
checkbox_inputElement ::
  forall t_a1Vez. Lens' (Checkbox t_a1Vez) (El t_a1Vez)
checkbox_inputElement
  f_a1Vky
  (Checkbox x1_a1Vkz x2_a1VkA x3_a1VkB x4_a1VkC x5_a1VkD x6_a1VkE)
  = fmap
      (\ y1_a1VkF
         -> Checkbox x1_a1Vkz x2_a1VkA x3_a1VkB x4_a1VkC x5_a1VkD y1_a1VkF)
      (f_a1Vky x6_a1VkE)
{-# INLINE checkbox_inputElement #-}
checkbox_value ::
  forall t_a1Vez. Lens' (Checkbox t_a1Vez) (Dynamic t_a1Vez Bool)
checkbox_value
  f_a1VkG
  (Checkbox x1_a1VkH x2_a1VkI x3_a1VkJ x4_a1VkK x5_a1VkL x6_a1VkM)
  = fmap
      (\ y1_a1VkN
         -> Checkbox y1_a1VkN x2_a1VkI x3_a1VkJ x4_a1VkK x5_a1VkL x6_a1VkM)
      (f_a1VkG x1_a1VkH)
{-# INLINE checkbox_value #-}
