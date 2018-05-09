-- src/Reflex/Dom/SemanticUI/Field.hs:34:1-63: Splicing declarations
fieldConfig_elConfig ::
  forall t_aUt3. Lens' (FieldConfig t_aUt3) (ActiveElConfig t_aUt3)
fieldConfig_elConfig f_aUAq (FieldConfig x1_aUAr x2_aUAs)
  = fmap (\ y1_aUAt -> FieldConfig x1_aUAr y1_aUAt) (f_aUAq x2_aUAs)
{-# INLINE fieldConfig_elConfig #-}
fieldConfig_error ::
  forall t_aUt3. Lens' (FieldConfig t_aUt3) (Active t_aUt3 Bool)
fieldConfig_error f_aUAv (FieldConfig x1_aUAw x2_aUAx)
  = fmap (\ y1_aUAy -> FieldConfig y1_aUAy x2_aUAx) (f_aUAv x1_aUAw)
{-# INLINE fieldConfig_error #-}
