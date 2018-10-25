-- src/Reflex/Dom/SemanticUI/Field.hs:34:1-63: Splicing declarations
fieldConfig_elConfig ::
  forall t_a1cOk.
  Lens' (FieldConfig t_a1cOk) (ActiveElConfig t_a1cOk)
fieldConfig_elConfig f_a1cTl (FieldConfig x1_a1cTm x2_a1cTn)
  = fmap
      (\ y1_a1cTo -> FieldConfig x1_a1cTm y1_a1cTo) (f_a1cTl x2_a1cTn)
{-# INLINE fieldConfig_elConfig #-}
fieldConfig_error ::
  forall t_a1cOk. Lens' (FieldConfig t_a1cOk) (Active t_a1cOk Bool)
fieldConfig_error f_a1cTs (FieldConfig x1_a1cTt x2_a1cTu)
  = fmap
      (\ y1_a1cTv -> FieldConfig y1_a1cTv x2_a1cTu) (f_a1cTs x1_a1cTt)
{-# INLINE fieldConfig_error #-}
