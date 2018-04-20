-- src/Reflex/Dom/SemanticUI/Field.hs:30:1-63: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''FieldConfig
--   ======>
fieldConfig_elConfig ::
  forall t_a370z.
  Control.Lens.Type.Lens' (FieldConfig t_a370z) (ActiveElConfig t_a370z)
fieldConfig_elConfig f_a3718 (FieldConfig x1_a3719 x2_a371a)
  = fmap
      (\ y1_a371b -> FieldConfig x1_a3719 y1_a371b) (f_a3718 x2_a371a)
{-# INLINE fieldConfig_elConfig #-}
fieldConfig_error ::
  forall t_a370z.
  Control.Lens.Type.Lens' (FieldConfig t_a370z) (Active t_a370z Bool)
fieldConfig_error f_a371c (FieldConfig x1_a371d x2_a371e)
  = fmap
      (\ y1_a371f -> FieldConfig y1_a371f x2_a371e) (f_a371c x1_a371d)
{-# INLINE fieldConfig_error #-}

