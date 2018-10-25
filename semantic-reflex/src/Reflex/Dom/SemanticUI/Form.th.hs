-- src/Reflex/Dom/SemanticUI/Form.hs:49:1-62: Splicing declarations
formConfig_elConfig ::
  forall t_a2kF0. Iso' (FormConfig t_a2kF0) (ActiveElConfig t_a2kF0)
formConfig_elConfig
  = iso (\ (FormConfig x_a2kGe) -> x_a2kGe) FormConfig
{-# INLINE formConfig_elConfig #-}
