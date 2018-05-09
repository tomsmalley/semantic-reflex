-- src/Reflex/Dom/SemanticUI/Form.hs:49:1-62: Splicing declarations
formConfig_elConfig ::
  forall t_a23SN. Iso' (FormConfig t_a23SN) (ActiveElConfig t_a23SN)
formConfig_elConfig
  = iso (\ (FormConfig x_a23WD) -> x_a23WD) FormConfig
{-# INLINE formConfig_elConfig #-}
