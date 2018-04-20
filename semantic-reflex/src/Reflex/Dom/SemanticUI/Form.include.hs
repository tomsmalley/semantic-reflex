-- src/Reflex/Dom/SemanticUI/Form.hs:43:1-62: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''FormConfig
--   ======>
formConfig_elConfig ::
  forall t_a3aMI.
  Control.Lens.Type.Iso' (FormConfig t_a3aMI) (ActiveElConfig t_a3aMI)
formConfig_elConfig
  = Control.Lens.Iso.iso
      (\ (FormConfig x_a3aNe) -> x_a3aNe) FormConfig
{-# INLINE formConfig_elConfig #-}

