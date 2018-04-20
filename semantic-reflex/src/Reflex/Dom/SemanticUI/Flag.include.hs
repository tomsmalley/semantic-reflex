-- src/Reflex/Dom/SemanticUI/Flag.hs:23:1-62: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''FlagConfig
--   ======>
flagConfig_elConfig ::
  forall t_a38NQ.
  Control.Lens.Type.Iso' (FlagConfig t_a38NQ) (ActiveElConfig t_a38NQ)
flagConfig_elConfig
  = Control.Lens.Iso.iso
      (\ (FlagConfig x_a38Om) -> x_a38Om) FlagConfig
{-# INLINE flagConfig_elConfig #-}

