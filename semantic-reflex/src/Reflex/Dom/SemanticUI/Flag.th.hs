-- src/Reflex/Dom/SemanticUI/Flag.hs:28:1-62: Splicing declarations
flagConfig_elConfig ::
  forall t_a1ydZ. Iso' (FlagConfig t_a1ydZ) (ActiveElConfig t_a1ydZ)
flagConfig_elConfig
  = iso (\ (FlagConfig x_a1yxa) -> x_a1yxa) FlagConfig
{-# INLINE flagConfig_elConfig #-}
