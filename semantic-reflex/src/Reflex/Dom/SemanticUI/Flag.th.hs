-- src/Reflex/Dom/SemanticUI/Flag.hs:28:1-62: Splicing declarations
flagConfig_elConfig ::
  forall t_a1yrc. Iso' (FlagConfig t_a1yrc) (ActiveElConfig t_a1yrc)
flagConfig_elConfig
  = iso (\ (FlagConfig x_a1yt8) -> x_a1yt8) FlagConfig
{-# INLINE flagConfig_elConfig #-}
