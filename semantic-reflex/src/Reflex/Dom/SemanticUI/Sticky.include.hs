-- src/Reflex/Dom/SemanticUI/Sticky.hs:32:1-64: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''StickyConfig
--   ======>
stickyConfig_elConfig ::
  forall t_a4dyh.
  Control.Lens.Type.Lens' (StickyConfig t_a4dyh) (ActiveElConfig t_a4dyh)
stickyConfig_elConfig f_a4dyR (StickyConfig x1_a4dyS x2_a4dyT)
  = fmap
      (\ y1_a4dyU -> StickyConfig x1_a4dyS y1_a4dyU) (f_a4dyR x2_a4dyT)
{-# INLINE stickyConfig_elConfig #-}
stickyConfig_pushing ::
  forall t_a4dyh. Control.Lens.Type.Lens' (StickyConfig t_a4dyh) Bool
stickyConfig_pushing f_a4dyV (StickyConfig x1_a4dyW x2_a4dyX)
  = fmap
      (\ y1_a4dyY -> StickyConfig y1_a4dyY x2_a4dyX) (f_a4dyV x1_a4dyW)
{-# INLINE stickyConfig_pushing #-}

