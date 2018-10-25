-- src/Reflex/Dom/SemanticUI/Sticky.hs:37:1-64: Splicing declarations
stickyConfig_elConfig ::
  forall t_a2IqS.
  Lens' (StickyConfig t_a2IqS) (ActiveElConfig t_a2IqS)
stickyConfig_elConfig f_a2IuU (StickyConfig x1_a2IuV x2_a2IuW)
  = fmap
      (\ y1_a2IuY -> StickyConfig x1_a2IuV y1_a2IuY) (f_a2IuU x2_a2IuW)
{-# INLINE stickyConfig_elConfig #-}
stickyConfig_pushing ::
  forall t_a2IqS. Lens' (StickyConfig t_a2IqS) Bool
stickyConfig_pushing f_a2Iv1 (StickyConfig x1_a2Iv2 x2_a2Iv3)
  = fmap
      (\ y1_a2Iv4 -> StickyConfig y1_a2Iv4 x2_a2Iv3) (f_a2Iv1 x1_a2Iv2)
{-# INLINE stickyConfig_pushing #-}
