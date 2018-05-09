-- src/Reflex/Dom/SemanticUI/Sticky.hs:37:1-64: Splicing declarations
stickyConfig_elConfig ::
  forall t_a2rgp.
  Lens' (StickyConfig t_a2rgp) (ActiveElConfig t_a2rgp)
stickyConfig_elConfig f_a2rqb (StickyConfig x1_a2rqd x2_a2rqe)
  = fmap
      (\ y1_a2rqg -> StickyConfig x1_a2rqd y1_a2rqg) (f_a2rqb x2_a2rqe)
{-# INLINE stickyConfig_elConfig #-}
stickyConfig_pushing ::
  forall t_a2rgp. Lens' (StickyConfig t_a2rgp) Bool
stickyConfig_pushing f_a2rqm (StickyConfig x1_a2rqn x2_a2rqo)
  = fmap
      (\ y1_a2rqq -> StickyConfig y1_a2rqq x2_a2rqo) (f_a2rqm x1_a2rqn)
{-# INLINE stickyConfig_pushing #-}
