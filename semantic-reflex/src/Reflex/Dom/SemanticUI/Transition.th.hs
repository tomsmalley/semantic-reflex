-- src/Reflex/Dom/SemanticUI/Transition.hs:188:1-29: Splicing declarations
transitionConfig_cancelling :: Lens' TransitionConfig Bool
transitionConfig_cancelling
  f_aHMl
  (TransitionConfig x1_aHMm x2_aHMn)
  = fmap
      (\ y1_aHMo -> TransitionConfig x1_aHMm y1_aHMo) (f_aHMl x2_aHMn)
{-# INLINE transitionConfig_cancelling #-}
transitionConfig_duration :: Lens' TransitionConfig NominalDiffTime
transitionConfig_duration f_aHMp (TransitionConfig x1_aHMq x2_aHMr)
  = fmap
      (\ y1_aHMs -> TransitionConfig y1_aHMs x2_aHMr) (f_aHMp x1_aHMq)
{-# INLINE transitionConfig_duration #-}
-- src/Reflex/Dom/SemanticUI/Transition.hs:506:1-22: Splicing declarations
event ::
  forall t_aHMR a_aHMS b_aHMT t_aIQu b_aIQv.
  Lens (SetValue' t_aHMR a_aHMS b_aHMT) (SetValue' t_aIQu a_aHMS b_aIQv) (Maybe (Event t_aHMR b_aHMT)) (Maybe (Event t_aIQu b_aIQv))
event f_aIQx (SetValue x1_aIQy x2_aIQz)
  = fmap (\ y1_aIQA -> SetValue x1_aIQy y1_aIQA) (f_aIQx x2_aIQz)
{-# INLINE event #-}
initial ::
  forall t_aHMR a_aHMS b_aHMT a_aIQw.
  Lens (SetValue' t_aHMR a_aHMS b_aHMT) (SetValue' t_aHMR a_aIQw b_aHMT) a_aHMS a_aIQw
initial f_aIQB (SetValue x1_aIQC x2_aIQD)
  = fmap (\ y1_aIQE -> SetValue y1_aIQE x2_aIQD) (f_aIQB x1_aIQC)
{-# INLINE initial #-}
