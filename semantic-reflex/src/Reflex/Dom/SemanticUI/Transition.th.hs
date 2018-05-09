-- src/Reflex/Dom/SemanticUI/Transition.hs:193:1-29: Splicing declarations
transitionConfig_cancelling :: Lens' TransitionConfig Bool
transitionConfig_cancelling
  f_aHGD
  (TransitionConfig x1_aHGE x2_aHGF x3_aHGG)
  = fmap
      (\ y1_aHGH -> TransitionConfig x1_aHGE x2_aHGF y1_aHGH)
      (f_aHGD x3_aHGG)
{-# INLINE transitionConfig_cancelling #-}
transitionConfig_direction ::
  Lens' TransitionConfig (Maybe Direction)
transitionConfig_direction
  f_aHGI
  (TransitionConfig x1_aHGJ x2_aHGK x3_aHGL)
  = fmap
      (\ y1_aHGM -> TransitionConfig x1_aHGJ y1_aHGM x3_aHGL)
      (f_aHGI x2_aHGK)
{-# INLINE transitionConfig_direction #-}
transitionConfig_duration :: Lens' TransitionConfig NominalDiffTime
transitionConfig_duration
  f_aHGN
  (TransitionConfig x1_aHGO x2_aHGP x3_aHGQ)
  = fmap
      (\ y1_aHGR -> TransitionConfig y1_aHGR x2_aHGP x3_aHGQ)
      (f_aHGN x1_aHGO)
{-# INLINE transitionConfig_duration #-}
-- src/Reflex/Dom/SemanticUI/Transition.hs:495:1-22: Splicing declarations
event ::
  forall t_aHH7 a_aHH8 b_aHH9 t_aILj b_aILk.
  Lens (SetValue' t_aHH7 a_aHH8 b_aHH9) (SetValue' t_aILj a_aHH8 b_aILk) (Maybe (Event t_aHH7 b_aHH9)) (Maybe (Event t_aILj b_aILk))
event f_aILm (SetValue x1_aILn x2_aILo)
  = fmap (\ y1_aILp -> SetValue x1_aILn y1_aILp) (f_aILm x2_aILo)
{-# INLINE event #-}
initial ::
  forall t_aHH7 a_aHH8 b_aHH9 a_aILl.
  Lens (SetValue' t_aHH7 a_aHH8 b_aHH9) (SetValue' t_aHH7 a_aILl b_aHH9) a_aHH8 a_aILl
initial f_aILq (SetValue x1_aILr x2_aILs)
  = fmap (\ y1_aILt -> SetValue y1_aILt x2_aILs) (f_aILq x1_aILr)
{-# INLINE initial #-}
