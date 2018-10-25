-- src/Reflex/Dom/SemanticUI/Transition.hs:188:1-29: Splicing declarations
transitionConfig_cancelling :: Lens' TransitionConfig Bool
transitionConfig_cancelling
  f_aHJX
  (TransitionConfig x1_aHJY x2_aHJZ)
  = fmap
      (\ y1_aHK0 -> TransitionConfig x1_aHJY y1_aHK0) (f_aHJX x2_aHJZ)
{-# INLINE transitionConfig_cancelling #-}
transitionConfig_duration :: Lens' TransitionConfig NominalDiffTime
transitionConfig_duration f_aHK1 (TransitionConfig x1_aHK2 x2_aHK3)
  = fmap
      (\ y1_aHK4 -> TransitionConfig y1_aHK4 x2_aHK3) (f_aHK1 x1_aHK2)
{-# INLINE transitionConfig_duration #-}
-- src/Reflex/Dom/SemanticUI/Transition.hs:506:1-22: Splicing declarations
event ::
  forall t_aHKm a_aHKn b_aHKo t_aINY b_aINZ.
  Lens (SetValue' t_aHKm a_aHKn b_aHKo) (SetValue' t_aINY a_aHKn b_aINZ) (Maybe (Event t_aHKm b_aHKo)) (Maybe (Event t_aINY b_aINZ))
event f_aIO1 (SetValue x1_aIO2 x2_aIO3)
  = fmap (\ y1_aIO4 -> SetValue x1_aIO2 y1_aIO4) (f_aIO1 x2_aIO3)
{-# INLINE event #-}
initial ::
  forall t_aHKm a_aHKn b_aHKo a_aIO0.
  Lens (SetValue' t_aHKm a_aHKn b_aHKo) (SetValue' t_aHKm a_aIO0 b_aHKo) a_aHKn a_aIO0
initial f_aIO5 (SetValue x1_aIO6 x2_aIO7)
  = fmap (\ y1_aIO8 -> SetValue y1_aIO8 x2_aIO7) (f_aIO5 x1_aIO6)
{-# INLINE initial #-}
