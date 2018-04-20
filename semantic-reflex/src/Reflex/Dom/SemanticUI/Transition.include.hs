-- src/Reflex/Dom/SemanticUI/Transition.hs:187:1-29: Splicing declarations
--     makeLenses ''TransitionConfig
--   ======>
transitionConfig_cancelling :: Lens' TransitionConfig Bool
transitionConfig_cancelling
  f_a4iNs
  (TransitionConfig x1_a4iNt x2_a4iNu x3_a4iNv)
  = fmap
      (\ y1_a4iNw -> TransitionConfig x1_a4iNt x2_a4iNu y1_a4iNw)
      (f_a4iNs x3_a4iNv)
{-# INLINE transitionConfig_cancelling #-}
transitionConfig_direction ::
  Lens' TransitionConfig (Maybe Direction)
transitionConfig_direction
  f_a4iNx
  (TransitionConfig x1_a4iNy x2_a4iNz x3_a4iNA)
  = fmap
      (\ y1_a4iNB -> TransitionConfig x1_a4iNy y1_a4iNB x3_a4iNA)
      (f_a4iNx x2_a4iNz)
{-# INLINE transitionConfig_direction #-}
transitionConfig_duration :: Lens' TransitionConfig NominalDiffTime
transitionConfig_duration
  f_a4iNC
  (TransitionConfig x1_a4iND x2_a4iNE x3_a4iNF)
  = fmap
      (\ y1_a4iNG -> TransitionConfig y1_a4iNG x2_a4iNE x3_a4iNF)
      (f_a4iNC x1_a4iND)
{-# INLINE transitionConfig_duration #-}
-- src/Reflex/Dom/SemanticUI/Transition.hs:517:1-22: Splicing declarations
--     makeLenses ''SetValue'
--   ======>
event ::
  forall t_a4iNH a_a4iNI b_a4iNJ t_a4jtV b_a4jtW.
  Control.Lens.Type.Lens (SetValue' t_a4iNH a_a4iNI b_a4iNJ) (SetValue' t_a4jtV a_a4iNI b_a4jtW) (Maybe (Event t_a4iNH b_a4iNJ)) (Maybe (Event t_a4jtV b_a4jtW))
event f_a4jtY (SetValue x1_a4jtZ x2_a4ju0)
  = fmap
      (\ y1_a4ju1 -> SetValue x1_a4jtZ y1_a4ju1) (f_a4jtY x2_a4ju0)
{-# INLINE event #-}
initial ::
  forall t_a4iNH a_a4iNI b_a4iNJ a_a4jtX.
  Control.Lens.Type.Lens (SetValue' t_a4iNH a_a4iNI b_a4iNJ) (SetValue' t_a4iNH a_a4jtX b_a4iNJ) a_a4iNI a_a4jtX
initial f_a4ju2 (SetValue x1_a4ju3 x2_a4ju4)
  = fmap
      (\ y1_a4ju5 -> SetValue y1_a4ju5 x2_a4ju4) (f_a4ju2 x1_a4ju3)
{-# INLINE initial #-}

