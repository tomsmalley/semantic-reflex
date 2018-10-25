-- src/Reflex/Dom/SemanticUI/Container.hs:31:1-67: Splicing declarations
containerConfig_elConfig ::
  forall t_aTNp.
  Lens' (ContainerConfig t_aTNp) (ActiveElConfig t_aTNp)
containerConfig_elConfig f_aUbR (ContainerConfig x1_aUbT x2_aUbU)
  = fmap
      (\ y1_aUbV -> ContainerConfig x1_aUbT y1_aUbV) (f_aUbR x2_aUbU)
{-# INLINE containerConfig_elConfig #-}
containerConfig_size ::
  forall t_aTNp.
  Lens' (ContainerConfig t_aTNp) (Active t_aTNp (Maybe Size))
containerConfig_size f_aUc0 (ContainerConfig x1_aUc1 x2_aUc3)
  = fmap
      (\ y1_aUc4 -> ContainerConfig y1_aUc4 x2_aUc3) (f_aUc0 x1_aUc1)
{-# INLINE containerConfig_size #-}
