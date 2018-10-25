-- src/Reflex/Dom/SemanticUI/Container.hs:31:1-67: Splicing declarations
containerConfig_elConfig ::
  forall t_aT2D.
  Lens' (ContainerConfig t_aT2D) (ActiveElConfig t_aT2D)
containerConfig_elConfig f_aTd3 (ContainerConfig x1_aTd4 x2_aTd5)
  = fmap
      (\ y1_aTd6 -> ContainerConfig x1_aTd4 y1_aTd6) (f_aTd3 x2_aTd5)
{-# INLINE containerConfig_elConfig #-}
containerConfig_size ::
  forall t_aT2D.
  Lens' (ContainerConfig t_aT2D) (Active t_aT2D (Maybe Size))
containerConfig_size f_aTd7 (ContainerConfig x1_aTd8 x2_aTd9)
  = fmap
      (\ y1_aTda -> ContainerConfig y1_aTda x2_aTd9) (f_aTd7 x1_aTd8)
{-# INLINE containerConfig_size #-}
