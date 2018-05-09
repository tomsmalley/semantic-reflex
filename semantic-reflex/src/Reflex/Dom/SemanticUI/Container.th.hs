-- src/Reflex/Dom/SemanticUI/Container.hs:31:1-67: Splicing declarations
containerConfig_elConfig ::
  forall t_a2d64.
  Lens' (ContainerConfig t_a2d64) (ActiveElConfig t_a2d64)
containerConfig_elConfig
  f_a2dg5
  (ContainerConfig x1_a2dgg x2_a2dgh)
  = fmap
      (\ y1_a2dgi -> ContainerConfig x1_a2dgg y1_a2dgi)
      (f_a2dg5 x2_a2dgh)
{-# INLINE containerConfig_elConfig #-}
containerConfig_size ::
  forall t_a2d64.
  Lens' (ContainerConfig t_a2d64) (Active t_a2d64 (Maybe Size))
containerConfig_size f_a2dgl (ContainerConfig x1_a2dgm x2_a2dgn)
  = fmap
      (\ y1_a2dgo -> ContainerConfig y1_a2dgo x2_a2dgn)
      (f_a2dgl x1_a2dgm)
{-# INLINE containerConfig_size #-}
