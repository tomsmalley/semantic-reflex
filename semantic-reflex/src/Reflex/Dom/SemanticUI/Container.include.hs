-- src/Reflex/Dom/SemanticUI/Container.hs:27:1-67: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''ContainerConfig
--   ======>
containerConfig_elConfig ::
  forall t_a2iHc.
  Lens' (ContainerConfig t_a2iHc) (ActiveElConfig t_a2iHc)
containerConfig_elConfig
  f_a2iHL
  (ContainerConfig x1_a2iHM x2_a2iHN)
  = fmap
      (\ y1_a2iHO -> ContainerConfig x1_a2iHM y1_a2iHO)
      (f_a2iHL x2_a2iHN)
{-# INLINE containerConfig_elConfig #-}
containerConfig_size ::
  forall t_a2iHc.
  Lens' (ContainerConfig t_a2iHc) (Active t_a2iHc (Maybe Size))
containerConfig_size f_a2iHP (ContainerConfig x1_a2iHQ x2_a2iHR)
  = fmap
      (\ y1_a2iHS -> ContainerConfig y1_a2iHS x2_a2iHR)
      (f_a2iHP x1_a2iHQ)
{-# INLINE containerConfig_size #-}

