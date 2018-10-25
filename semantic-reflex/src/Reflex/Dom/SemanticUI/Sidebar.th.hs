-- src/Reflex/Dom/SemanticUI/Sidebar.hs:77:1-65: Splicing declarations
sidebarConfig_closeOnClick ::
  forall t_aUqg. Lens' (SidebarConfig t_aUqg) (Dynamic t_aUqg Bool)
sidebarConfig_closeOnClick
  f_aUXm
  (SidebarConfig x1_aUXn x2_aUXo x3_aUXp)
  = fmap
      (\ y1_aUXq -> SidebarConfig x1_aUXn y1_aUXq x3_aUXp)
      (f_aUXm x2_aUXo)
{-# INLINE sidebarConfig_closeOnClick #-}
sidebarConfig_dimming ::
  forall t_aUqg. Lens' (SidebarConfig t_aUqg) (Dynamic t_aUqg Bool)
sidebarConfig_dimming
  f_aUXu
  (SidebarConfig x1_aUXv x2_aUXw x3_aUXx)
  = fmap
      (\ y1_aUXy -> SidebarConfig y1_aUXy x2_aUXw x3_aUXx)
      (f_aUXu x1_aUXv)
{-# INLINE sidebarConfig_dimming #-}
sidebarConfig_transition ::
  forall t_aUqg.
  Lens' (SidebarConfig t_aUqg) (Dynamic t_aUqg SidebarTransition)
sidebarConfig_transition
  f_aUXC
  (SidebarConfig x1_aUXD x2_aUXE x3_aUXF)
  = fmap
      (\ y1_aUXG -> SidebarConfig x1_aUXD x2_aUXE y1_aUXG)
      (f_aUXC x3_aUXF)
{-# INLINE sidebarConfig_transition #-}
