-- src/Reflex/Dom/SemanticUI/Sidebar.hs:80:1-65: Splicing declarations
sidebarConfig_closeOnClick ::
  forall t_a2EzL.
  Lens' (SidebarConfig t_a2EzL) (Dynamic t_a2EzL Bool)
sidebarConfig_closeOnClick
  f_a2F75
  (SidebarConfig x1_a2F76 x2_a2F77 x3_a2F78)
  = fmap
      (\ y1_a2F79 -> SidebarConfig x1_a2F76 y1_a2F79 x3_a2F78)
      (f_a2F75 x2_a2F77)
{-# INLINE sidebarConfig_closeOnClick #-}
sidebarConfig_dimming ::
  forall t_a2EzL.
  Lens' (SidebarConfig t_a2EzL) (Dynamic t_a2EzL Bool)
sidebarConfig_dimming
  f_a2F7b
  (SidebarConfig x1_a2F7c x2_a2F7d x3_a2F7e)
  = fmap
      (\ y1_a2F7f -> SidebarConfig y1_a2F7f x2_a2F7d x3_a2F7e)
      (f_a2F7b x1_a2F7c)
{-# INLINE sidebarConfig_dimming #-}
sidebarConfig_transition ::
  forall t_a2EzL.
  Lens' (SidebarConfig t_a2EzL) (Dynamic t_a2EzL SidebarTransition)
sidebarConfig_transition
  f_a2F7g
  (SidebarConfig x1_a2F7h x2_a2F7i x3_a2F7j)
  = fmap
      (\ y1_a2F7k -> SidebarConfig x1_a2F7h x2_a2F7i y1_a2F7k)
      (f_a2F7g x3_a2F7j)
{-# INLINE sidebarConfig_transition #-}
