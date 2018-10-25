-- src/Reflex/Dom/SemanticUI/Image.hs:59:1-63: Splicing declarations
imageConfig_elConfig ::
  forall t_a1rrq.
  Lens' (ImageConfig t_a1rrq) (ActiveElConfig t_a1rrq)
imageConfig_elConfig
  f_a1rDa
  (ImageConfig x1_a1rDb x2_a1rDd x3_a1rDe x4_a1rDf x5_a1rDg x6_a1rDh)
  = fmap
      (\ y1_a1rDj
         -> ImageConfig
              x1_a1rDb x2_a1rDd x3_a1rDe x4_a1rDf x5_a1rDg y1_a1rDj)
      (f_a1rDa x6_a1rDh)
{-# INLINE imageConfig_elConfig #-}
imageConfig_floated ::
  forall t_a1rrq.
  Lens' (ImageConfig t_a1rrq) (Active t_a1rrq (Maybe Floated))
imageConfig_floated
  f_a1rDl
  (ImageConfig x1_a1rDm x2_a1rDn x3_a1rDo x4_a1rDp x5_a1rDq x6_a1rDr)
  = fmap
      (\ y1_a1rDs
         -> ImageConfig
              x1_a1rDm x2_a1rDn x3_a1rDo y1_a1rDs x5_a1rDq x6_a1rDr)
      (f_a1rDl x4_a1rDp)
{-# INLINE imageConfig_floated #-}
imageConfig_inline ::
  forall t_a1rrq. Lens' (ImageConfig t_a1rrq) (Active t_a1rrq Bool)
imageConfig_inline
  f_a1rDt
  (ImageConfig x1_a1rDu x2_a1rDv x3_a1rDw x4_a1rDx x5_a1rDy x6_a1rDz)
  = fmap
      (\ y1_a1rDA
         -> ImageConfig
              y1_a1rDA x2_a1rDv x3_a1rDw x4_a1rDx x5_a1rDy x6_a1rDz)
      (f_a1rDt x1_a1rDu)
{-# INLINE imageConfig_inline #-}
imageConfig_shape ::
  forall t_a1rrq.
  Lens' (ImageConfig t_a1rrq) (Active t_a1rrq (Maybe ImageShape))
imageConfig_shape
  f_a1rDB
  (ImageConfig x1_a1rDC x2_a1rDD x3_a1rDE x4_a1rDG x5_a1rDH x6_a1rDI)
  = fmap
      (\ y1_a1rDJ
         -> ImageConfig
              x1_a1rDC x2_a1rDD y1_a1rDJ x4_a1rDG x5_a1rDH x6_a1rDI)
      (f_a1rDB x3_a1rDE)
{-# INLINE imageConfig_shape #-}
imageConfig_size ::
  forall t_a1rrq.
  Lens' (ImageConfig t_a1rrq) (Active t_a1rrq (Maybe Size))
imageConfig_size
  f_a1rDK
  (ImageConfig x1_a1rDL x2_a1rDM x3_a1rDN x4_a1rDO x5_a1rDP x6_a1rDQ)
  = fmap
      (\ y1_a1rDR
         -> ImageConfig
              x1_a1rDL y1_a1rDR x3_a1rDN x4_a1rDO x5_a1rDP x6_a1rDQ)
      (f_a1rDK x2_a1rDM)
{-# INLINE imageConfig_size #-}
imageConfig_spaced ::
  forall t_a1rrq.
  Lens' (ImageConfig t_a1rrq) (Active t_a1rrq (Maybe Spaced))
imageConfig_spaced
  f_a1rDS
  (ImageConfig x1_a1rDT x2_a1rDU x3_a1rDV x4_a1rDW x5_a1rDX x6_a1rDY)
  = fmap
      (\ y1_a1rDZ
         -> ImageConfig
              x1_a1rDT x2_a1rDU x3_a1rDV x4_a1rDW y1_a1rDZ x6_a1rDY)
      (f_a1rDS x5_a1rDX)
{-# INLINE imageConfig_spaced #-}
-- src/Reflex/Dom/SemanticUI/Image.hs:162:1-61: Splicing declarations
imgConfig_alt ::
  forall t_a1rIm.
  Lens' (ImgConfig t_a1rIm) (Active t_a1rIm (Maybe Text))
imgConfig_alt f_a1tyB (ImgConfig x1_a1tyC x2_a1tyD x3_a1tyE)
  = fmap
      (\ y1_a1tyF -> ImgConfig x1_a1tyC y1_a1tyF x3_a1tyE)
      (f_a1tyB x2_a1tyD)
{-# INLINE imgConfig_alt #-}
imgConfig_elConfig ::
  forall t_a1rIm. Lens' (ImgConfig t_a1rIm) (ActiveElConfig t_a1rIm)
imgConfig_elConfig f_a1tyH (ImgConfig x1_a1tyI x2_a1tyJ x3_a1tyK)
  = fmap
      (\ y1_a1tyL -> ImgConfig x1_a1tyI x2_a1tyJ y1_a1tyL)
      (f_a1tyH x3_a1tyK)
{-# INLINE imgConfig_elConfig #-}
imgConfig_title ::
  forall t_a1rIm.
  Lens' (ImgConfig t_a1rIm) (Active t_a1rIm (Maybe Text))
imgConfig_title f_a1tyM (ImgConfig x1_a1tyN x2_a1tyO x3_a1tyP)
  = fmap
      (\ y1_a1tyQ -> ImgConfig y1_a1tyQ x2_a1tyO x3_a1tyP)
      (f_a1tyM x1_a1tyN)
{-# INLINE imgConfig_title #-}
