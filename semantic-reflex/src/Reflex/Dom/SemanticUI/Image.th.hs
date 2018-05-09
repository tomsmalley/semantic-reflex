-- src/Reflex/Dom/SemanticUI/Image.hs:59:1-63: Splicing declarations
imageConfig_elConfig ::
  forall t_a2ep8.
  Lens' (ImageConfig t_a2ep8) (ActiveElConfig t_a2ep8)
imageConfig_elConfig
  f_a2eDR
  (ImageConfig x1_a2eDS x2_a2eDT x3_a2eDU x4_a2eDV x5_a2eDW x6_a2eDX)
  = fmap
      (\ y1_a2eDY
         -> ImageConfig
              x1_a2eDS x2_a2eDT x3_a2eDU x4_a2eDV x5_a2eDW y1_a2eDY)
      (f_a2eDR x6_a2eDX)
{-# INLINE imageConfig_elConfig #-}
imageConfig_floated ::
  forall t_a2ep8.
  Lens' (ImageConfig t_a2ep8) (Active t_a2ep8 (Maybe Floated))
imageConfig_floated
  f_a2eE0
  (ImageConfig x1_a2eE1 x2_a2eE2 x3_a2eE3 x4_a2eE5 x5_a2eE6 x6_a2eE7)
  = fmap
      (\ y1_a2eE8
         -> ImageConfig
              x1_a2eE1 x2_a2eE2 x3_a2eE3 y1_a2eE8 x5_a2eE6 x6_a2eE7)
      (f_a2eE0 x4_a2eE5)
{-# INLINE imageConfig_floated #-}
imageConfig_inline ::
  forall t_a2ep8. Lens' (ImageConfig t_a2ep8) (Active t_a2ep8 Bool)
imageConfig_inline
  f_a2eE9
  (ImageConfig x1_a2eEa x2_a2eEb x3_a2eEc x4_a2eEe x5_a2eEf x6_a2eEg)
  = fmap
      (\ y1_a2eEh
         -> ImageConfig
              y1_a2eEh x2_a2eEb x3_a2eEc x4_a2eEe x5_a2eEf x6_a2eEg)
      (f_a2eE9 x1_a2eEa)
{-# INLINE imageConfig_inline #-}
imageConfig_shape ::
  forall t_a2ep8.
  Lens' (ImageConfig t_a2ep8) (Active t_a2ep8 (Maybe ImageShape))
imageConfig_shape
  f_a2eEi
  (ImageConfig x1_a2eEj x2_a2eEk x3_a2eEm x4_a2eEn x5_a2eEo x6_a2eEp)
  = fmap
      (\ y1_a2eEq
         -> ImageConfig
              x1_a2eEj x2_a2eEk y1_a2eEq x4_a2eEn x5_a2eEo x6_a2eEp)
      (f_a2eEi x3_a2eEm)
{-# INLINE imageConfig_shape #-}
imageConfig_size ::
  forall t_a2ep8.
  Lens' (ImageConfig t_a2ep8) (Active t_a2ep8 (Maybe Size))
imageConfig_size
  f_a2eEr
  (ImageConfig x1_a2eEs x2_a2eEt x3_a2eEu x4_a2eEv x5_a2eEx x6_a2eEy)
  = fmap
      (\ y1_a2eEz
         -> ImageConfig
              x1_a2eEs y1_a2eEz x3_a2eEu x4_a2eEv x5_a2eEx x6_a2eEy)
      (f_a2eEr x2_a2eEt)
{-# INLINE imageConfig_size #-}
imageConfig_spaced ::
  forall t_a2ep8.
  Lens' (ImageConfig t_a2ep8) (Active t_a2ep8 (Maybe Spaced))
imageConfig_spaced
  f_a2eEA
  (ImageConfig x1_a2eEB x2_a2eEC x3_a2eED x4_a2eEE x5_a2eEF x6_a2eEG)
  = fmap
      (\ y1_a2eEI
         -> ImageConfig
              x1_a2eEB x2_a2eEC x3_a2eED x4_a2eEE y1_a2eEI x6_a2eEG)
      (f_a2eEA x5_a2eEF)
{-# INLINE imageConfig_spaced #-}
-- src/Reflex/Dom/SemanticUI/Image.hs:162:1-61: Splicing declarations
imgConfig_alt ::
  forall t_a2eHw.
  Lens' (ImgConfig t_a2eHw) (Active t_a2eHw (Maybe Text))
imgConfig_alt f_a2gp5 (ImgConfig x1_a2gp6 x2_a2gp7 x3_a2gp8)
  = fmap
      (\ y1_a2gp9 -> ImgConfig x1_a2gp6 y1_a2gp9 x3_a2gp8)
      (f_a2gp5 x2_a2gp7)
{-# INLINE imgConfig_alt #-}
imgConfig_elConfig ::
  forall t_a2eHw. Lens' (ImgConfig t_a2eHw) (ActiveElConfig t_a2eHw)
imgConfig_elConfig f_a2gpa (ImgConfig x1_a2gpb x2_a2gpc x3_a2gpd)
  = fmap
      (\ y1_a2gpe -> ImgConfig x1_a2gpb x2_a2gpc y1_a2gpe)
      (f_a2gpa x3_a2gpd)
{-# INLINE imgConfig_elConfig #-}
imgConfig_title ::
  forall t_a2eHw.
  Lens' (ImgConfig t_a2eHw) (Active t_a2eHw (Maybe Text))
imgConfig_title f_a2gpg (ImgConfig x1_a2gph x2_a2gpj x3_a2gpl)
  = fmap
      (\ y1_a2gpm -> ImgConfig y1_a2gpm x2_a2gpj x3_a2gpl)
      (f_a2gpg x1_a2gph)
{-# INLINE imgConfig_title #-}
