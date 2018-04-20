-- src/Reflex/Dom/SemanticUI/Image.hs:54:1-63: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''ImageConfig
--   ======>
imageConfig_elConfig ::
  forall t_a3wkQ.
  Control.Lens.Type.Lens' (ImageConfig t_a3wkQ) (ActiveElConfig t_a3wkQ)
imageConfig_elConfig
  f_a3wnB
  (ImageConfig x1_a3wnC x2_a3wnD x3_a3wnE x4_a3wnF x5_a3wnG x6_a3wnH)
  = fmap
      (\ y1_a3wnI
         -> ImageConfig
              x1_a3wnC x2_a3wnD x3_a3wnE x4_a3wnF x5_a3wnG y1_a3wnI)
      (f_a3wnB x6_a3wnH)
{-# INLINE imageConfig_elConfig #-}
imageConfig_floated ::
  forall t_a3wkQ.
  Control.Lens.Type.Lens' (ImageConfig t_a3wkQ) (Active t_a3wkQ (Maybe Floated))
imageConfig_floated
  f_a3wnJ
  (ImageConfig x1_a3wnK x2_a3wnL x3_a3wnM x4_a3wnN x5_a3wnO x6_a3wnP)
  = fmap
      (\ y1_a3wnQ
         -> ImageConfig
              x1_a3wnK x2_a3wnL x3_a3wnM y1_a3wnQ x5_a3wnO x6_a3wnP)
      (f_a3wnJ x4_a3wnN)
{-# INLINE imageConfig_floated #-}
imageConfig_inline ::
  forall t_a3wkQ.
  Control.Lens.Type.Lens' (ImageConfig t_a3wkQ) (Active t_a3wkQ Bool)
imageConfig_inline
  f_a3wnR
  (ImageConfig x1_a3wnS x2_a3wnT x3_a3wnU x4_a3wnV x5_a3wnW x6_a3wnX)
  = fmap
      (\ y1_a3wnY
         -> ImageConfig
              y1_a3wnY x2_a3wnT x3_a3wnU x4_a3wnV x5_a3wnW x6_a3wnX)
      (f_a3wnR x1_a3wnS)
{-# INLINE imageConfig_inline #-}
imageConfig_shape ::
  forall t_a3wkQ.
  Control.Lens.Type.Lens' (ImageConfig t_a3wkQ) (Active t_a3wkQ (Maybe ImageShape))
imageConfig_shape
  f_a3wnZ
  (ImageConfig x1_a3wo0 x2_a3wo1 x3_a3wo2 x4_a3wo3 x5_a3wo4 x6_a3wo5)
  = fmap
      (\ y1_a3wo6
         -> ImageConfig
              x1_a3wo0 x2_a3wo1 y1_a3wo6 x4_a3wo3 x5_a3wo4 x6_a3wo5)
      (f_a3wnZ x3_a3wo2)
{-# INLINE imageConfig_shape #-}
imageConfig_size ::
  forall t_a3wkQ.
  Control.Lens.Type.Lens' (ImageConfig t_a3wkQ) (Active t_a3wkQ (Maybe Size))
imageConfig_size
  f_a3wo7
  (ImageConfig x1_a3wo8 x2_a3wo9 x3_a3woa x4_a3wob x5_a3woc x6_a3wod)
  = fmap
      (\ y1_a3woe
         -> ImageConfig
              x1_a3wo8 y1_a3woe x3_a3woa x4_a3wob x5_a3woc x6_a3wod)
      (f_a3wo7 x2_a3wo9)
{-# INLINE imageConfig_size #-}
imageConfig_spaced ::
  forall t_a3wkQ.
  Control.Lens.Type.Lens' (ImageConfig t_a3wkQ) (Active t_a3wkQ (Maybe Spaced))
imageConfig_spaced
  f_a3wof
  (ImageConfig x1_a3wog x2_a3woh x3_a3woi x4_a3woj x5_a3wok x6_a3wol)
  = fmap
      (\ y1_a3wom
         -> ImageConfig
              x1_a3wog x2_a3woh x3_a3woi x4_a3woj y1_a3wom x6_a3wol)
      (f_a3wof x5_a3wok)
{-# INLINE imageConfig_spaced #-}
-- src/Reflex/Dom/SemanticUI/Image.hs:155:1-61: Splicing declarations
--     makeLensesWith (lensRules & simpleLenses .~ True) ''ImgConfig
--   ======>
imgConfig_alt ::
  forall t_a3won.
  Control.Lens.Type.Lens' (ImgConfig t_a3won) (Active t_a3won (Maybe Text))
imgConfig_alt f_a3wRh (ImgConfig x1_a3wRi x2_a3wRj x3_a3wRk)
  = fmap
      (\ y1_a3wRl -> ImgConfig x1_a3wRi y1_a3wRl x3_a3wRk)
      (f_a3wRh x2_a3wRj)
{-# INLINE imgConfig_alt #-}
imgConfig_elConfig ::
  forall t_a3won.
  Control.Lens.Type.Lens' (ImgConfig t_a3won) (ActiveElConfig t_a3won)
imgConfig_elConfig f_a3wRm (ImgConfig x1_a3wRn x2_a3wRo x3_a3wRp)
  = fmap
      (\ y1_a3wRq -> ImgConfig x1_a3wRn x2_a3wRo y1_a3wRq)
      (f_a3wRm x3_a3wRp)
{-# INLINE imgConfig_elConfig #-}
imgConfig_title ::
  forall t_a3won.
  Control.Lens.Type.Lens' (ImgConfig t_a3won) (Active t_a3won (Maybe Text))
imgConfig_title f_a3wRr (ImgConfig x1_a3wRs x2_a3wRt x3_a3wRu)
  = fmap
      (\ y1_a3wRv -> ImgConfig y1_a3wRv x2_a3wRt x3_a3wRu)
      (f_a3wRr x1_a3wRs)
{-# INLINE imgConfig_title #-}

