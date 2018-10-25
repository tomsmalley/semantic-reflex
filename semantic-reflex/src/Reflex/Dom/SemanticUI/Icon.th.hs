-- src/Reflex/Dom/SemanticUI/Icon.hs:83:1-62: Splicing declarations
iconConfig_bordered ::
  forall t_a1Az4. Lens' (IconConfig t_a1Az4) (Active t_a1Az4 Bool)
iconConfig_bordered
  f_a1Bdu
  (IconConfig x1_a1Bdv
              x2_a1Bdw
              x3_a1Bdx
              x4_a1Bdy
              x5_a1Bdz
              x6_a1BdA
              x7_a1BdC
              x8_a1BdD
              x9_a1BdE
              x10_a1BdF
              x11_a1BdG
              x12_a1BdH
              x13_a1BdI
              x14_a1BdJ)
  = fmap
      (\ y1_a1BdK
         -> IconConfig
              x1_a1Bdv
              x2_a1Bdw
              x3_a1Bdx
              x4_a1Bdy
              x5_a1Bdz
              y1_a1BdK
              x7_a1BdC
              x8_a1BdD
              x9_a1BdE
              x10_a1BdF
              x11_a1BdG
              x12_a1BdH
              x13_a1BdI
              x14_a1BdJ)
      (f_a1Bdu x6_a1BdA)
{-# INLINE iconConfig_bordered #-}
iconConfig_circular ::
  forall t_a1Az4. Lens' (IconConfig t_a1Az4) (Active t_a1Az4 Bool)
iconConfig_circular
  f_a1BdQ
  (IconConfig x1_a1BdR
              x2_a1BdS
              x3_a1BdT
              x4_a1BdU
              x5_a1BdY
              x6_a1BdZ
              x7_a1Be0
              x8_a1Be1
              x9_a1Be2
              x10_a1Be3
              x11_a1Be4
              x12_a1Be5
              x13_a1Be6
              x14_a1Be7)
  = fmap
      (\ y1_a1Be8
         -> IconConfig
              x1_a1BdR
              x2_a1BdS
              x3_a1BdT
              x4_a1BdU
              y1_a1Be8
              x6_a1BdZ
              x7_a1Be0
              x8_a1Be1
              x9_a1Be2
              x10_a1Be3
              x11_a1Be4
              x12_a1Be5
              x13_a1Be6
              x14_a1Be7)
      (f_a1BdQ x5_a1BdY)
{-# INLINE iconConfig_circular #-}
iconConfig_color ::
  forall t_a1Az4.
  Lens' (IconConfig t_a1Az4) (Active t_a1Az4 (Maybe Color))
iconConfig_color
  f_a1Be9
  (IconConfig x1_a1Bea
              x2_a1Beb
              x3_a1Bec
              x4_a1Bed
              x5_a1Bee
              x6_a1Bef
              x7_a1Beg
              x8_a1Beh
              x9_a1Bei
              x10_a1Bej
              x11_a1Bek
              x12_a1Bel
              x13_a1Bem
              x14_a1Ben)
  = fmap
      (\ y1_a1Beo
         -> IconConfig
              x1_a1Bea
              x2_a1Beb
              x3_a1Bec
              x4_a1Bed
              x5_a1Bee
              x6_a1Bef
              x7_a1Beg
              x8_a1Beh
              x9_a1Bei
              x10_a1Bej
              y1_a1Beo
              x12_a1Bel
              x13_a1Bem
              x14_a1Ben)
      (f_a1Be9 x11_a1Bek)
{-# INLINE iconConfig_color #-}
iconConfig_corner ::
  forall t_a1Az4.
  Lens' (IconConfig t_a1Az4) (Active t_a1Az4 (Maybe Corner))
iconConfig_corner
  f_a1Beq
  (IconConfig x1_a1Ber
              x2_a1Bes
              x3_a1Bet
              x4_a1Beu
              x5_a1Bev
              x6_a1Bew
              x7_a1Bex
              x8_a1Bey
              x9_a1Bez
              x10_a1BeA
              x11_a1BeB
              x12_a1BeC
              x13_a1BeD
              x14_a1BeE)
  = fmap
      (\ y1_a1BeF
         -> IconConfig
              x1_a1Ber
              x2_a1Bes
              x3_a1Bet
              x4_a1Beu
              x5_a1Bev
              x6_a1Bew
              x7_a1Bex
              x8_a1Bey
              x9_a1Bez
              x10_a1BeA
              x11_a1BeB
              y1_a1BeF
              x13_a1BeD
              x14_a1BeE)
      (f_a1Beq x12_a1BeC)
{-# INLINE iconConfig_corner #-}
iconConfig_disabled ::
  forall t_a1Az4. Lens' (IconConfig t_a1Az4) (Active t_a1Az4 Bool)
iconConfig_disabled
  f_a1BeO
  (IconConfig x1_a1BeP
              x2_a1BeQ
              x3_a1BeR
              x4_a1BeU
              x5_a1BeV
              x6_a1BeY
              x7_a1BeZ
              x8_a1Bf0
              x9_a1Bf1
              x10_a1Bf2
              x11_a1Bf3
              x12_a1Bf5
              x13_a1Bf8
              x14_a1Bf9)
  = fmap
      (\ y1_a1Bfa
         -> IconConfig
              y1_a1Bfa
              x2_a1BeQ
              x3_a1BeR
              x4_a1BeU
              x5_a1BeV
              x6_a1BeY
              x7_a1BeZ
              x8_a1Bf0
              x9_a1Bf1
              x10_a1Bf2
              x11_a1Bf3
              x12_a1Bf5
              x13_a1Bf8
              x14_a1Bf9)
      (f_a1BeO x1_a1BeP)
{-# INLINE iconConfig_disabled #-}
iconConfig_elConfig ::
  forall t_a1Az4. Lens' (IconConfig t_a1Az4) (ActiveElConfig t_a1Az4)
iconConfig_elConfig
  f_a1Bfc
  (IconConfig x1_a1Bfd
              x2_a1Bfe
              x3_a1Bff
              x4_a1Bfg
              x5_a1Bfh
              x6_a1Bfi
              x7_a1Bfj
              x8_a1Bfk
              x9_a1Bfl
              x10_a1Bfm
              x11_a1Bfn
              x12_a1Bfo
              x13_a1Bfp
              x14_a1Bfq)
  = fmap
      (\ y1_a1Bfr
         -> IconConfig
              x1_a1Bfd
              x2_a1Bfe
              x3_a1Bff
              x4_a1Bfg
              x5_a1Bfh
              x6_a1Bfi
              x7_a1Bfj
              x8_a1Bfk
              x9_a1Bfl
              x10_a1Bfm
              x11_a1Bfn
              x12_a1Bfo
              x13_a1Bfp
              y1_a1Bfr)
      (f_a1Bfc x14_a1Bfq)
{-# INLINE iconConfig_elConfig #-}
iconConfig_fitted ::
  forall t_a1Az4. Lens' (IconConfig t_a1Az4) (Active t_a1Az4 Bool)
iconConfig_fitted
  f_a1Bfs
  (IconConfig x1_a1Bft
              x2_a1Bfu
              x3_a1Bfv
              x4_a1Bfw
              x5_a1Bfx
              x6_a1Bfy
              x7_a1Bfz
              x8_a1BfA
              x9_a1BfB
              x10_a1BfC
              x11_a1BfD
              x12_a1BfE
              x13_a1BfF
              x14_a1BfG)
  = fmap
      (\ y1_a1BfH
         -> IconConfig
              x1_a1Bft
              x2_a1Bfu
              y1_a1BfH
              x4_a1Bfw
              x5_a1Bfx
              x6_a1Bfy
              x7_a1Bfz
              x8_a1BfA
              x9_a1BfB
              x10_a1BfC
              x11_a1BfD
              x12_a1BfE
              x13_a1BfF
              x14_a1BfG)
      (f_a1Bfs x3_a1Bfv)
{-# INLINE iconConfig_fitted #-}
iconConfig_flipped ::
  forall t_a1Az4.
  Lens' (IconConfig t_a1Az4) (Active t_a1Az4 (Maybe Flipped))
iconConfig_flipped
  f_a1Bg4
  (IconConfig x1_a1Bg5
              x2_a1Bg6
              x3_a1Bg8
              x4_a1Bg9
              x5_a1Bgb
              x6_a1Bgc
              x7_a1Bgd
              x8_a1Bge
              x9_a1Bgf
              x10_a1Bgg
              x11_a1Bgh
              x12_a1Bgi
              x13_a1Bgj
              x14_a1Bgk)
  = fmap
      (\ y1_a1Bgl
         -> IconConfig
              x1_a1Bg5
              x2_a1Bg6
              x3_a1Bg8
              x4_a1Bg9
              x5_a1Bgb
              x6_a1Bgc
              x7_a1Bgd
              x8_a1Bge
              y1_a1Bgl
              x10_a1Bgg
              x11_a1Bgh
              x12_a1Bgi
              x13_a1Bgj
              x14_a1Bgk)
      (f_a1Bg4 x9_a1Bgf)
{-# INLINE iconConfig_flipped #-}
iconConfig_inverted ::
  forall t_a1Az4. Lens' (IconConfig t_a1Az4) (Active t_a1Az4 Bool)
iconConfig_inverted
  f_a1Bgw
  (IconConfig x1_a1Bgx
              x2_a1Bgy
              x3_a1Bgz
              x4_a1BgA
              x5_a1BgB
              x6_a1BgC
              x7_a1BgD
              x8_a1BgE
              x9_a1BgG
              x10_a1BgI
              x11_a1BgJ
              x12_a1BgK
              x13_a1BgL
              x14_a1BgM)
  = fmap
      (\ y1_a1BgN
         -> IconConfig
              x1_a1Bgx
              x2_a1Bgy
              x3_a1Bgz
              x4_a1BgA
              x5_a1BgB
              x6_a1BgC
              y1_a1BgN
              x8_a1BgE
              x9_a1BgG
              x10_a1BgI
              x11_a1BgJ
              x12_a1BgK
              x13_a1BgL
              x14_a1BgM)
      (f_a1Bgw x7_a1BgD)
{-# INLINE iconConfig_inverted #-}
iconConfig_link ::
  forall t_a1Az4. Lens' (IconConfig t_a1Az4) (Active t_a1Az4 Bool)
iconConfig_link
  f_a1BgT
  (IconConfig x1_a1BgU
              x2_a1BgV
              x3_a1BgW
              x4_a1BgX
              x5_a1BgY
              x6_a1BgZ
              x7_a1Bh0
              x8_a1Bh1
              x9_a1Bh2
              x10_a1Bh3
              x11_a1Bh4
              x12_a1Bh5
              x13_a1Bh6
              x14_a1Bh7)
  = fmap
      (\ y1_a1Bh8
         -> IconConfig
              x1_a1BgU
              x2_a1BgV
              x3_a1BgW
              y1_a1Bh8
              x5_a1BgY
              x6_a1BgZ
              x7_a1Bh0
              x8_a1Bh1
              x9_a1Bh2
              x10_a1Bh3
              x11_a1Bh4
              x12_a1Bh5
              x13_a1Bh6
              x14_a1Bh7)
      (f_a1BgT x4_a1BgX)
{-# INLINE iconConfig_link #-}
iconConfig_loading ::
  forall t_a1Az4. Lens' (IconConfig t_a1Az4) (Active t_a1Az4 Bool)
iconConfig_loading
  f_a1Bhg
  (IconConfig x1_a1Bhh
              x2_a1Bhi
              x3_a1Bhj
              x4_a1Bhk
              x5_a1Bhl
              x6_a1Bhm
              x7_a1Bhn
              x8_a1Bho
              x9_a1Bhq
              x10_a1Bhs
              x11_a1Bht
              x12_a1Bhu
              x13_a1Bhw
              x14_a1Bhz)
  = fmap
      (\ y1_a1BhA
         -> IconConfig
              x1_a1Bhh
              y1_a1BhA
              x3_a1Bhj
              x4_a1Bhk
              x5_a1Bhl
              x6_a1Bhm
              x7_a1Bhn
              x8_a1Bho
              x9_a1Bhq
              x10_a1Bhs
              x11_a1Bht
              x12_a1Bhu
              x13_a1Bhw
              x14_a1Bhz)
      (f_a1Bhg x2_a1Bhi)
{-# INLINE iconConfig_loading #-}
iconConfig_rotated ::
  forall t_a1Az4.
  Lens' (IconConfig t_a1Az4) (Active t_a1Az4 (Maybe Rotated))
iconConfig_rotated
  f_a1BhB
  (IconConfig x1_a1BhC
              x2_a1BhD
              x3_a1BhE
              x4_a1BhF
              x5_a1BhG
              x6_a1BhH
              x7_a1BhI
              x8_a1BhJ
              x9_a1BhK
              x10_a1BhL
              x11_a1BhM
              x12_a1BhN
              x13_a1BhO
              x14_a1BhP)
  = fmap
      (\ y1_a1BhQ
         -> IconConfig
              x1_a1BhC
              x2_a1BhD
              x3_a1BhE
              x4_a1BhF
              x5_a1BhG
              x6_a1BhH
              x7_a1BhI
              x8_a1BhJ
              x9_a1BhK
              y1_a1BhQ
              x11_a1BhM
              x12_a1BhN
              x13_a1BhO
              x14_a1BhP)
      (f_a1BhB x10_a1BhL)
{-# INLINE iconConfig_rotated #-}
iconConfig_size ::
  forall t_a1Az4.
  Lens' (IconConfig t_a1Az4) (Active t_a1Az4 (Maybe Size))
iconConfig_size
  f_a1BhW
  (IconConfig x1_a1BhX
              x2_a1BhY
              x3_a1BhZ
              x4_a1Bi0
              x5_a1Bi1
              x6_a1Bi5
              x7_a1Bi6
              x8_a1Bi7
              x9_a1Bi8
              x10_a1Bi9
              x11_a1Bia
              x12_a1Bib
              x13_a1Bic
              x14_a1Bid)
  = fmap
      (\ y1_a1Bie
         -> IconConfig
              x1_a1BhX
              x2_a1BhY
              x3_a1BhZ
              x4_a1Bi0
              x5_a1Bi1
              x6_a1Bi5
              x7_a1Bi6
              y1_a1Bie
              x9_a1Bi8
              x10_a1Bi9
              x11_a1Bia
              x12_a1Bib
              x13_a1Bic
              x14_a1Bid)
      (f_a1BhW x8_a1Bi7)
{-# INLINE iconConfig_size #-}
iconConfig_title ::
  forall t_a1Az4.
  Lens' (IconConfig t_a1Az4) (Active t_a1Az4 (Maybe Text))
iconConfig_title
  f_a1BiD
  (IconConfig x1_a1BiE
              x2_a1BiF
              x3_a1BiG
              x4_a1BiH
              x5_a1BiJ
              x6_a1BiL
              x7_a1BiM
              x8_a1BiN
              x9_a1BiO
              x10_a1BiP
              x11_a1BiQ
              x12_a1BiR
              x13_a1BiS
              x14_a1BiT)
  = fmap
      (\ y1_a1BiU
         -> IconConfig
              x1_a1BiE
              x2_a1BiF
              x3_a1BiG
              x4_a1BiH
              x5_a1BiJ
              x6_a1BiL
              x7_a1BiM
              x8_a1BiN
              x9_a1BiO
              x10_a1BiP
              x11_a1BiQ
              x12_a1BiR
              y1_a1BiU
              x14_a1BiT)
      (f_a1BiD x13_a1BiS)
{-# INLINE iconConfig_title #-}
-- src/Reflex/Dom/SemanticUI/Icon.hs:162:1-63: Splicing declarations
iconsConfig_elConfig ::
  forall t_a1Bqa.
  Lens' (IconsConfig t_a1Bqa) (ActiveElConfig t_a1Bqa)
iconsConfig_elConfig f_a1Cib (IconsConfig x1_a1Cic x2_a1Cid)
  = fmap
      (\ y1_a1Cie -> IconsConfig x1_a1Cic y1_a1Cie) (f_a1Cib x2_a1Cid)
{-# INLINE iconsConfig_elConfig #-}
iconsConfig_size ::
  forall t_a1Bqa.
  Lens' (IconsConfig t_a1Bqa) (Active t_a1Bqa (Maybe Size))
iconsConfig_size f_a1Cif (IconsConfig x1_a1Cig x2_a1Cih)
  = fmap
      (\ y1_a1Cii -> IconsConfig y1_a1Cii x2_a1Cih) (f_a1Cif x1_a1Cig)
{-# INLINE iconsConfig_size #-}
