-- src/Reflex/Dom/SemanticUI/Header.hs:104:1-64: Splicing declarations
headerConfig_aligned ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (Active t_a2AXc (Maybe Aligned))
headerConfig_aligned
  f_a2B4V
  (HeaderConfig x1_a2B4W
                x2_a2B4X
                x3_a2B4Y
                x4_a2B4Z
                x5_a2B50
                x6_a2B51
                x7_a2B52
                x8_a2B53
                x9_a2B54
                x10_a2B55
                x11_a2B56
                x12_a2B57
                x13_a2B58)
  = fmap
      (\ y1_a2B59
         -> HeaderConfig
              x1_a2B4W
              x2_a2B4X
              x3_a2B4Y
              x4_a2B4Z
              x5_a2B50
              x6_a2B51
              x7_a2B52
              x8_a2B53
              y1_a2B59
              x10_a2B55
              x11_a2B56
              x12_a2B57
              x13_a2B58)
      (f_a2B4V x9_a2B54)
{-# INLINE headerConfig_aligned #-}
headerConfig_attached ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (Active t_a2AXc (Maybe VerticalAttached))
headerConfig_attached
  f_a2B5b
  (HeaderConfig x1_a2B5c
                x2_a2B5d
                x3_a2B5e
                x4_a2B5f
                x5_a2B5g
                x6_a2B5h
                x7_a2B5i
                x8_a2B5j
                x9_a2B5k
                x10_a2B5l
                x11_a2B5m
                x12_a2B5n
                x13_a2B5o)
  = fmap
      (\ y1_a2B5p
         -> HeaderConfig
              x1_a2B5c
              x2_a2B5d
              x3_a2B5e
              x4_a2B5f
              x5_a2B5g
              x6_a2B5h
              x7_a2B5i
              x8_a2B5j
              x9_a2B5k
              x10_a2B5l
              y1_a2B5p
              x12_a2B5n
              x13_a2B5o)
      (f_a2B5b x11_a2B5m)
{-# INLINE headerConfig_attached #-}
headerConfig_block ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (Active t_a2AXc Bool)
headerConfig_block
  f_a2B5r
  (HeaderConfig x1_a2B5s
                x2_a2B5t
                x3_a2B5u
                x4_a2B5v
                x5_a2B5w
                x6_a2B5x
                x7_a2B5y
                x8_a2B5z
                x9_a2B5A
                x10_a2B5B
                x11_a2B5C
                x12_a2B5D
                x13_a2B5E)
  = fmap
      (\ y1_a2B5F
         -> HeaderConfig
              x1_a2B5s
              x2_a2B5t
              x3_a2B5u
              x4_a2B5v
              y1_a2B5F
              x6_a2B5x
              x7_a2B5y
              x8_a2B5z
              x9_a2B5A
              x10_a2B5B
              x11_a2B5C
              x12_a2B5D
              x13_a2B5E)
      (f_a2B5r x5_a2B5w)
{-# INLINE headerConfig_block #-}
headerConfig_color ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (Active t_a2AXc (Maybe Color))
headerConfig_color
  f_a2B5H
  (HeaderConfig x1_a2B5I
                x2_a2B5J
                x3_a2B5K
                x4_a2B5L
                x5_a2B5M
                x6_a2B5N
                x7_a2B5O
                x8_a2B5P
                x9_a2B5Q
                x10_a2B5R
                x11_a2B5S
                x12_a2B5T
                x13_a2B5U)
  = fmap
      (\ y1_a2B5V
         -> HeaderConfig
              x1_a2B5I
              x2_a2B5J
              x3_a2B5K
              x4_a2B5L
              x5_a2B5M
              x6_a2B5N
              x7_a2B5O
              x8_a2B5P
              x9_a2B5Q
              y1_a2B5V
              x11_a2B5S
              x12_a2B5T
              x13_a2B5U)
      (f_a2B5H x10_a2B5R)
{-# INLINE headerConfig_color #-}
headerConfig_disabled ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (Active t_a2AXc Bool)
headerConfig_disabled
  f_a2B5X
  (HeaderConfig x1_a2B5Y
                x2_a2B5Z
                x3_a2B60
                x4_a2B61
                x5_a2B62
                x6_a2B63
                x7_a2B64
                x8_a2B65
                x9_a2B66
                x10_a2B67
                x11_a2B68
                x12_a2B69
                x13_a2B6a)
  = fmap
      (\ y1_a2B6b
         -> HeaderConfig
              x1_a2B5Y
              x2_a2B5Z
              x3_a2B60
              y1_a2B6b
              x5_a2B62
              x6_a2B63
              x7_a2B64
              x8_a2B65
              x9_a2B66
              x10_a2B67
              x11_a2B68
              x12_a2B69
              x13_a2B6a)
      (f_a2B5X x4_a2B61)
{-# INLINE headerConfig_disabled #-}
headerConfig_dividing ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (Active t_a2AXc Bool)
headerConfig_dividing
  f_a2B6d
  (HeaderConfig x1_a2B6e
                x2_a2B6f
                x3_a2B6g
                x4_a2B6h
                x5_a2B6j
                x6_a2B6k
                x7_a2B6l
                x8_a2B6m
                x9_a2B6n
                x10_a2B6o
                x11_a2B6p
                x12_a2B6q
                x13_a2B6r)
  = fmap
      (\ y1_a2B6s
         -> HeaderConfig
              x1_a2B6e
              y1_a2B6s
              x3_a2B6g
              x4_a2B6h
              x5_a2B6j
              x6_a2B6k
              x7_a2B6l
              x8_a2B6m
              x9_a2B6n
              x10_a2B6o
              x11_a2B6p
              x12_a2B6q
              x13_a2B6r)
      (f_a2B6d x2_a2B6f)
{-# INLINE headerConfig_dividing #-}
headerConfig_elConfig ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (ActiveElConfig t_a2AXc)
headerConfig_elConfig
  f_a2B6u
  (HeaderConfig x1_a2B6v
                x2_a2B6w
                x3_a2B6x
                x4_a2B6y
                x5_a2B6z
                x6_a2B6A
                x7_a2B6B
                x8_a2B6C
                x9_a2B6D
                x10_a2B6E
                x11_a2B6F
                x12_a2B6G
                x13_a2B6H)
  = fmap
      (\ y1_a2B6J
         -> HeaderConfig
              x1_a2B6v
              x2_a2B6w
              x3_a2B6x
              x4_a2B6y
              x5_a2B6z
              x6_a2B6A
              x7_a2B6B
              x8_a2B6C
              x9_a2B6D
              x10_a2B6E
              x11_a2B6F
              x12_a2B6G
              y1_a2B6J)
      (f_a2B6u x13_a2B6H)
{-# INLINE headerConfig_elConfig #-}
headerConfig_floated ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (Active t_a2AXc (Maybe Floated))
headerConfig_floated
  f_a2B6K
  (HeaderConfig x1_a2B6M
                x2_a2B6N
                x3_a2B6O
                x4_a2B6P
                x5_a2B6Q
                x6_a2B6R
                x7_a2B6S
                x8_a2B6T
                x9_a2B6U
                x10_a2B6V
                x11_a2B6W
                x12_a2B6X
                x13_a2B6Y)
  = fmap
      (\ y1_a2B6Z
         -> HeaderConfig
              x1_a2B6M
              x2_a2B6N
              x3_a2B6O
              x4_a2B6P
              x5_a2B6Q
              x6_a2B6R
              x7_a2B6S
              y1_a2B6Z
              x9_a2B6U
              x10_a2B6V
              x11_a2B6W
              x12_a2B6X
              x13_a2B6Y)
      (f_a2B6K x8_a2B6T)
{-# INLINE headerConfig_floated #-}
headerConfig_inverted ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (Active t_a2AXc Bool)
headerConfig_inverted
  f_a2B70
  (HeaderConfig x1_a2B71
                x2_a2B72
                x3_a2B73
                x4_a2B74
                x5_a2B75
                x6_a2B76
                x7_a2B77
                x8_a2B78
                x9_a2B79
                x10_a2B7a
                x11_a2B7b
                x12_a2B7c
                x13_a2B7d)
  = fmap
      (\ y1_a2B7e
         -> HeaderConfig
              x1_a2B71
              x2_a2B72
              x3_a2B73
              x4_a2B74
              x5_a2B75
              y1_a2B7e
              x7_a2B77
              x8_a2B78
              x9_a2B79
              x10_a2B7a
              x11_a2B7b
              x12_a2B7c
              x13_a2B7d)
      (f_a2B70 x6_a2B76)
{-# INLINE headerConfig_inverted #-}
headerConfig_largeIcon ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (Active t_a2AXc Bool)
headerConfig_largeIcon
  f_a2B7f
  (HeaderConfig x1_a2B7g
                x2_a2B7h
                x3_a2B7i
                x4_a2B7j
                x5_a2B7k
                x6_a2B7l
                x7_a2B7m
                x8_a2B7n
                x9_a2B7o
                x10_a2B7p
                x11_a2B7q
                x12_a2B7r
                x13_a2B7s)
  = fmap
      (\ y1_a2B7t
         -> HeaderConfig
              y1_a2B7t
              x2_a2B7h
              x3_a2B7i
              x4_a2B7j
              x5_a2B7k
              x6_a2B7l
              x7_a2B7m
              x8_a2B7n
              x9_a2B7o
              x10_a2B7p
              x11_a2B7q
              x12_a2B7r
              x13_a2B7s)
      (f_a2B7f x1_a2B7g)
{-# INLINE headerConfig_largeIcon #-}
headerConfig_preContent ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (Maybe (m_a2AXd ()))
headerConfig_preContent
  f_a2B7u
  (HeaderConfig x1_a2B7v
                x2_a2B7w
                x3_a2B7x
                x4_a2B7y
                x5_a2B7z
                x6_a2B7A
                x7_a2B7B
                x8_a2B7C
                x9_a2B7D
                x10_a2B7E
                x11_a2B7F
                x12_a2B7G
                x13_a2B7H)
  = fmap
      (\ y1_a2B7I
         -> HeaderConfig
              x1_a2B7v
              x2_a2B7w
              x3_a2B7x
              x4_a2B7y
              x5_a2B7z
              x6_a2B7A
              x7_a2B7B
              x8_a2B7C
              x9_a2B7D
              x10_a2B7E
              x11_a2B7F
              y1_a2B7I
              x13_a2B7H)
      (f_a2B7u x12_a2B7G)
{-# INLINE headerConfig_preContent #-}
headerConfig_size ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (Active t_a2AXc (Maybe HeaderSize))
headerConfig_size
  f_a2B7J
  (HeaderConfig x1_a2B7L
                x2_a2B7M
                x3_a2B7N
                x4_a2B7O
                x5_a2B7P
                x6_a2B7Q
                x7_a2B7R
                x8_a2B7S
                x9_a2B7T
                x10_a2B7U
                x11_a2B7V
                x12_a2B7W
                x13_a2B7X)
  = fmap
      (\ y1_a2B7Y
         -> HeaderConfig
              x1_a2B7L
              x2_a2B7M
              x3_a2B7N
              x4_a2B7O
              x5_a2B7P
              x6_a2B7Q
              y1_a2B7Y
              x8_a2B7S
              x9_a2B7T
              x10_a2B7U
              x11_a2B7V
              x12_a2B7W
              x13_a2B7X)
      (f_a2B7J x7_a2B7R)
{-# INLINE headerConfig_size #-}
headerConfig_sub ::
  forall t_a2AXc m_a2AXd.
  Lens' (HeaderConfig t_a2AXc m_a2AXd) (Active t_a2AXc Bool)
headerConfig_sub
  f_a2B7Z
  (HeaderConfig x1_a2B80
                x2_a2B81
                x3_a2B82
                x4_a2B83
                x5_a2B84
                x6_a2B85
                x7_a2B86
                x8_a2B87
                x9_a2B88
                x10_a2B89
                x11_a2B8a
                x12_a2B8b
                x13_a2B8c)
  = fmap
      (\ y1_a2B8d
         -> HeaderConfig
              x1_a2B80
              x2_a2B81
              y1_a2B8d
              x4_a2B83
              x5_a2B84
              x6_a2B85
              x7_a2B86
              x8_a2B87
              x9_a2B88
              x10_a2B89
              x11_a2B8a
              x12_a2B8b
              x13_a2B8c)
      (f_a2B7Z x3_a2B82)
{-# INLINE headerConfig_sub #-}
