longestSlideDown p=head $ foldr1 s p
  where
    s u l=zipWith(+)u(zipWith max l(tail l))