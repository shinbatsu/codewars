import Data.List
snail x|null x||null(head x)=[]|otherwise=head x++snail(
  reverse $ transpose (tail x)
  )