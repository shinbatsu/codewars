import TreeByLevels.TreeNode
treeByLevels t = let
  f a = case a of
    ([], []) -> []
    ([], b) -> f (reverse b, [])
    (y:b, back) -> case y of
      Nothing -> f (b, back)
      Just (TreeNode {value=v,left=l,right=r})->v:f(b,r:l:back)
  in f ([t], [])