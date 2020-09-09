import qualified Data.Map.Strict as M
import qualified Data.Set as S
import ScreenLockingPatterns.Shared (Vertex(..))

type BridgeMap = M.Map Vertex (M.Map Vertex Vertex)
type NeighbourMap = M.Map Vertex (S.Set Vertex)
bm = M.fromList
  [ (A, M.fromList [(B, C), (E, I), (D, G)])
  , (B, M.fromList [(E, H)])
  , (C, M.fromList [(B, A), (E, G), (F, I)])
  , (D, M.fromList [(E, F)])
  , (E, M.empty)
  , (F, M.fromList [(E, D)])
  , (G, M.fromList [(D, A), (E, C), (H, I)])
  , (H, M.fromList [(E, B)])
  , (I, M.fromList [(E, A), (F, C), (H, G)])
  ]
nm = M.fromList
  [ (A, S.fromList [B, D, E, F, H])
  , (B, S.fromList [A, C, D, E, F, G, I])
  , (C, S.fromList [B, D, E, F, H])
  , (D, S.fromList [A, B, C, E, G, H, I])
  , (E, S.fromList [A, B, C, D, F, G, H, I])
  , (F, S.fromList [A, B, C, E, G, H, I])
  , (G, S.fromList [B, D, E, F, H])
  , (H, S.fromList [A, C, D, E, F, G, I])
  , (I, S.fromList [B, D, E, F, H])
  ]

countPatternsFrom :: Vertex -> Int -> Int
countPatternsFrom _ l | l < 1 || l > 10 = 0
countPatternsFrom s 1 = 1
countPatternsFrom s len = dfs s (len - 1) S.empty
  where
    dfs :: Vertex -> Int -> S.Set Vertex -> Int
    dfs _ 0 _ = 1
    dfs current rem visited =
      let visited' = S.insert current visited
          directNext = filter (`S.notMember` visited)
                     $ S.toList (nm M.! current)
          indirectNext = [ dst
                         | (via, dst) <- M.toList (bm M.! current)
                         , S.member via visited
                         , not (S.member dst visited)
                         ]
          allNext = directNext ++ indirectNext
      in sum [ dfs next (rem - 1) visited' | next <- allNext ]