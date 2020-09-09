import Data.Bits; import Preloaded (Parity(EVEN,ODD))

subsetsParity :: Int -> Int -> Parity
subsetsParity n k|complement n.&.k==0=ODD|0<1=EVEN