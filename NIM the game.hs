import Data.Bits;
chooseMove::[Int]->(Int,Int)
chooseMove a=let t=foldl xor 0 a in head[(i,v-xor t v)|(i,v)<-zip[0..]a,v>xor t v]