import Data.Bits

mystery,mysteryInv::Int->Int

mystery y=xor y(div y 2)
mysteryInv n=foldl xor n(takeWhile(>0)(iterate(`div`2)(div n 2)))
nameOfMystery= if False then"GRAY" else "Gray code"