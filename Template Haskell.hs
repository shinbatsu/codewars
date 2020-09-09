{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
tuple n=mapM(newName.("a"++).show)[1..n]>>= \a->lamE(map varP a)(if n==1 then varE(head a)else tupE(map varE a))