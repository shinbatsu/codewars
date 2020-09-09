class X a r | r->a where k::[a]->r
instance X a r => X a (a->r) where k l x=k(l++[x])
instance X a [a] where k=id
polyList :: X a r => r
polyList = k []
class Y a where f::Int->a
instance (Integral x, Y r, Enum x) => Y (x -> r) where f n x = f (n + fromEnum x)
instance Y Int where f=id
polyAdd :: Y r => r
polyAdd = f 0
class Z a where h::String->a
instance Z r => Z (String->r) where h s x = h (unwords [s, x])
instance Z String where h ""="" ; h xs=tail xs
polyWords :: Z a => a
polyWords = h ""