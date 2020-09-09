data User = User { rank :: Int, progress :: Int }
instance Show User where
    show (User r p) = "User { rank = " ++ show r ++ ", progress = " ++ show p ++ " }"
    
newUser :: User
newUser = User (-8) 0

gain :: Int -> User -> User
gain d u = u { progress = progress u + g }
  where g = if d > 0 then 10 * d ^ 2 else if d == 0 then 3 else if d == -1 then 1 else 0

promote :: User -> User
promote (User 8 _) = User 8 0
promote u@(User r p)
  | p < threshold = u
  | r + 1 == 0 = promote $ User 1 (p - threshold)
  | otherwise = promote $ User (r + 1) (p - threshold)
  where 
    threshold = 100

incProgress :: Int -> User -> User
incProgress k u@(User r _)
  | bad k || bad r = error "Exception"
  | otherwise = promote $ gain d u
  where
    d = if signum k /= signum r then signum k * (abs (k - r) - 1) else k - r
    bad x = x < -8 || x > 8 || x == 0