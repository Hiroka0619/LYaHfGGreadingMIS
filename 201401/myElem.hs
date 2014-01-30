elem' :: Eq a => a -> [a] -> Bool
elem' a xs = foldl (\acc x -> if a == x then True else acc) False xs
