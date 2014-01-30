data Shape = Circle Float Float Float |
             Rectangle Float Float Float Float
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 x2 y1 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
