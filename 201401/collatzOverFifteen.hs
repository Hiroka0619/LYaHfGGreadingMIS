collatzOverFifteen :: [Integer]
collatzOverFifteen = [ head x | x <- filter isGtFifteen (map chain [1..100]) ]
 where isGtFifteen x = length x > 15
chain :: Integer -> [ Integer ]
chain 1 = [1]
chain n
 | even n = n : chain (n `div` 2)
 | odd n = n : chain (n * 3 + 1)
