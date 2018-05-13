myLast :: Eq a => [a] -> a
myLast [] = error "Empty List"
myLast (x:xs)
  | xs == [] = x
  | otherwise = myLast xs


myButLast :: Eq a => [a] -> a
myButLast [] = error "Empty List"
myButLast (x:xs)
  | (length xs) == 1 = x
  | otherwise = myButLast xs

myButLast'' [x,_] = x
myButLast'' (_:xs) = myButLast'' xs

myLength :: [a] -> Int
myLength = foldl (\n _ -> n + 1) 0
