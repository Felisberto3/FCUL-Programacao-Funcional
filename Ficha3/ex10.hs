halve :: [a] -> ([a], [a])
halve list = (take middle list, drop middle list)
  where
    middle = length list `div` 2

-- UTILIZANDO in

halve2 list =
  let middle = length list `div` 2
      list1 = take middle list
      list2 = drop middle list
   in (list1, list2)
