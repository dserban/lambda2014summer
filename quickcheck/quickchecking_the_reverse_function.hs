import Test.QuickCheck

propertyOfReverse :: [Int] -> [Int] -> Bool
propertyOfReverse xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

main = quickCheck propertyOfReverse

