import Data.List

main = do 
{-
  putStrLn $ show $ intersperse '.' "MONKEY"
  putStrLn $ show $ intersperse 0 [1,2,3,4,5,6]
  putStrLn $ show $ intercalate [0,0,0] [[1,2,3], [4,5,6], [7,8,9]]
  putStrLn $ show $ intercalate "  " ["Hello,", "there", "Khan"]
  putStrLn $ show $ transpose [[1,2,3],[4,5,6],[7,8,9]]
-}
  -- 3x^2 + 5x + 9, 10x^3 + 9, 8x^3 + 5x^2 + x - 1
  putStrLn $ show $ map sum $ transpose [[0,3,5,9],[10,0,0,9],[0,5,1,-1]]

  putStrLn $ show $ concat [[1,2,3],[3,4,5]]
  putStrLn $ show $ concat ["Hello ", "World"]

  putStrLn $ show $ concat $ concat [[[1,2,3],[4,5,6]],[[1,2,3],[7,8,0]],[[-1,2,-3],[-1,2,-100]]]

  putStrLn $ show $ and $ map (>4) [1,2,3]
  putStrLn $ show $ and $ map (>4) [5,6,7]
  putStrLn $ show $ or $ map (>4) [1,2,3]
  putStrLn $ show $ or $ map (>4) [5,1,1]

  putStrLn $ show $ any (>4) [1,2,3]
  putStrLn $ show $ all  (>4) [5,1,1]

  -- infinite 
  putStrLn $ show $ take 10 $ iterate (*2) 1
  putStrLn $ show $ take 3 $ iterate (++ "Tt") "a"

  putStrLn $ show $ splitAt 3 "Hello World"

  putStrLn $ show $ takeWhile (<4) [-1, 2, 3, 4, 5, 10] 
  putStrLn $ show $ takeWhile (/=' ') "Hello World"
  putStrLn $ show $ dropWhile (/=' ') "Hello World"

  putStrLn $ show $ sort [-1,2,3,40183,124,829,2]
  putStrLn $ show $ group [1,2,3,1,2,3,4,4,5,8]
  putStrLn $ show $ group $ sort $ [1,2,3,1,2,3,4,4,5,8]

  putStrLn $ show $ inits "HELLO WORLD"
  putStrLn $ show $ tails "HELLO WORLD"

  putStrLn $ show $ isInfixOf "cat" "HELLO WORLD"
  putStrLn $ show $ isInfixOf "cat" "im a cat burglar"

  putStrLn $ show $ isPrefixOf "cat" "cat burglar"
  putStrLn $ show $ isPrefixOf "cat" "!cat burglar"

  putStrLn $ show $ isSuffixOf "burglar" "cat burglar"
  putStrLn $ show $ isSuffixOf "burglar!" "cat burglar"

  putStrLn $ show $ elem 1 [2,3,-1,1,2]
  putStrLn $ show $ notElem 1 [2,3,-1,1,2]

  putStrLn $ show $ partition (>3) [1,2,3,4,5]

  putStrLn $ show $ lines "abc\ndef"
  putStrLn $ show $ unlines ["abc", "def"]

  putStrLn $ show $ words "abc def"
  putStrLn $ show $ unwords ["abc", "def"]

  putStrLn $ show $ nub [1,1,1,21,3,1,2,1,4,5,2,1]

  putStrLn$ show $ delete 'h' "hhello world"

  putStrLn $ show $ [1..10] \\ [2,3,4,5]
  putStrLn $ show $ [1..10] `union` [13..20]
  putStrLn $ show $ [1..10] `intersect` [10..20]
  putStrLn $ show $ insert 4 [3,5,1,2]




{-
-- TODO fix 
concatNTimes :: Int -> [[a]] -> [a]
concatNTimes n list 
  | n<=0 = list 
  | otherwise = concatNTimes (n-1) $ concatedList
  where concatedList = concat list 
-}

numUniques:: (Eq a) => [a] -> Int 
numUniques = length . nub
