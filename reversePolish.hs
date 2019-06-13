import Data.List 

solveRPN :: (Num a, Read a) => String -> a 
solveRPN expression = head $ foldl foldingFunction [] (words expression)

foldingFunction :: (Num a, Read a) => [a] -> String -> [a]
foldingFunction (x:y:xs) "*" = (x*y):xs
foldingFunction (x:y:xs) "+" = (x+y):xs
foldingFunction (x:y:xs) "-" = (x-y):xs
foldingFunction (x:y:xs) "/" = (x/y):xs
foldingFunction (x:y:xs) "^" = (x**y):xs
foldingFunction (x:xs) "ln" = (log x):xs
foldingFunction (xs) "sum" = [sum xs] 
foldingFunction stack numberString = read numberString:stack
