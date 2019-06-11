doubleMe x = x + x
-- :t doubleMe
-- doubleMe :: Num a => a -> a

doubleUs :: Num a => a -> a -> a
doubleUs x y = x*2 + y*2

doubleUs2 :: Num a => a -> a -> a
doubleUs2 x y = doubleMe x + doubleMe y

doubleSmallNumbers :: (Ord a, Num a) => a -> a
doubleSmallNumbers x = if x > 100 then x else doubleMe x

lucky :: (Integral a) => a -> String
lucky 7 = "I am Lucky"
lucky x = "Not Lucky"

addVectors0 :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors0 x y = ((fst x + fst y), (snd x + snd y))

addVectors1 :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors1 (x1, x2) (y1, y2) = (x1+y1, x2+y2)

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c


listLen :: (Num b) => [a] -> b
listLen [] = 0 
listLen (_:xs) = 1 + listLen xs

sumList :: (Num a) => [a] -> a 
sumList [] = 0 
sumList (x:xs) = x + sumList xs


maxGuard :: (Ord a) => a -> a -> a
maxGuard a b 
  | a > b = a 
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering 
myCompare a b 
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

destructureOrd :: Ordering -> String 
destructureOrd a = case a of 
                    EQ -> "EQ"
                    LT -> "LT"
                    GT -> "GT"

bmiTell1 :: (Ord a, RealFloat a) => a -> String 
bmiTell1 bmi 
  | bmi <= 18.5 = "UnderWeight"
  | bmi <= 25 = "Normal" 
  | bmi <= 30 = "fat"
  | otherwise = "Obese"

bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
  | weight / height ^ 2 <= 18.5 = "Under Weight"
  | weight / height ^ 2 <= 25 = "Normal"
  | weight / height ^ 2 <= 30 = "Fat" 
  | otherwise = "Obese"

bmiTell3 :: (RealFloat a) => a -> a -> String 
bmiTell3 weight height 
  | bmi <= 18.5 = "Under Weight"
  | bmi <= 25 = "Normal" 
  | bmi <= 30 = "fat"
  | otherwise = "Obese"
  where bmi = weight / height ^ 2

bmiTell4 :: (RealFloat a) => a -> a -> String 
bmiTell4 weight height 
  | bmi <= skinny = "Under Weight"
  | bmi <= normal = "normal"
  | bmi <= fat = "fat"
  | otherwise = "obese"
  where bmi = weight / height ^2 
        skinny = 18.5
        normal = 25
        fat = 30

bmiTell5 :: (RealFloat a) => a -> a -> String 
bmiTell5 weight height 
  | bmi <= skinny = "Under Weight"
  | bmi <= normal = "Normal"
  | bmi <= fat = "Fat"
  | otherwise = "Obese"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25, 30)

bmiTell6 :: (RealFloat a) => a -> a -> String 
bmiTell6 weight height 
  | bmi <= fst skinny = snd skinny 
  | bmi <= fst normal = snd normal 
  | bmi <= fst fat = snd fat
  | otherwise = "Obese"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = ((18.5, "Skinny"), (25, "Normal"), (30, "Fat"))

calcBMI :: (RealFloat a) => [(a, a)] -> [a]
calcBMI xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a 
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

main = do 
  putStrLn $ show $ [let square x = x * x in (square 5, square 3, square 2)]
  putStrLn $ show $ (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey"; bar=" there!" in foo ++ bar)
  putStrLn $ show $ (let (a,b,c) = (100, 200, 300) in a+b+c)

calcBMIs :: (RealFloat a) => [(a, a)] -> [a]
calcBMIs xs = [bmi | (w, h) <- xs, let bmi = w/h^2]

calcBMIFat :: (RealFloat a) => [(a, a)] -> [a]
calcBMIFat xs = [bmi | (w,h) <- xs, let bmi = w/h^2, bmi>=25]


myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "No Maximum for Empty list"
myMaximum (x:[]) = x
myMaximum (x:xs) = max x $ myMaximum xs 

replicateThis :: (Num i, Ord i) => i -> a -> [a]
replicateThis i a
  | i <= 0 = []
  | otherwise = a : replicateThis (i-1) a

takeThis :: (Num i, Ord i) => i -> [a] -> [a] 
takeThis _ [] = []
takeThis i (x:xs)
  | i<=0 = []
  | otherwise = x : takeThis (i-1) xs

reverseThis :: [a] -> [a] 
reverseThis [] = []
reverseThis (x:xs) = (reverseThis xs) ++ [x]

repeatThis :: x -> [x]
repeatThis x = x:repeatThis x

zipThis :: [x] -> [y] -> [(x,y)]
zipThis _ [] = []
zipThis [] _ = [] 
zipThis (x:xs) (y:ys) = (x,y):zipThis xs ys

inThis :: (Eq a) => a -> [a] -> Bool 
inThis a [] = False
inThis a (x:xs) 
  | x == a = True 
  | otherwise = inThis a xs


quickSort :: (Ord a) => [a] -> [a] 
quickSort [] = []
quickSort (x:xs) =
  let smallerSorted = quickSort [a | a <- xs, a<=x] 
      biggerSorted = quickSort [a | a <- xs, a>x]
  in smallerSorted ++ [x] ++ biggerSorted

-- todo bubblesort 

-- mergesort
firstHalf :: [a] -> [a]
firstHalf xs = take(div (length xs) 2) xs

secondHalf  :: [a] -> [a] 
secondHalf xs = drop (div (length xs) 2) xs

mergesort :: (Ord a) => [a] -> [a] 
mergesort [] = [] 
mergesort (x:y:[]) = merge [x] [y]
mergesort xs = do
                  let fh = mergesort (firstHalf xs) 
                  let sh = mergesort (secondHalf xs) 
                  merge fh sh 

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
  | x > y = y : merge (x:xs) ys
  | otherwise = x : merge xs (y:ys)

-- selection sort 
-- todo fix [1,-1,2,100,40,15]
selectionsort :: (Ord a) => [a] -> [a] 
selectionsort xs = 
                    let smin = (findMin xs)
                    in smin: (getRidOf smin xs)

findMin :: (Ord a) => [a] -> a
findMin (x:[]) = x
findMin (x:xs) = min x $ findMin xs

-- dont want to remove dublicate element that occur more than once
getRidOf :: (Eq a) => a -> [a] -> [a] 
getRidOf _ [] = [] 
getRidOf y (x:[]) = if y==x then [] else [x]
getRidOf y (x:xs)
  | y==x = xs 
  | otherwise = x:(getRidOf y xs)

-- Higher Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f $ f x

zipFunction :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipFunction _ [] _ = [] 
zipFunction _ _ [] = [] 
zipFunction f (x:xs) (y:ys) = f x y : zipFunction f xs ys

flipIt :: (a -> b -> c) -> (b -> a -> c) 
flipIt f = g
  where g x y = f y x


mapIt :: (a -> b) -> [a] -> [b] 
mapIt _ [] = []
mIt f (x:xs) = f x: mapIt f xs

filterIt :: (a -> Bool) -> [a] -> [a] 
filterIt _ [] = [] 
filterIt f (x:xs) 
  | f x = x : filterIt f xs
  | otherwise = filterIt f xs 
