import qualified Data.Map as Map

phoneBook = [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]
fibNs = [(0,1), (1,1), (1,2)]

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing 
findKey key ((k,v):xs) = if key == k 
                            then Just v
                            else findKey key xs

findKeyFold :: (Eq k) => k -> [(k,v)] -> Maybe v
findKeyFold key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing 

mapPhoneBook associatedList = Map.fromList associatedList

emptyMap = Map.empty

main = do 
  -- insert 
  putStrLn $ show $ Map.insert 3 100 Map.empty
  putStrLn $ show $ Map.insert 5 10 $ Map.insert 100 1 $ Map.insert 3 100 Map.empty

  -- check for null
  putStrLn $ show $ Map.null Map.empty 
  -- size 
  putStrLn $ show $ Map.size Map.empty
  -- singleton 
  putStrLn $ show $ Map.singleton 3 9

  -- member: checks for key in map 
  putStrLn $ show $ Map.member 3 $ Map.fromList [(3,5)]

  -- Map.map 
  putStrLn $ show $ Map.map (*100) $ Map.fromList [(1,1), (2,3), (4,100)]
  -- Map.filter
  putStrLn $ show $ Map.filter (==20) $ Map.fromList [(1,20), (2,2), (3,10)]

  -- Map.toList : converts map to list 
  putStrLn $ show $ Map.toList $ Map.singleton 3 10

  -- Map.elems
  putStrLn $ show $ Map.elems $ Map.fromList [(1,2), (2,3), (4,5)]
  -- Map.keys
  putStrLn $ show $ Map.keys $ Map.fromList [(1,2), (2,3), (4,5)]

  -- lookup 
  putStrLn $ show $ Map.lookup "patsy" $ phoneBookToMap phoneBook2

  putStrLn $ show $ phoneBookToMap2 phoneBook2

  putStrLn $ show $ Map.fromListWith max [(2,3), (2,5), (1,100), (20, 35), (50, 200), (50,2)]

  putStrLn $ show $ Map.insertWith (+) 3 100 $ Map.fromList [(3, 100)]


myFromList :: (Ord k) => [(k,v)] -> Map.Map k v
myFromList = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

phoneBook2 =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]  

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String 
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs 

phoneBookToMap2 :: (Ord k) => [(k,a)] -> Map.Map k [a] 
phoneBookToMap2 xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
