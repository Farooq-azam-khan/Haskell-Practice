data TrafficLight = Red | Yellow | Green 

instance Eq TrafficLight where
  Red == Red = True 
  Green == Green = True 
  Yellow == Yellow = True 
  _ == _ = False 

instance Show TrafficLight where 
  show Red = "Red Light"
  show Green = "Green Light"
  show Yellow = "Yellow Light" 

class YesNo a where 
  yesno :: a -> Bool 

instance YesNo Int where 
  yesno 0 = False 
  yesno _ = True 

instance YesNo [a] where 
  yesno [] = False 
  yesno _ = True 

instance YesNo Bool where 
  yesno = id 
  {-
  yesno True = True 
  yesno False = False 
  -}
instance YesNo (Maybe a) where 
  yesno (Just _) = True 
  yesno Nothing = False 
  


