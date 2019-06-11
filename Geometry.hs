module Geometry (
                  sphereVolume, 
                  sphereArea, 
                  cubeVolume, 
                  cubeArea, 
                  cuboidVolume, 
                  cuboidArea

) where 

sphereVolume :: Float -> Float 
sphereVolume radius = (4.0/3.0) * pi * radius * radius * radius 

sphereArea :: Float -> Float 
sphereArea radius = 4 * pi * radius * radius 

rectangleArea :: Float -> Float -> Float 
rectangleArea a b = a * b 

cuboidArea :: Float -> Float -> Float -> Float 
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2 

cuboidVolume :: Float -> Float -> Float -> Float 
cuboidVolume a b c = rectangleArea a b * c 

cubeArea :: Float -> Float 
cubeArea side = cuboidArea side side side 

cubeVolume :: Float -> Float 
cubeVolume side = cuboidVolume side side side 



