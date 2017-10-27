data Color = Red | Green | Blue | Yellow | Orange | White deriving (Show)

data Face = Face {  _00 :: Color  
                  , _01 :: Color 
                  , _02 :: Color
                  , _10 :: Color 
                  , _11 :: Color 
                  , _12 :: Color  
                  , _20 :: Color  
                  , _21 :: Color  
                  , _22 :: Color  
                  } deriving (Show)

data Cube = Cube {  top :: Face
                  , bottom :: Face
                  , left :: Face
                  , right :: Face
                  , front :: Face
                  , back :: Face
                 } deriving (Show)

uniColorFace :: Color -> Face
uniColorFace c = Face c c c c c c c c c

solvedCube = Cube 
                (uniColorFace Red) 
                (uniColorFace Green)
                (uniColorFace Blue)
                (uniColorFace Yellow)
                (uniColorFace Orange)
                (uniColorFace White)

rotate :: Cube -> Cube
rotate Cube top
         bottom
         left
         right
         front
         back
                
