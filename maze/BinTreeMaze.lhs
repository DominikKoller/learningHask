Module to define the type of a maze

> module BinTreeMaze (
>   Maze, 
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf    -- :: Maze -> Size
> )
> where

> import Geography
> import Data.List

> data BinTree a = EndNode | Node a (BinTree a) (BinTree a) deriving Show

> treeFromList :: Ord a => [a] -> BinTree a
> treeFromList xs = treeFromSortedList . sort $ xs

> treeFromSortedList [] = EndNode
> treeFromSortedList xs = Node r (treeFromSortedList left) (treeFromSortedList right)
>   where (left, r:right) = splitAt (length xs `div` 2) xs

> isIn :: Ord a => a -> BinTree a -> Bool
> isIn _ EndNode = False
> isIn x (Node n left right) | x == n = True
>                            | x >= n = isIn x right
>                            | otherwise = isIn x left

> tInsert :: Ord a => a -> BinTree a -> BinTree a
> tInsert x EndNode = Node x EndNode EndNode
> tInsert x (Node n left right) | x == n = Node n left right -- no duplicates
>                               | x >= n = Node n left (tInsert x right)
>                               | otherwise = Node n (tInsert x left) right

We will represent a maze by its size and a list of its walls.

> data Maze = Maze Size (BinTree Place) (BinTree Place) (BinTree Place) (BinTree Place)

The list of walls will be complete in the sense that we record
both sides of the wall; for example, if the list includes 
((3,4), N), then it will also include ((3,5),S).

This function creates a maze given its size and a list of walls; 
the list of walls might not be complete in the above sense.

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls = Maze (x,y) (treeFromList northWalls) (treeFromList southWalls) (treeFromList eastWalls) (treeFromList westWalls)
>   where northBound =  [(i  ,y-1) | i <- [0..x-1]]
>         southBound =  [(i  ,0  ) | i <- [0..x-1]]
>         eastBound =   [(x-1,j  ) | j <- [0..y-1]]
>         westBound =   [(0  ,j  ) | j <- [0..y-1]]
>         innerWalls = walls ++ map reflect (walls)
>         (northWalls, southWalls, eastWalls, westWalls) = dealWalls (northBound, southBound, eastBound, westBound) innerWalls

DealWalls takes a 4-Tuple of Places representing walls in N S E W directions, new walls, and the new tuple to which the additional walls have been 'dealt'

> dealWalls :: ([Place], [Place], [Place], [Place]) -> [Wall] -> ([Place], [Place], [Place], [Place])
> dealWalls = foldr split
>   where   split :: Wall -> ([Place], [Place], [Place], [Place]) -> ([Place], [Place], [Place], [Place])
>           split (place, dir) (n,s,e,w) | dir == N = (place:n,s,e,w)
>                                        | dir == S = (n,place:s,e,w)
>                                        | dir == E = (n,s,place:e,w)
>                                        | dir == W = (n,s,e,place:w)

The following function "reflects" a wall; i.e. gives the representation as
seen from the other side; for example, reflect ((3,4), N) = ((3,5),S)

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

The following function tests whether the maze includes a wall in a particular
direction from a particular place:

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (Maze _ n _ _ _ ) pos N = pos `isIn` n
> hasWall (Maze _ _ s _ _ ) pos S = pos `isIn` s
> hasWall (Maze _ _ _ e _ ) pos E = pos `isIn` e
> hasWall (Maze _ _ _ _ w ) pos W = pos `isIn` w

The following function returns the size of a maze:

> sizeOf :: Maze -> Size
> sizeOf (Maze size _ _ _ _ ) = size