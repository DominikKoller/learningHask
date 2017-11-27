> import Geography
> import MyMaze

======================================================================

Draw a maze.

***************************************
*              Question 2             *
* Complete the definition of drawMaze *
***************************************

> drawMaze :: Maze -> IO()
> drawMaze maze = putStr (stringifyMaze ((snd.sizeOf) maze) maze)

> stringifyMaze :: Int -> Maze -> String
> stringifyMaze row maze    | row == ((snd.sizeOf) maze) = (drawSWalls (0,row) maze)
>                               ++ (stringifyMaze (row-1) maze)
>                           | row > 0 = (drawWWalls (0,row) maze) 
>                               ++ (drawSWalls (0,row) maze)
>                               ++ (stringifyMaze (row-1) maze)
>                           | otherwise = (drawWWalls (0,row) maze) 
>                               ++ (drawSWalls (0,row) maze)

> drawSWalls :: Place -> Maze -> String
> drawSWalls (col,row) maze | col == widthsize = "+\n"
>                           | (hasWall maze (col,row) S) = "+--" 
>                               ++ drawSWalls (move E (col,row)) maze 
>                           | otherwise = "+  " 
>                               ++ drawSWalls (move E (col,row)) maze 
>                   where (widthsize,_) = sizeOf maze


> drawWWalls :: Place -> Maze -> String
> drawWWalls (col,row) maze | col == widthsize = "|\n"
>                           | (hasWall maze (col,row) W) = "|  " 
>                               ++ drawWWalls (move E (col,row)) maze 
>                           | otherwise = "   " 
>                               ++ drawWWalls (move E (col,row)) maze 
>                       where (widthsize,_) = sizeOf maze

======================================================================

Solve the maze, giving a result of type:

> type Path = [Direction]

***************************************
*            Questions 3--4           *
*     Complete the definition of      *
*              solveMaze              *
***************************************

> solveMaze :: Maze -> Place -> Place -> Path
> solveMaze maze start target = solveMazeIter maze target [(start, [])]

> solve :: Maze -> Path
> solve maze = solveMaze maze (0,0) topRight
>   where topRight = ((\ (x,y) -> (x-1,y-1)) . sizeOf) maze

> fastSolveMaze :: Maze -> Place -> Place -> Path
> fastSolveMaze maze start target = fastSolveMazeIter maze target [(start, [])] []

> fastSolve :: Maze -> Path
> fastSolve maze = fastSolveMaze maze (0,0) topRight
>   where topRight = ((\ (x,y) -> (x-1,y-1)) . sizeOf) maze

> solveMazeIter :: Maze -> Place -> [(Place, Path)] -> Path
> solveMazeIter maze target ((place, path):ps) 
>   | place == target = path
>   | otherwise = solveMazeIter maze target (ps++ explore)
>       where   explore :: [(Place, Path)]
>               explore= map (\ d -> (move d place, path++[d])) . filter (not.hasWall maze place) $ [N,S,W,E]

> fastSolveMazeIter :: Maze -> Place -> [(Place, Path)] -> [Place] -> Path
> fastSolveMazeIter maze target ((place, path):frontier) visited
>   | place == target = path
>   | otherwise = fastSolveMazeIter maze target (frontier++newFrontier) (newVisited++visited)
>       where   newFrontier :: [(Place, Path)]
>               newFrontier = filter (not.wasVisited) . map moveTo . filter (not.hasWall maze place) $ [N,S,W,E]
>               moveTo :: Direction -> (Place, Path)
>               moveTo d = (move d place, path++[d])
>               wasVisited :: (Place, Path) -> Bool
>               wasVisited (place, path) = elem place visited
>               newVisited = map fst newFrontier

> fastSolveMazeIter _ _ [] _ = error "This maze is impossibro"

======================================================================

Some test mazes.  In both cases, the task is to find a path from the bottom
left corner to the top right.

First a small one

> smallMaze :: Maze
> smallMaze = 
>   let walls = [((0,0), N), ((2,2), E), ((2,1),E), ((1,0),E), 
>                ((1,2), E), ((1,1), N)]
>   in makeMaze (4,3) walls

Now a large one.  Define a function to produce a run of walls:

> run (x,y) n E = [((x,y+i),E) | i <- [0..n-1]]
> run (x,y) n N = [((x+i,y),N) | i <- [0..n-1]]

And here is the maze.

> largeMaze :: Maze 
> largeMaze =
>   let walls = 
>         run (0,0) 3 E ++ run (1,1) 3 E ++ [((1,3),N)] ++ run (0,4) 5 E ++
>         run (2,0) 5 E ++ [((2,4),N)] ++ run (1,5) 3 E ++
>         run (1,8) 3 N ++ run (2,6) 3 E ++
>         run (3,1) 7 E ++ run (4,0) 4 N ++ run (4,1) 5 E ++ run (5,2) 3 N ++
>         run (4,6) 2 N ++ run (5,4) 3 E ++ run (6,3) 5 N ++ run (8,0) 4 E ++
>         run (6,1) 3 N ++ run (0,9) 3 N ++ run (1,10) 3 N ++ run (0,11) 3 N ++
>         run (1,12) 6 N ++ run (3,9) 4 E ++ run (4,11) 2 N ++
>         run (5,9) 3 E ++ run (4,8) 3 E ++ run (5,7) 5 N ++ run (6,4) 9 E ++
>         run (7,5) 3 N ++ run (8,4) 4 N ++ run (8,6) 3 N ++ run (10,5) 7 E ++
>         run (9,8) 3 E ++ run (8,9) 3 E ++ run (7,8) 3 E ++ run (8,11) 3 N ++
>         run (0,13) 5 N ++ run (4,14) 2 E ++ run (0,15) 2 E ++ 
>         run (1,14) 3 N ++ run (3,15) 2 E ++ run (0,17) 2 N ++ 
>         run (1,16) 2 E ++ run (2,15) 1 N ++ run (3,16) 3 N ++
>         run (2,17) 2 E ++ run (1,18) 6 N ++ run (4,17) 3 N ++ 
>         run (6,14) 7 E ++ run (5,13) 4 E ++ run (7,12) 2 E ++
>         run (8,13) 3 N ++ run (7,14) 3 N ++ run (10,14) 2 E ++
>         run (8,15) 5 N ++ run (7,16) 5 N ++ run (9,1) 2 E ++
>         run (10,0) 12 N ++ run (21,1) 1 E ++ run (10,2) 2 E ++
>         run (11,1) 7 N ++ run (17,1) 1 E ++ run (11,3) 3 E ++
>         run (12,2) 7 N ++ run (18,2) 2 E ++ run (19,1) 2 N ++
>         run (15,3) 3 N ++ run (14,4) 3 E ++ run (13,3) 3 E ++
>         run (12,4) 3 E ++ run (12,6) 3 N ++ run (11,7) 8 E ++ 
>         run (9,12) 3 N ++ run (12,14) 1 N ++ run (12,8) 10 E ++
>         run (0,19) 6 N ++ run (1,20) 6 N ++ run (7,18) 8 E ++
>         run (8,17) 1 N ++ run (8,18) 3 E ++ run (9,17) 4 E ++ 
>         run (10,18) 2 E ++ run (11,17) 2 E ++ run (10,20) 3 N ++
>         run (11,19) 3 N ++ run (12,18) 2 N ++ run (13,17) 2 N ++
>         run (13,13) 4 E ++ run (14,12) 7 N ++ run (13,11) 2 N ++
>         run (14,10) 2 E ++ run (13,9)2 E ++ run (14,8) 3 N ++ 
>         run (13,7) 3 N ++ run (15,5) 3 E ++ run (16,6) 3 E ++
>         run (18,5) 4 N ++ run (16,4) 2 N ++ run (13,20) 2 E ++
>         run (14,18) 4 E ++ run (20,2) 3 N ++ run (19,3) 2 E ++
>         run (18,4) 2 E ++ run (23,4) 1 E ++ run (22,4) 1 N ++
>         run (21,3) 1 N ++ run (20,4) 2 E ++ run (17,6) 4 N ++ 
>         run (20,7) 2 E ++ run (21,7) 2 N ++ run (21,6) 1 E ++ 
>         run (15,9) 1 E ++ run (17,8) 2 E ++ run (18,7) 2 E ++ 
>         run (19,8) 2 E ++ run (21,9) 1 E ++ run (16,9) 6 N ++
>         run (16,10) 7 N ++ run (15,11) 2 E ++ run (17,11) 5 N ++ 
>         run (14,14) 3 E ++ run (15,15) 6 E ++ run (17,14) 4 E ++
>         run (16,18) 4 E ++ run (15,17) 1 N ++ run (17,17) 3 N ++
>         run (15,13) 7 N ++ run (21,12) 2 E ++ run (16,16) 1 N ++
>         run (16,14) 1 N ++ run (17,15) 3 N ++ run (19,14) 4 N ++
>         run (20,15) 5 E ++ run (19,16) 2 N ++ run (21,16) 5 E ++
>         run (17,19) 2 E ++ run (18,20) 2 E ++ run (19,19) 2 E ++
>         run (18,18) 2 N ++ run (20,20) 3 N
>   in makeMaze (23,22) walls

And now an impossible maze

> impossibleMaze :: Maze
> impossibleMaze =
>   let walls = [((0,1), E), ((1,0),N), ((1,2), E), ((2,1), N)]
>   in makeMaze (3,3) walls

run (x,y) n E = [((x,y+i),E) | i <- [0..n-1]]
run (x,y) n N = [((x+i,y),N) | i <- [0..n-1]]


> snakeMaze :: Size -> Maze
> snakeMaze (x,y) = makeMaze (x,y) walls
>   where walls = [((i,j),N) | i <- [0..x-2], j <- [0,2..y-1]] ++ [((i,j),N) | i <- [1..x-1], j <- [1,3..y-2]]

> emptyMaze :: Size -> Maze
> emptyMaze s = makeMaze s []




Speed comparison: 

Maze represented as > data Maze = Maze Size [Wall]
*Main> fastSolve largeMaze
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.13 secs, 4,671,000 bytes)

Maze represented as > data Maze = Maze Size [Place] [Place] [Place] [Place]
*Main> fastSolve largeMaze
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.04 secs, 4,037,040 bytes)