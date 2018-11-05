> module MyMaze (
>   Maze,
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall, -- :: Maze -> Place -> Direction -> Bool
>   sizeOf -- :: Maze -> Size
> )
> where

> import Geography

> data Maze = Maze Size [Place] [Place] [Place] [Place]

> sizeOf :: Maze -> Size
> sizeOf (Maze size _ _ _ _) = size

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (Maze _ placesN placesS placesE placesW) pos N = pos `elem` placesN
> hasWall (Maze _ placesN placesS placesE placesW) pos S = pos `elem` placesS
> hasWall (Maze _ placesN placesS placesE placesW) pos E = pos `elem` placesE
> hasWall (Maze _ placesN placesS placesE placesW) pos W = pos `elem` placesW

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls = Maze (x,y) placesN placesS placesE placesW
>     where boundaries = -- the four boundaries
>               [((0,j), W) | j <- [0..y-1]] ++ -- westerly boundary
>               [((x-1,j), E) | j <- [0..y-1]] ++ -- easterly boundary
>               [((i,0), S) | i <- [0..x-1]] ++ -- southerly boundary
>               [((i,y-1), N) | i <- [0..x-1]] -- northerly boundary
>           allWalls = walls ++ boundaries ++ map reflect (walls ++ boundaries)
>           placesN = map fst (filter ((==N).snd) allWalls)
>           placesS = map fst (filter ((==S).snd) allWalls)
>           placesE = map fst (filter ((==E).snd) allWalls)
>           placesW = map fst (filter ((==W).snd) allWalls)
