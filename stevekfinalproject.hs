{-#Language GADTs #-}

--imported so I could use Set Difference "\\"
import Data.List


data Grid where
  N    :: Grid
  E    :: Grid
  W    :: Grid
  S    :: Grid
  deriving (Show,Eq)

--A list of Grids [Grid] is a Point. A maze is a two dimensional array of Points.
newtype Maze = Maze [[[Grid]]]
  
instance Show Maze where
  show (Maze (x:xs))  =
    " " ++ (concat (take ((length x) -1) $ repeat "__")) ++ "_"++ "\n" ++
        mazeShow (Maze (totalNorthWestRemove (x:xs)))
                   where totalNorthWestRemove [] = []
                         totalNorthWestRemove (x:xs) = (map northWestRemove x) :
                                                     totalNorthWestRemove xs
                      
--removes the Norths and Wests so not to double mark blocks
northWestRemove :: [Grid] -> [Grid]
northWestRemove []        = []
northWestRemove (N:xs)   =       northWestRemove xs
northWestRemove (E:xs)    =  E : northWestRemove xs
northWestRemove (W:xs)    =      northWestRemove xs
northWestRemove (S:xs)    =  S : northWestRemove xs
                      
mazeShow :: Maze -> String
mazeShow (Maze [])   = ""
mazeShow (Maze (x:xs)) = "|" ++ lineShow x ++ "\n" ++ mazeShow (Maze xs) 

lineShow :: [[Grid]] -> String
lineShow []     = ""
lineShow ([]:xs) = "  " ++ lineShow xs
lineShow ([E]:xs) = " |" ++ lineShow xs
lineShow ([S]:xs) = "__" ++ lineShow xs
lineShow ([E,S]:xs) = "_|" ++ lineShow xs
lineShow ([S,E]:xs) = "_|" ++ lineShow xs

--starting position array, beginning a throwaway point to align the indecies
points = [((0,0),(0,0)),((1,2),(1,0)),((2,1),(0,1)),((1,1),(1,0)),
          ((1,0),(4,1)),((2,0),(2,3)),((0,0),(4,2)),((4,4),(0,4)),
          ((0,0),(2,2)),((8,0),(0,0)),((1,1),(0,0))]
mazes = [maze1, maze1,maze2,maze3,maze4,maze5,maze6,maze7,maze8,maze9,maze10]
                      
--Maze 1, 3x3 on http://www.logicmazes.com/theseus.html           
maze1 = Maze [[[N,W], [N,S], [N,E]], [[W], [N,E,S], [W]], [[W,S], [N,S], [E,S]]]
--Maze 2, 7x4
maze2 = Maze [[[N,W],[N],[N],[N],[N],[N],[N,E]],
              [[E,W], [E,W,S],[W],[],[],[],[E]],
              [[W],[N],[],[],[E],[E,W,S],[E,W]],
              [[W,S],[],[S],[S],[S],[S],[E,S]]]
--Maze 3, 3x4 (needs delay)
maze3 = Maze[[[N,W],[N,S],[N,E]],
             [[W],[N],[]],
             [[E,W,S],[W],[E]],
             [[N,S,W],[S],[S,E]]]
--Maze 4, 5x5
maze4 = Maze [[[N,E,W],[N,W],[N],[S],[N,E]],
              [[W],[S,E],[W],[N,E],[W,E]],
              [[W],[N,S],[S,E],[W],[E]],
              [[W],[N],[N,E],[E,S,W],[W,E]],
              [[W,S],[S],[S],[N,S],[S,E]]]
--Maze 5, 7x5
maze5 = Maze[[[N,W],[N],[N],[N,E],[N,W],[S,E],[N,E,W]],
             [[W,E],[W,S],[],[E],[W],[N,S],[E]],
             [[W],[N],[E,S],[W],[],[N,E,S],[W,E]],
             [[W],[E],[N,W,S],[S,E],[E,W,S],[N,W],[E]],
             [[S,W],[S],[N,S],[N,S],[N,S],[S],[S,E]]]
--Maze 6, 6x4
maze6 = Maze [[[N,W,S],[N],[N],[N],[E],[N,E,W]],
              [[N,W],[S,E],[W],[E],[E,S,W],[E,W]],
              [[W,E],[N,E,W],[E,W,S],[W],[N,S],[E]], 
              [[W,E],[W],[N,S],[E],[N,W],[E]],
              [[W,E],[W,E],[W,N],[],[],[E]],
              [[W,S],[S],[S],[S],[S],[E,S]]]
--Maze 7, 6x6
maze7 :: Maze
maze7 = Maze [[[N,W],[S],[N],[N],[N,S,E],[N,E,W]],
              [[W],[N],[E],[W],[N,S],[E]],
              [[W],[],[E],[W],[N,E],[W,E]],
              [[S,E,W],[W],[],[E],[W,E,S],[W,E]],
              [[N,W],[],[S],[],[N,E],[W,E]],
              [[S,W],[S],[N,S],[S],[S],[S,E]]]
--Maze 8, 9x8
maze8 = Maze  [[[N,W,S],[N,S],[N],[N,E],[N,W],[N,E],[N,W],[N,S],[N,E]],
               [[N,W],[N,E],[S,E,W],[W,E],[W,E],[W,E],[W],[S,E,N],[W,E]],
               [[W,E],[W,S],[N,E],[W],[S,E],[W,S],[S,E],[N,W],[E]],
               [[S,W],[N,E],[W,E],[W,S],[N,E],[N,W],[N,S],[S],[S,E]],
               [[N,E,W],[W,E],[W,S],[N],[S,E],[W,S],[N,S],[N,S],[N,E]],
               [[W],[S,E],[N,W],[S,E],[N,W],[N],[N,S],[N,S],[S,E]],
               [[W,E],[N,W,E],[W],[N,S,E],[W,E],[W,S],[N,S],[N,S],[N,E]],
               [[S,W,E],[W],[S],[N,S],[S],[N,S,E],[W,S,N],[N,S],[S,E]]]
--Maze 9, 9x8
maze9 = Maze [[[N,S,W],[N,S],[N],[],[N],[N,S],[N],[N],[N,E]],
              [[N,W],[N],[],[],[],[N],[],[],[E]],
              [[W],[],[],[S],[],[],[],[],[E]],
              [[W,E],[S,W],[E],[N,E,W],[W],[],[],[E],[W,E]],
              [[W,E],[N,W],[],[],[],[],[],[E,S],[W,E]],
              [[W,E],[W],[],[],[],[],[],[N,E],[W,E]],
              [[W],[],[],[S],[],[],[],[E],[W,E]],
              [[W,S],[S],[S],[N,S],[S],[S],[S],[S],[S,E]]]
--Maze 10, 8x8
maze10 = Maze [[[N,E],[W,N],[N],[N],[N],[N,S],[N],[N,E,S]],
              [[W],[],[],[],[],[N],[E],[N,W,E]],
              [[W],[],[],[],[E],[W],[],[E]],
              [[W],[S],[],[E],[W],[S],[],[E]],
              [[W],[N],[E],[W],[],[N],[],[E]],
              [[E,W,S],[W,E],[W],[],[],[E],[W,S],[E]],
              [[W,N,S],[E],[W],[E],[W,S],[],[N],[E]],
              [[N,W,S],[S],[S],[S],[N,E,S],[S,W],[S],[S,E]]]


--takes in a touple of thesus's and the minotaur's positions and returns
--a touple with the minotaurs location updated once (needs to be run twice)
minotaur :: Maze -> ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
minotaur maze (t@(tX,tY),m@(mX,mY)) | tX < mX && (elem (mX-1,mY) possible)
                                 =(t,(mX-1,mY))
                               | tX > mX && (elem (mX+1,mY) possible)
                                 =(t,(mX+1,mY))
                               | tY < mY && (elem (mX,mY-1) possible)
                                 =(t,(mX,mY-1))
                               | tY > mY && (elem (mX,mY+1) possible)
                                 =(t,(mX,mY+1))
                               | otherwise = (t,m)
                               where possible = movesAndDelay maze m

--puts it all together                                                
ariadne :: Maze -> ((Int, Int),(Int,Int)) -> [[((Int,Int),(Int,Int))]]
ariadne m x = ariadne' m (concat (map (ballOString m) [[x]]))
  where ariadne' :: Maze -> [[((Int, Int),(Int,Int))]]  -> [[((Int,Int),(Int,Int))]]
        ariadne' m [] = error "Could not find a solution"
        ariadne' m@(Maze (p:ps)) l@(((x,y):xs):xss)
          | ((hasTheseusSkippedTown m l) /= []) = hasTheseusSkippedTown m l
          | otherwise =  ariadne' m(loseTheLoses (concat (map (ballOString m) l')))
          where l' = (rebuild (loseTheLoses ( minotaurAttacks m l)) [])


--the minotaur moves two spaces towards Theseus on all possibilities
minotaurAttacks :: Maze -> [[((Int, Int),(Int,Int))]]  -> [[((Int,Int),(Int,Int))]]
minotaurAttacks m l = map (\list -> (minotaur m (head list)) : list)
  (map (\list -> (minotaur m (head list)) : list) l)
                      

--checks if a solution has been found yet. the empty list stands for "not yet"
--and it returns all optimal solutionsk, if any found yet
hasTheseusSkippedTown :: Maze -> [[((Int,Int),(Int,Int))]] -> [[((Int,Int),(Int,Int))]]
hasTheseusSkippedTown m [] = []
hasTheseusSkippedTown maze@(Maze (p:ps)) ((((x,y),m):xs):xss)
  | (x < 0 || x >= (length p))  = reverse (((x,y),m):xs) : hasTheseusSkippedTown maze xss
  | (y < 0 || y >  (length ps)) = reverse (((x,y),m):xs) : hasTheseusSkippedTown maze xss
  | otherwise                   = hasTheseusSkippedTown maze xss

--continue each attempt moving Theseus one move foward, whie making sure not
--to repeat exact arrangements in a specific solution
ballOString :: Maze ->  [((Int,Int),(Int,Int))] -> [[((Int,Int),(Int,Int))]]
ballOString  m = (\list -> (map (\move -> (move,(snd (head list))) : list)
                                  ((movesAndDelay m (fst (head list))) \\(map
                                  fst (removeDoubles  (head list) (check (head list) list)) )) ))
  where check pos list= if (pos == (minotaur m (minotaur m pos))) then list else
			  tail list
	removeDoubles r [] = []
        removeDoubles r (x:xs) | snd r == snd x =  x: removeDoubles r xs
                               | otherwise = removeDoubles r xs
                                             
--returns all possible moves (including the delay)      
movesAndDelay :: Maze -> (Int, Int) -> [(Int,Int)]
movesAndDelay (Maze m) (x,y) = decodeGrid  (m!!y!!x) (x,y) [(x-1,y),(x+1,y),(x,y-1),(x,y+1),(x,y)]
  where decodeGrid []     (x,y) given = given
        decodeGrid (p:ps) (x,y) given
          |p==N    = decodeGrid ps (x,y) given \\ [(x,y-1)]
          |p==E    = decodeGrid ps (x,y) given \\ [(x+1,y)]
          |p==W    = decodeGrid ps (x,y) given \\ [(x-1,y)]
          |p==S    = decodeGrid ps (x,y) given \\ [(x,y+1)]

--get rids of the solutions in which Theseus dies
loseTheLoses :: [[((Int,Int),(Int,Int))]] -> [[((Int,Int),(Int,Int))]]
loseTheLoses []                       = []
loseTheLoses ((((tX,tY),(mX,mY)):xs):xss)
  | tX == mX && tY == mY = loseTheLoses xss
  | otherwise            = (((tX,tY),(mX,mY)):xs) :loseTheLoses xss

--convert from moves into readable text
wrapUpTheYarn :: [[((Int,Int),(Int,Int))]] -> [[[Char]]]
wrapUpTheYarn l = map (wrapUpTheYarn' 1 (fst (head (head l))))
                  (map (\list -> (map fst) list) l)
                    where wrapUpTheYarn' :: Int -> (Int,Int) -> [(Int,Int)] -> [[Char]]
                          wrapUpTheYarn' c p [] = []
                          wrapUpTheYarn' c p l
                            | c == 0 = (wrapUpTheYarn' (c+1) p (tail l))
                            | c == 1 = (wrapUpTheYarn' (c+1) p (tail l))
                            | c == 2 = (transisition p (head l)) : (wrapUpTheYarn' 0 (head l) (tail l))
                            where transisition (aX,aY) (bX,bY)
                                     | bX > aX = "Right"
                                     | bX < aX = "Left"
                                     | bY > aY = "Down"
                                     | bY < aY = "Up"
                                     | otherwise = "Delay"

--Diana's optimization to get rid of converging solutions
rebuild :: (Eq a) => [[a]] -> [[a]] -> [[a]]
rebuild [] b = b
rebuild ((a:as):ass)  b | (contains a b) = rebuild ass b
                        | otherwise = rebuild ass ((a:as):b)
                        where contains :: (Eq a) => a -> [[a]] -> Bool
                              contains a [] = False
                              contains a (x:xs) = if (a==(head x)) then True else contains a xs

--makes the output from ariadne reaable and takes the maze # from the puzzle as input
solve n = wrapUpTheYarn (ariadne (mazes!!n) (points!!n))
