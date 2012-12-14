
{-#Language GADTs #-}
--import qualified Data.Map as Map
import Debug.Trace
import Data.List
  
--A failed first attempt to represent Grid Data
{-
--where input to Space in N-E-W-S order
data Grid where
  Null  :: Grid
  Space :: Grid ->  Grid -> Grid -> Grid -> Grid
  Win   ::  Grid
           deriving Show

north Null            = Null
north Win             = Null
north (Space n _ _ _) = n

east Null            = Null
east Win             = Null                        
east (Space _ e _ _) = e

west Null            = Null
west Win            = Null                       
west (Space _ _ w _) = w                          

south Null            = Null
south Win             = Null
south (Space _ _ _ s) = s

--spread x y from top left to bottom right            
spread 0 0 = Null
spread 1 1 = Space Null Null             Null Null
spread x 1 = Space Null (spread (x-1) 1) Null Null
spread 1 y = Space Null Null             Null (spread 1 (y-1))
spread x y = Space Null (spread (x-1) 1) Null (spread 1 (y-1))
-}

data Grid where
  N    :: Grid
  NE   :: Grid
  NEW  :: Grid
  NEWS :: Grid
  NES  :: Grid
  NW   :: Grid
  NWS  :: Grid
  NS   :: Grid
  E    :: Grid
  EW   :: Grid
  EWS  :: Grid
  ES   :: Grid
  W    :: Grid
  WS   :: Grid
  S    :: Grid
  NONE :: Grid
  deriving (Show,Eq)

newtype Maze = Maze [[Grid]]
  
instance Show Maze where
  show (Maze (x:xs))  =
    " " ++ (concat (take ((length x) -1) $ repeat "__")) ++ "_"++ "\n" ++
        mazeShow (Maze (map northWestRemove (x:xs)))
                      
--removes the Norths and Wests so no to double mark blocks
northWestRemove :: [Grid] -> [Grid]
northWestRemove []        = []
northWestRemove (N:xs)    = NONE : northWestRemove xs
northWestRemove (NE:xs)   = E : northWestRemove xs
northWestRemove (NEW:xs)  = E : northWestRemove xs
northWestRemove (NEWS:xs) = ES : northWestRemove xs
northWestRemove (NES:xs)  = ES : northWestRemove xs
northWestRemove (NW:xs)   = NONE : northWestRemove xs
northWestRemove (NWS:xs)  = S : northWestRemove xs
northWestRemove (NS:xs)   = S : northWestRemove xs
northWestRemove (E:xs)    = E : northWestRemove xs
northWestRemove (EW:xs)   = E : northWestRemove xs
northWestRemove (EWS:xs)  = ES : northWestRemove xs
northWestRemove (ES:xs)   = ES : northWestRemove xs
northWestRemove (W:xs)    = NONE : northWestRemove xs
northWestRemove (WS:xs)   = S : northWestRemove xs
northWestRemove (S:xs)    = S : northWestRemove xs
northWestRemove (NONE:xs) = NONE : northWestRemove xs
                      
mazeShow :: Maze -> String
mazeShow (Maze [])   = ""
mazeShow (Maze (x:xs)) = "|" ++ lineShow x ++ "\n" ++ mazeShow (Maze xs) 

lineShow :: [Grid] -> String
lineShow []     = ""
lineShow (E:xs) = " |" ++ lineShow xs
lineShow (S:xs) = "__" ++ lineShow xs
lineShow (ES:xs) = "_|" ++ lineShow xs
lineShow (NONE:xs) = "  " ++ lineShow xs
    
--3x3 on http://www.logicmazes.com/theseus.html           
maze1 :: Maze
maze1 = Maze [[NW, NS, NE], [W, NES, W], [WS, S, ES]]

--7x4
maze2 :: Maze
maze2 = Maze [[NW,N,N,N,N,N,NE], [EW, EWS, W, NONE, NONE, NONE, E],
              [W,N,NONE,NONE,E,EWS,EW], [WS,NONE,S,S,S,S,ES]]

findPath :: Maze -> (Int, Int) -> [[(Int,Int)]]
findPath m x = findPath' m [[x]]

findPath' :: Maze -> [[(Int, Int)]]  -> [[(Int,Int)]]
findPath' m@(Maze (p:ps)) l@(((x,y):xs):xss)
  | ((findSolutionInSolutionStack m l) /= []) = findSolutionInSolutionStack m l
  | otherwise                                 = findPath' m
                                               (concat (map (updateList m) l))

--checks if a solution has been found yet                                 
findSolutionInSolutionStack :: Maze -> [[(Int,Int)]] -> [[(Int,Int)]]
findSolutionInSolutionStack m []= []
findSolutionInSolutionStack m@(Maze (p:ps)) l@(((x,y):xs):xss) 
  | (x < 0 || x >= (length p))  = reverse ((x,y):xs) : findSolutionInSolutionStack m xss
  | (y < 0 || y >  (length ps)) = reverse ((x,y):xs) : findSolutionInSolutionStack m xss
  | otherwise                   = findSolutionInSolutionStack m xss

--continue each attempt one move foward, whie making sure not to retrace steps
updateList :: Maze ->  ([(Int,Int)] -> [[(Int,Int)]])
updateList m = (\list -> (map (\move -> move : list)

                                ((moves m (head list)) \\  list) ))
--returns all possible moves
moves :: Maze -> (Int, Int) -> [(Int,Int)]
moves (Maze m) (x,y) = decodeGrid  (m!!y!!x) (x,y)
  where decodeGrid g (x,y)
          |g==N    = [(x-1,y),(x+1,y),(x,y+1)]
          |g==NE   = [(x-1,y),(x,y+1)]
          |g==NEW  = [(x,y+1)]
          |g==NEWS = []
          |g==NES  = [(x-1,y)]
          |g==NW   = [(x+1,y),(x,y+1)]
          |g==NWS  = [(x+1,y)]
          |g==NS   = [(x-1,y),(x+1,y)]
          |g==E    = [(x-1,y),(x,y-1),(x,y+1)]
          |g==EW   = [(x,y-1),(x,y+1)]
          |g==EWS  = [(x,y-1)]
          |g==ES   = [(x-1,y),(x,y-1)]
          |g==W    = [(x+1,y),(x,y-1),(x,y+1)]
          |g==WS   = [(x+1,y),(x,y-1)]
          |g==S    = [(x-1,y),(x+1,y),(x,y-1)]
          |g==NONE = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
