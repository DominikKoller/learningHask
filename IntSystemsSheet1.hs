import Data.List
import Data.Maybe

data State = State { 
                     ml :: Int 
                   , cl :: Int
                   , mr :: Int
                   , cr :: Int
                   , boat :: Int
                  } deriving Show

initialState = State 3 3 0 0 1

isGoal (State ml cl mr cr _) = ml == 0 && cl == 0 && mr == 3 && cr == 3

data Action = Action Int Int deriving Show

isValid (State ml cl mr cr _) = (ml >= cl || ml == 0) &&
                                (mr >= cr || mr == 0) &&
                                ml>=0 && cl>=0 && mr>=0 && cr>=0  

actions s = filter (\a -> isValid $ result s a) [Action 0 1, Action 1 0, Action 0 2, Action 1 1, Action 2 0]

result (State ml cl mr cr b) (Action am ac) = State
    (ml - (am*b))
    (cl - (ac*b))
    (mr + (am*b))
    (cr + (ac*b))
    (b*(-1))


data Node = Node State Path deriving Show
newNode (Node s p) action = Node newState ((newState, action):p)
            where newState = result s action

getState (Node s _) = s
children (Node s p) = map(newNode (Node s p)) . actions $ s

type Path = [(State, Action)]

depthFirstSearch :: Node -> Maybe Node
depthFirstSearch n
    | isGoal.getState $ n = Just n
    | otherwise =   headMay
                    .catMaybes 
                    .map (depthFirstSearch)
                    .children $ n

breadthFirstSearch :: [Node] -> Maybe Node
breadthFirstSearch frontier
     | any(isGoal.getState) frontier = find(isGoal.getState) frontier
     | otherwise =   breadthFirstSearch
                     .concat
                     .map (children) 
                     $ frontier

maxDepthSearch :: Int -> Node -> Maybe Node
maxDepthSearch d n
    | isGoal.getState $ n = Just n
    | d == 0 = Nothing
    | otherwise =   headMay
                    .catMaybes
                    .map (maxDepthSearch (d-1))
                    .children $ n

depthIteratingSearch d n
    | isJust result = result
    | otherwise = depthIteratingSearch (d+1) n
        where result = maxDepthSearch d n

-- | This is defined in Safe.hs, but I don't know why I cannot load it

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay xs = Just (head xs)

-- | Breadth First Search:
-- | Just (Node 
-- |     (State {ml = 0, cl = 0, mr = 3, cr = 3, boat = -1}) 
-- | 
-- |     [(State {ml = 0, cl = 0, mr = 3, cr = 3, boat = -1},    Action 0 2),
-- |     (State {ml = 0, cl = 2, mr = 3, cr = 1, boat = 1},      Action 0 1),
-- |     (State {ml = 0, cl = 1, mr = 3, cr = 2, boat = -1},     Action 0 2),
-- |     (State {ml = 0, cl = 3, mr = 3, cr = 0, boat = 1},      Action 0 1),
-- |     (State {ml = 0, cl = 2, mr = 3, cr = 1, boat = -1},     Action 2 0),
-- |     (State {ml = 2, cl = 2, mr = 1, cr = 1, boat = 1},      Action 1 1),
-- |     (State {ml = 1, cl = 1, mr = 2, cr = 2, boat = -1},     Action 2 0),
-- |     (State {ml = 3, cl = 1, mr = 0, cr = 2, boat = 1},      Action 0 1),
-- |     (State {ml = 3, cl = 0, mr = 0, cr = 3, boat = -1},     Action 0 2),
-- |     (State {ml = 3, cl = 2, mr = 0, cr = 1, boat = 1},      Action 0 1),
-- |     (State {ml = 3, cl = 1, mr = 0, cr = 2, boat = -1},     Action 0 2)])
-- | (0.19 secs, 57,161,992 bytes)