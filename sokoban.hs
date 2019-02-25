{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists

data List a = Empty | Entry a (List a) deriving Eq

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

appendList :: List a -> List a -> List a
appendList a Empty = a
appendList a (Entry b bRest) = appendList nA bRest
  where
    nA = Entry b a

listLength :: List a -> Integer
listLength Empty = 0
listLength (Entry x xs) = 1 + listLength xs 

filterList :: (a -> Bool) -> List a -> List a
filterList _ Empty = Empty
filterList p (Entry x xs)
  | p x = Entry x (filterList p xs)
  | otherwise = filterList p xs

contains :: Eq a => a -> List a -> Bool
contains _ Empty = False
contains coord (Entry c cs) = c == coord || contains coord cs 

nths :: List a -> Integer -> a
nths (Entry x xs) n
  | n > listLength (Entry x xs) = error "list too short"
  | n == 1 = x
  | otherwise = nths xs (n-1)
  
-- Coordinates

data Coord = C Integer Integer
data Direction = U | D | L | R deriving Eq

allDirections :: List Direction
allDirections = Entry U (Entry L (Entry R (Entry D Empty)))

instance Eq Coord where
  C x1 y1 == C x2 y2 = x1 == x2 && y1 == y2
  c1 /= c2 = not (c1 == c2)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord U (C x y) = C x (y+1)

adjacentCoord D (C x y) = C x (y-1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord R (C x y) = C (x+1) y

-- The maze

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 1        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
  
boxes :: List Coord -> Picture
boxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

noBoxMaze :: (Coord -> Tile) -> Coord -> Tile
noBoxMaze maze c = case (maze c) of
  Box -> Ground
  t -> t 

mazeWithBoxes :: Integer -> List Coord -> Coord -> Tile
mazeWithBoxes _ Empty c = maze c
mazeWithBoxes level list coord
  | contains coord list = Box
  | otherwise = noBoxMaze maze coord
  where
    (Maze c maze) = nths mazes level

-- The state

data State = S Coord Direction (List Coord) Integer deriving Eq

getBox :: (Coord -> Tile) -> Coord -> List Coord
getBox maze c
  | (maze c) == Box = Entry c Empty
  | otherwise = Empty

allBoxes :: (Coord -> Tile) -> Integer -> List Coord
allBoxes _ 11 = Empty
allBoxes maze n = appendList (colBox maze (C n (-10))) (allBoxes maze (n+1))

colBox :: (Coord -> Tile) -> Coord -> List Coord
colBox _ (C _ 11) = Empty
colBox maze (C r c) = appendList (getBox maze (C r c)) (colBox maze (C r (c+1)))

initBoxList :: (Coord -> Tile) -> List Coord
initBoxList maze = allBoxes maze (-10)

initState :: State
initState = loadMaze 1

loadMaze :: Integer -> State
loadMaze n = (S initialCoord R (initBoxList maze) n)
  where (Maze initialCoord maze) = nths mazes n

-- Event handling

changeBoxState :: State -> State
changeBoxState (S playerCurrent dir list level) = boxMove (S playerCurrent dir list level) boxCurrent boxTo  
                                              where
                                                boxCurrent = adjacentCoord dir playerCurrent
                                                boxTo = adjacentCoord dir boxCurrent

boxMove :: State -> Coord -> Coord -> State
boxMove (S playerCurrent dir list level) boxFromCoord boxToCoord
  | isOk (mazeWithBoxes level list boxToCoord) = (S boxFromCoord dir (mapList (\c -> moveCoordFromTo boxFromCoord boxToCoord c) list) level)  
  | otherwise = (S playerCurrent dir list level)

move :: State -> State
move (S c dir list level)
  | adjTile == Box = changeBoxState (S c dir list level)
  | isOk adjTile = (S adjCoord dir list level)
  | otherwise = (S c dir list level)
    where
      adjCoord = adjacentCoord dir c
      adjTile = mazeWithBoxes level list adjCoord

isOk :: Tile -> Bool
isOk Ground = True
isOk Storage = True
isOk _ = False

moveFromTo :: Eq c => c -> c -> c -> c
moveFromTo fromC toC cFromTheList
  | fromC == cFromTheList = toC
  | otherwise = cFromTheList

moveCoordFromTo :: Coord -> Coord -> Coord -> Coord
moveCoordFromTo = moveFromTo

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (S c dir bcs level)
    | isWon bcs level = loadMaze (level + 1)
    | key == "Right" = move (S c R bcs level)
    | key == "Up"    = move (S c U bcs level)
    | key == "Left"  = move (S c L bcs level)
    | key == "Down"  = move (S c D bcs level)
handleEvent _ state      = state

handleTime :: Double -> State -> State
handleTime _ ps = ps

-- Drawing

wall, ground, storage, box, player, lowerBody :: Picture
wall =    colored gray (solidRectangle 1 1)
ground =  colored Yellow (solidRectangle 1 1)
storage = colored White (solidCircle 0.3) & ground
box =     colored Brown (solidRectangle 1 1)
player = translated 0.0 0.3 cranium
       & path [(0,0),(0.3,0.05)] 
       & path [(0,0),(0.3,-0.05)] 
       & path [(0,-0.2),(0,0.1)] 
       & path [(0,-0.2),(0.1,-0.5)]
       & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18 & sector (7/6*pi) (1/6*pi) 0.18

lowerBody = path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
        
playerHead :: Picture -> Picture
playerHead dim = translated 0.0 0.3 dim
changePlayerPicture :: Direction -> Picture
changePlayerPicture U = lowerBody & path [(0,0),(0.3,0.05)] & path[(0,0),(-0.3,0.05)] & playerHead (solidCircle 0.18)
changePlayerPicture R = player
changePlayerPicture L = lowerBody & path [(0,0),(-0.3,0.05)] & path [(0,0),(-0.3,-0.05)]
                                  & playerHead (circle 0.18 & sector (11/6*pi) (17/6*pi) 0.18)
changePlayerPicture D = lowerBody & path [(0,0),(0.3,0.05)] & path[(0,0),(-0.3,0.05)] 
                                  & playerHead (circle 0.18 & 
                                               translated (-0.05) 0.02 (solidCircle 0.04)
                                               & translated 0.05 0.02 (solidCircle 0.04)) 

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go 11 = blank
    go n = something n & go (n+1)

pictureOfMaze :: Integer -> Picture
pictureOfMaze level = draw21times (\r -> (draw21times (\c -> (drawTileAt level r c))))

drawTileAt :: Integer -> Integer -> Integer -> Picture
drawTileAt level r c = translated (fromIntegral r) (fromIntegral c) (drawTile (noBoxMaze maze (C r c)))
  where (Maze initCoord maze) = nths mazes level
  
drawState :: State -> Picture
drawState (S c dir bxs level) = hasWon bxs level & atCoord c (changePlayerPicture dir)  & boxes bxs & (pictureOfMaze level)

-- The complete interaction

runInteraction :: Interaction s -> IO()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where  
    handle' (KeyPress key) _ | key == "Esc" = state0
    handle' e s = handle e s

-- Start Screen 

data SSState world = StartScreen | Running world
data Interaction world  = Interaction
    world
    (Double -> world -> world)
    (Event -> world -> world)
    (world -> Picture)

instance Eq s => Eq (SSState s) where
  StartScreen == StartScreen = True
  Running s == Running s' = s == s'
  _ == _ = False

startScreen :: Picture
startScreen = colored purple (scaled 3 3 (text "Sokoban!")) & instructions

instructions :: Picture
instructions = translated 0 (-5) (scaled 1 1 (colored gray (text "Space to begin"))) &
               translated 0 (-6) (scaled 1 1 (colored gray (text "u to undo"))) &
               translated 0 (-7) (scaled 1 1 (colored gray (text "Esc to exit"))) &
               translated 0 (-3) (scaled 1 1 (text "Press ↑, →, ↓, ← to move"))

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen
    
    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)
    
    handle' (KeyPress key) StartScreen
      | key == " " = Running state0
    handle' _ StartScreen = StartScreen
    handle' e (Running s) = Running (handle e s)
    
    draw' StartScreen = startScreen
    draw' (Running s) = draw s
    
-- Undo functionality

data WithUndo a = WithUndo a (List a)

undoInteraction :: Eq a => Interaction a -> Interaction (WithUndo a)
undoInteraction (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = WithUndo state0 Empty
    step' t (WithUndo s stack) = WithUndo (step t s) stack
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of
        Entry s' stack' -> WithUndo s' stack'
        Empty -> WithUndo s Empty
    handle' e (WithUndo s stack)
      | s' == s = WithUndo s stack
      | otherwise = WithUndo (handle e s) (Entry s stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s
 
-- Match Won

wonScreen :: Picture
wonScreen = scaled 2 2 (colored white (text "You won!")) 

isStorage :: Tile -> Bool
isStorage Storage = True
isStorage _ = False

boxOnStorage :: Integer -> Coord -> Bool
boxOnStorage level boxCoord
  | isStorage (noBoxMaze maze boxCoord) =  True
  | otherwise = False
  where
    (Maze _ maze) = nths mazes level

reduceList :: List Bool -> Bool
reduceList Empty = True
reduceList (Entry c cs) = c && reduceList cs

allOnStorage :: List Coord -> Integer -> Bool
allOnStorage list level = reduceList (mapList (\c -> boxOnStorage level c) list)

isWon :: List Coord -> Integer -> Bool
isWon list level
 | allOnStorage list level = True
 | otherwise = False
 
hasWon :: List Coord -> Integer -> Picture
hasWon list level
  | isWon list level && listLength mazes == level = gameOver
  | isWon list level = wonScreen
  | otherwise = blank

gameOver :: Picture
gameOver = colored black (scaled 2 2 (text "You Won All! 🎉"))

-- The main function

sokoban :: Interaction State
sokoban = Interaction initState handleTime handleEvent drawState

main :: IO ()
main = runInteraction (resetable (undoInteraction (withStartScreen sokoban)))

-- List of all mazes

data Maze = Maze Coord (Coord -> Tile)

mazes :: List Maze
mazes =
  Entry (Maze (C 0 1)       maze1) $
  Entry (Maze (C (-2) 4)    maze6) $
  Entry (Maze (C (-4) 3)    maze3) $
  Entry (Maze (C 1 1)       maze9) $ 
  Entry (Maze (C 0 1)       maze5) $
  Entry (Maze (C 1 (-3))    maze4) $
  Empty
  
extraMazes :: List Maze
extraMazes =
  Entry (Maze (C 1 (-3))    maze4') $
  Entry (Maze (C 1 (-3))    maze4'') $
  Entry (Maze (C 1 1)       maze9') $
  mazes

maze1 :: Coord -> Tile 
maze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

maze3 :: Coord -> Tile
maze3 (C (-5) (-5)) = Wall
maze3 (C (-5) (-4)) = Wall
maze3 (C (-5) (-3)) = Wall
maze3 (C (-5) (-2)) = Wall
maze3 (C (-5) (-1)) = Wall
maze3 (C (-5)   0 ) = Wall
maze3 (C (-5)   1 ) = Wall
maze3 (C (-5)   2 ) = Wall
maze3 (C (-5)   3 ) = Wall
maze3 (C (-5)   4 ) = Wall

maze3 (C (-4) (-5)) = Wall
maze3 (C (-4) (-4)) = Ground
maze3 (C (-4) (-3)) = Ground
maze3 (C (-4) (-2)) = Ground
maze3 (C (-4) (-1)) = Ground
maze3 (C (-4)   0 ) = Ground
maze3 (C (-4)   1 ) = Ground
maze3 (C (-4)   2 ) = Ground
maze3 (C (-4)   3 ) = Ground
maze3 (C (-4)   4 ) = Wall

maze3 (C (-3) (-5)) = Wall
maze3 (C (-3) (-4)) = Ground
maze3 (C (-3) (-3)) = Wall
maze3 (C (-3) (-2)) = Wall
maze3 (C (-3) (-1)) = Wall
maze3 (C (-3)   0 ) = Wall
maze3 (C (-3)   1 ) = Ground
maze3 (C (-3)   2 ) = Wall
maze3 (C (-3)   3 ) = Ground
maze3 (C (-3)   4 ) = Wall
maze3 (C (-3)   5 ) = Wall

maze3 (C (-2) (-5)) = Wall
maze3 (C (-2) (-4)) = Box
maze3 (C (-2) (-3)) = Ground
maze3 (C (-2) (-2)) = Ground
maze3 (C (-2) (-1)) = Ground
maze3 (C (-2)   0 ) = Wall
maze3 (C (-2)   1 ) = Ground
maze3 (C (-2)   2 ) = Box
maze3 (C (-2)   3 ) = Box
maze3 (C (-2)   4 ) = Ground
maze3 (C (-2)   5 ) = Wall

maze3 (C (-1) (-6)) = Wall
maze3 (C (-1) (-5)) = Wall
maze3 (C (-1) (-4)) = Ground
maze3 (C (-1) (-3)) = Ground
maze3 (C (-1) (-2)) = Ground
maze3 (C (-1) (-1)) = Ground
maze3 (C (-1)   0 ) = Wall
maze3 (C (-1)   1 ) = Ground
maze3 (C (-1)   2 ) = Ground
maze3 (C (-1)   3 ) = Box
maze3 (C (-1)   4 ) = Ground
maze3 (C (-1)   5 ) = Wall
maze3 (C (-1)   6 ) = Wall

maze3 (C   0  (-6)) = Wall
maze3 (C   0  (-5)) = Ground
maze3 (C   0  (-4)) = Ground
maze3 (C   0  (-3)) = Ground
maze3 (C   0  (-2)) = Ground
maze3 (C   0  (-1)) = Ground
maze3 (C   0    0 ) = Wall
maze3 (C   0    1 ) = Wall
maze3 (C   0    2 ) = Wall
maze3 (C   0    3 ) = Wall
maze3 (C   0    4 ) = Ground
maze3 (C   0    5 ) = Ground
maze3 (C   0    6 ) = Wall

maze3 (C   1  (-6)) = Wall
maze3 (C   1  (-5)) = Ground
maze3 (C   1  (-4)) = Ground
maze3 (C   1  (-3)) = Ground
maze3 (C   1  (-2)) = Ground
maze3 (C   1  (-1)) = Ground
maze3 (C   1    0 ) = Wall
maze3 (C   1    1 ) = Storage
maze3 (C   1    2 ) = Storage
maze3 (C   1    3 ) = Storage
maze3 (C   1    4 ) = Ground
maze3 (C   1    5 ) = Ground
maze3 (C   1    6 ) = Wall

maze3 (C   2  (-6)) = Wall
maze3 (C   2  (-5)) = Wall
maze3 (C   2  (-4)) = Ground
maze3 (C   2  (-3)) = Ground
maze3 (C   2  (-2)) = Ground
maze3 (C   2  (-1)) = Ground
maze3 (C   2    0 ) = Wall
maze3 (C   2    1 ) = Wall
maze3 (C   2    2 ) = Wall
maze3 (C   2    3 ) = Wall
maze3 (C   2    4 ) = Wall
maze3 (C   2    5 ) = Wall
maze3 (C   2    6 ) = Wall

maze3 (C   3  (-5)) = Wall
maze3 (C   3  (-4)) = Ground
maze3 (C   3  (-3)) = Ground
maze3 (C   3  (-2)) = Storage
maze3 (C   3  (-1)) = Ground
maze3 (C   3    0 ) = Wall

maze3 (C   4  (-5)) = Wall
maze3 (C   4  (-4)) = Wall
maze3 (C   4  (-3)) = Wall
maze3 (C   4  (-2)) = Wall
maze3 (C   4  (-1)) = Wall
maze3 (C   4    0 ) = Wall

maze3 _ = Blank

maze4 :: Coord -> Tile
maze4 (C x y)
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x ==  2 && y <   0           = Wall
  | x >= -1 && y ==  1 && x <= 2 = Wall
  | x == -3 && y ==  1           = Wall
  | x ==  0 && y ==  3           = Wall
  | x ==  0 && y ==  0           = Wall
  | x ==  3 && y == -3           = Storage
  | x ==  1 && y ==  2           = Storage
  | x == -3 && y ==  2           = Storage
  | x ==  1 && y == -1           = Storage
  | x == -2 && y ==  1           = Box
  | x ==  2 && y ==  2           = Box
  | x <=  1 && y == -2 && x >= 0 = Box
  | otherwise                    = Ground

maze5 :: Coord -> Tile 
maze5 (C x y)
  | abs x >  4 || abs y >  4           = Blank
  | abs x == 4 || abs y == 4           = Wall
  | x ==     1 && y <      0           = Wall
  | x ==    -3 && y ==    -2           = Wall
  | x <=     1 && x >     -2 && y == 0 = Wall
  | x >     -3 && x <      3 && y == 2 = Wall
  | x ==     3 && y >      1           = Storage
  | y ==    -2 && x <      0           = Box
  | y ==    -2 && x ==     2           = Box
  | y ==    0  && x ==     3           = Box
  | y == -1    && x > 1      && x < 4  = Storage
  | otherwise                          = Ground

maze6 :: Coord -> Tile 
maze6 (C x y)
  | abs x > 3  || abs y > 5                 = Blank
  | abs x == 3 || (abs y == 5 && abs x < 4) = Wall
  | x == 0 && abs y < 4                     = Storage
  | x == -1 && (y == 0 || abs y == 2)       = Box
  | x == 1 && (abs y == 1 || abs y == 3)    = Box
  | x == (-2) &&  y == 1                    = Wall
  | otherwise                               = Ground

maze7 :: Coord -> Tile
maze7 (C x y)
  | abs x > 4  || abs y > 4   = Blank
  | abs x == 4 || abs y == 4  = Wall
  | not (x == 2)  && y == 2   = Wall
  | not (x == -2)  && y == -1 = Wall
  | x ==  3 && y == -3        = Storage
  | x == 2 && y == 2          = Box
  | otherwise                 = Ground
  
maze8 :: Coord -> Tile
maze8 (C x y)
  | abs x > 10 || abs y > 10    = Blank
  | x == 0 && y == 0            = Ground
  | abs x == 9 && abs y == 9    = Wall
  | abs x == 10 || abs y == 10  = Wall
  | x == y                      = Storage
  | abs x == abs y              = Box
  | x < 0 && x > (-9) && y == 0 = Box
  | x > 0 && x < 9 && y == 0    = Storage
  | otherwise                   = Ground

maze9 :: Coord -> Tile 
maze9 (C x y)
  | abs x > 4  || abs y > 4                  = Blank
  | abs x == 4 || abs y == 4 || x == -3      = Wall
  | x == -2 && (y == 3 || y == 0)            = Wall
  | x == -1 &&  y == -1                      = Wall
  | x == -0 &&  y == 1                       = Wall
  | x ==  3 &&  y == 0                       = Wall
  | x <   0 && (y == 2 || y == -3)           = Storage
  | x == -1 &&  y == 1                       = Storage
  | x ==  0 && (y == 2 || y == 0 || y == -1) = Box
  | x ==  1 &&  y == -2                      = Box
  | x ==  2 &&  y == -3                      = Box
  | otherwise                                = Ground

maze4'' :: Coord -> Tile
maze4'' (C 1 (-3)) = Box
maze4'' c = maze4 c

maze4' :: Coord -> Tile
maze4' (C 0 1) = Blank
maze4' c = maze4 c

maze9' :: Coord -> Tile
maze9' (C 3 0) = Box
maze9' (C 4 0) = Box
maze9'  c      = maze9 c

-- Graph Search (unused)

isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk = go Empty (Entry initial Empty)
  where
    go _ Empty = True
    go seen (Entry c todo) | contains c seen = go seen todo
    go _ (Entry c _) | not(isOk c) = False
    go seen (Entry c todo) = go (Entry c seen) (appendList (adjacent c) todo)
  
isClosed :: Maze -> Bool
isClosed (Maze c0 maze) = onRightSpot && isGraphClosed c0 adjacent isOk
  where
    onRightSpot = case maze c0 of
      Ground -> True
      Storage -> True
      _ -> False
    isOk c = case maze c0 of
      Blank -> False
      _ -> True
    adjacent c = filterList check (mapList (\d -> adjacentCoord d c) allDirections)
      where
        check c = case maze c of
          Wall -> False
          _ -> True

