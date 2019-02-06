{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists

data List a = Empty | Entry a (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

-- Coordinates

data Coord = C Integer Integer
data Direction = U | D | L | R

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord D (C x y) = C x (y-1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord R (C x y) = C (x+1) y

-- The maze

data Tile = Wall | Ground | Storage | Box | Blank

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

eqCoord :: Coord -> Coord -> Bool
eqCoord (C r1 c1) (C r2 c2)
  | r1 == r2 && c1 == c2 = True
  | otherwise = False

noBoxMaze :: Coord -> Tile
noBoxMaze c = case (maze c) of
  Box -> Ground
  t -> t
 
contains :: List Coord -> Coord -> Bool
contains Empty _ = False
contains (Entry c cs) coord = eqCoord c coord || contains cs coord

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes Empty c = maze c
mazeWithBoxes list coord
  | contains list coord = Box
  | otherwise = noBoxMaze coord

-- The state

data State = S Coord Direction (List Coord)

appendList :: List a -> List a -> List a
appendList a Empty = a
appendList a (Entry b bRest) = appendList nA bRest
  where
    nA = Entry b a

isBox :: Tile -> Bool
isBox Box = True
isBox _ = False

getBox :: Coord -> List Coord
getBox c
  | isBox (maze c) = Entry c Empty
  | otherwise = Empty

allBoxes :: Integer -> List Coord
allBoxes 11 = Empty
allBoxes n = appendList (colBox (C n (-10))) (allBoxes (n+1))

colBox :: Coord -> List Coord
colBox (C _ 11) = Empty
colBox (C r c) = appendList (getBox (C r c)) (colBox (C r (c+1)))

initBoxList :: List Coord
initBoxList = allBoxes (-10)

initState :: State
initState = (S (C 0 1) R initBoxList)

-- Event handling

changeBoxState :: State -> State
changeBoxState (S playerCurrent dir list) = boxMove (S playerCurrent dir list) boxCurrent boxTo  
                                              where
                                                boxCurrent = adjacentCoord dir playerCurrent
                                                boxTo = adjacentCoord dir boxCurrent

boxMove :: State -> Coord -> Coord -> State
boxMove (S playerCurrent dir list) boxFromCoord boxToCoord
  | isOk (mazeWithBoxes list boxToCoord) = (S boxFromCoord dir (mapList (\c -> moveFromTo boxFromCoord boxToCoord c) list))  
  | otherwise = (S playerCurrent dir list)

move :: State -> State
move (S c dir list)
  | isBox adjTile = changeBoxState (S c dir list)
  | isOk adjTile = (S adjCoord dir list)
  | otherwise = (S c dir list)
    where
      adjCoord = adjacentCoord dir c
      adjTile = mazeWithBoxes list adjCoord

isOk :: Tile -> Bool
isOk Ground = True
isOk Storage = True
isOk _ = False

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo fromC toC coordInTheList
  | eqCoord fromC coordInTheList = toC
  | otherwise = coordInTheList

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (S c dir bcs)
    | isWon (S c dir bcs) = (S c dir bcs)
    | key == "Right" = move (S c R bcs)
    | key == "Up"    = move (S c U bcs)
    | key == "Left"  = move (S c L bcs)
    | key == "Down"  = move (S c D bcs)
handleEvent _ state      = state

handleTime :: Double -> State -> State
handleTime _ ps = ps

-- Drawing

wall, ground, storage, box, player, lowerBody :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown (solidRectangle 1 1)
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

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> (draw21times (\c -> (drawTileAt r c))))

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (noBoxMaze (C r c)))
  
drawState :: State -> Picture
drawState (S c dir bxs) = hasWon (S c dir bxs) & atCoord c (changePlayerPicture dir)  & boxes bxs & pictureOfMaze
   
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
    
startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

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

-- Match Won

wonScreen :: Picture
wonScreen = scaled 2 2 (colored white (text "You won!")) 

isStorage :: Tile -> Bool
isStorage Storage = True
isStorage _ = False

boxOnStorage :: Coord -> Bool
boxOnStorage boxCoord
  | isStorage (noBoxMaze boxCoord) =  True
  | otherwise = False

reduceList :: List Bool -> Bool
reduceList Empty = True
reduceList (Entry c cs) = c && reduceList cs

allOnStorage :: State -> Bool
allOnStorage (S _ _ list) = reduceList (mapList (\c -> boxOnStorage c) list)

isWon :: State -> Bool
isWon state
 | allOnStorage state = True
 | otherwise = False
 
hasWon :: State -> Picture
hasWon state
  | isWon state = wonScreen
  | otherwise = blank

-- The main function

sokoban :: Interaction State
sokoban = Interaction initState handleTime handleEvent drawState

main :: IO ()
main = runInteraction (resetable (withStartScreen sokoban))