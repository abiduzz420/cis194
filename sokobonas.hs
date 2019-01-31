{-# LANGUAGE OverloadedStrings #-}
import CodeWorld


data Tile = Wall | Ground | Storage | Box | Blank
data Direction = U | D | L | R
data Coord = C Integer Integer
data PlayerState = PS Coord Picture

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
changePlayerPicture R = lowerBody & path [(0,0),(0.3,0.05)] & path [(0,0),(0.3,-0.05)]
                                  & playerHead (circle 0.18 & sector (7/6*pi) (1/6*pi) 0.18)
changePlayerPicture L = lowerBody & path [(0,0),(-0.3,0.05)] & path [(0,0),(-0.3,-0.05)]
                                  & playerHead (circle 0.18 & sector (11/6*pi) (17/6*pi) 0.18)
changePlayerPicture D = lowerBody & path [(0,0),(0.3,0.05)] & path[(0,0),(-0.3,0.05)] 
                                  & playerHead (circle 0.18 & 
                                               translated (-0.05) 0.02 (solidCircle 0.04)
                                               & translated 0.05 0.02 (solidCircle 0.04)) 

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

move :: Direction -> Coord -> Coord
move dir c
  | isOk adjTile = adjCoord
  | otherwise = c
    where
      adjCoord = adjacentCoord dir c
      adjTile = maze adjCoord

isOk :: Tile -> Bool
isOk Ground = True
isOk Storage = True
isOk _ = False

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord D (C x y) = C x (y-1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord R (C x y) = C (x+1) y

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
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze (C r c)))

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 1        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

handleTime :: Double -> PlayerState -> PlayerState
handleTime _ ps = ps

handleEvent :: Event -> PlayerState -> PlayerState
handleEvent (KeyPress key) (PS c _)
    | key == "Right" = (PS (move R c) (changePlayerPicture R))
    | key == "Up"    = (PS (move U c) (changePlayerPicture U))
    | key == "Left"  = (PS (move L c) (changePlayerPicture L))
    | key == "Down"  = (PS (move D c) (changePlayerPicture D))
handleEvent _ ps      = ps

drawState :: PlayerState -> Picture
drawState (PS c pic) = atCoord c pic & pictureOfMaze

initPlayerState :: PlayerState
initPlayerState = (PS (C 0 1) player)

data SSState world = StartScreen | Running world
data Interaction world  = Interaction
    world
    (Double -> world -> world)
    (Event -> world -> world)
    (world -> Picture)
    
startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where  
    handle' (KeyPress key) _ | key == "Esc" = state0
    handle' e s = handle e s

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
        
runInteraction :: Interaction s -> IO()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

initialInteraction :: Interaction PlayerState
initialInteraction = Interaction initPlayerState handleTime handleEvent drawState
 
main :: IO ()
main = runInteraction (resetable (withStartScreen initialInteraction))