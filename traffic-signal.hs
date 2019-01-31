import CodeWorld

topCircle :: Color -> Picture
midCircle :: Color -> Picture
botCircle :: Color -> Picture
frame :: Picture

topCircle c = translated 0 (3) (colored c (solidCircle 1))
midCircle c = colored c (solidCircle 1)
botCircle c = translated 0 (-3) (colored c (solidCircle 1))
frame = rectangle 3 10

goState :: Picture
brakeState :: Picture
stopState :: Picture
readyState :: Picture

goState = topCircle Black & midCircle Black & botCircle Green & frame
brakeState = topCircle Black & midCircle Yellow & botCircle Black & frame
stopState = topCircle Red & midCircle Black & botCircle Black & frame
readyState = topCircle Red & midCircle Yellow & botCircle Black & frame

trafficController :: Double -> Picture
trafficController t
  | round t `mod` 6 < 2                   = goState
  | round t `mod` 6 >= 2 && round t `mod` 6 < 3 = brakeState
  | round t `mod` 6 >= 3 && round t `mod` 6 < 5 = stopState
  | otherwise                       = readyState
    
main :: IO()
main = animationOf trafficController