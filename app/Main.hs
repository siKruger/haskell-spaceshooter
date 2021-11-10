module Main where

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed (Pico)
import System.IO.Unsafe
import Text.Read.Lex (Number)

import Data.Int
import System.Random (randomRIO, getStdGen, Random (randomR))
import Data.Semigroup (diff)
import Graphics.Gloss.Data.Point (pointInBox)

data MoveDirection = East | West | North | South | None
  deriving (Eq)

type Asteroid = Point

data GameState =
  GameState
    { position :: Point
    , direction :: MoveDirection
    , livesLeft :: Int
    , difficulty :: Int
    , asteroids :: [Asteroid]
    , asteroidSpawnPos :: IO Float
    , asteroidSpawnChance :: IO Int
    }

tileSize :: Float
tileSize = 32.0

window :: Display
window = InWindow "Space Shooter" (500, 700) (50, 50)

background :: Color
background = makeColor 0.033 0.04 0.26 1

fps :: Int
fps = 60


-- Key Input abarbeiten und handeln
handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs = gs {direction = West}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs = gs {direction = East}
handleKeys (EventKey (SpecialKey KeyUp ) Down _ _) gs = gs {direction = North}
handleKeys (EventKey (SpecialKey KeyDown  ) Down _ _) gs = gs {direction = South}
handleKeys _ gs = gs {direction = None}



{-
Kollision mit der Wand unserer Spielwelt

todo: refactor in eine methode mit pattern matching
-}
isCollisionWithGameWallRight :: Point -> Bool
isCollisionWithGameWallRight pnt = if(fst (pnt) < 200) then True else False

isCollisionWithGameWallLeft :: Point -> Bool
isCollisionWithGameWallLeft pnt = if(fst (pnt) > -200) then True else False

isCollisionWithGameWallUp :: Point -> Bool
isCollisionWithGameWallUp pnt = if(snd (pnt) < 300) then True else False 

isCollisionWithGameWallDown :: Point -> Bool
isCollisionWithGameWallDown pnt = if(snd (pnt) > -300) then True else False 




{-
  Move player mit Abfrage für Game Over
-}
movePlayer :: MoveDirection -> GameState -> Point
movePlayer dir gs = if(livesLeft gs > 0) then 
  movePlayerProc dir gs
  else
    (0,-50)

{- 
fst, snd geben das erste/letzte Element einer Liste zurück
-}
movePlayerProc :: MoveDirection -> GameState -> Point
movePlayerProc East gs = 
  if(isCollisionWithGameWallRight (fst (position gs), snd (position gs))) 
    then  
    (fst (position gs) + 5, snd (position gs))
    else
      (fst (position gs), snd (position gs))
  
movePlayerProc West gs =
  if(isCollisionWithGameWallLeft (fst (position gs), snd (position gs))) 
    then
      (fst (position gs) + 5 * (-1), snd (position gs))
    else
      (fst (position gs), snd (position gs))

movePlayerProc North gs =
  if(isCollisionWithGameWallUp (fst (position gs), snd (position gs)))
    then
      (fst (position gs), snd (position gs) + (-5) * (-1))
    else 
      (fst (position gs), snd (position gs))


movePlayerProc South gs =
  if(isCollisionWithGameWallDown (fst (position gs), snd (position gs)))
    then
      (fst (position gs), snd (position gs) +  (-5) * 1)
    else 
      (fst (position gs), snd (position gs))
      
movePlayerProc _ gs = (fst (position gs), snd (position gs))



{-
  Asteroiden erstellen wir hier
-}
generateAsteroid :: GameState -> [Asteroid]
generateAsteroid gs = 
  --if((unsafePerformIO (asteroidSpawnChance gs)) * (difficulty gs)> 295) then
  if((unsafePerformIO (asteroidSpawnChance gs)) + (difficulty gs * 3)> 295 && length (asteroids gs) < 8 + 1 * fromIntegral (difficulty gs)) then
       [(unsafePerformIO (asteroidSpawnPos gs), 320)]
    else 
      []



{-
  Überprüft die Kollision mit allen Asteroiden
-}
isCollidingWithAsteroideList :: GameState -> Bool
isCollidingWithAsteroideList gs = length [pos | pos <- asteroids gs, isCollidingWithAsteroide  pos (position gs)] > 0


{-
  Überprüft die Kollisionen mit einem Punkt
-}

--haben aste 24x24
-- schiff hat 50x53

collisionSize :: Float
collisionSize = 5

isCollidingWithAsteroide :: Point -> Point -> Bool 
isCollidingWithAsteroide playerPos astePos = pointInBox playerPos (fst astePos + 25, snd astePos + 8) (fst astePos - 25, snd astePos - 25)
{-
(1,2) (3,4)
1 geht nach rechts
3 geht nach links

2 geht nach oben
4 geht nach unten

-}
-- 30 15




asteroidCollisionDetect :: GameState -> [Asteroid]
asteroidCollisionDetect gs = [(fst aste, snd aste - (asteroidBaseSpeed +  0.2 * fromIntegral (difficulty gs))) | aste <- asteroids gs, snd aste > -320 && not (isCollidingWithAsteroide aste (position gs))]


{-
  Updaten des Gamestates!
-}
update :: Float -> GameState -> GameState
update _ gs = 
  gs {
    position = movePlayer (direction gs) gs,
    asteroids = asteroidCollisionDetect gs ++ generateAsteroid gs,
    livesLeft = if (isCollidingWithAsteroideList gs) then livesLeft gs -1 else livesLeft gs
  }











{-
  Generiert random Positionen für unsere Asteroiden
-}
getRandomNum :: IO Float
getRandomNum =  randomRIO ((0),20) --150 --120?

{-
  Random int zur bestimmung, ob ein Asteroid spawn (decluttern)
-}
getRandomInt :: IO Int
getRandomInt = randomRIO(0, 300)

asteroidBaseSpeed :: Float
asteroidBaseSpeed = 0.8 --0.8

main :: IO ()
main = do
  asteroidImg <- loadBMP "assets/Asteroid.bmp"
  spaceshipImg <- loadBMP "assets/SpaceshipTest1.bmp"
  let state =
        GameState
          { position = (0.0, 0.0)
          , direction = None
          , livesLeft = 500
          --, asteroids = [(300, -300), (300, -200), (300, 0), (300, -100), (300, 200), (300, 300), (300, 400), (300, 500), (300, 600), (00, 50)]
          , asteroids = [(80, 90), (-80, 90), (50, 90), (-50, 90), (40, 90), (-40, 90), (30, 90), (-30, 90), (20, 80), (-20, 80), (00, 80)] -- SHOULD COLLIDE WITH ALL OF THEM
          , difficulty = 1
          , asteroidSpawnPos = getRandomNum
          , asteroidSpawnChance = getRandomInt
          }
  play
    window
    background
    fps
    state
    (`render` [asteroidImg, spaceshipImg])
    handleKeys
    update


-- render Methode von Gloss zum Darstellen
render :: GameState -> [Picture] -> Picture
render gs imgs = pictures
    ([ translate
         (fst (position gs))
         (snd (position gs))
         (imgs !! 1) -- Position 2 des render arrays stellt den Spieler da
     ] ++ [
       drawLivesLeft gs
       ] ++ [
         drawAsteroids gs imgs
       ])

{-
  Liefert alle Asteroiden als Picture
-}
drawAsteroids :: GameState -> [Picture] -> Picture
drawAsteroids gs imgs = pictures [scale (asteroidSizeCalc gs) (asteroidSizeCalc gs) (translate (fst aste) (snd aste) (imgs !! 0)) | aste <- asteroids gs]

asteroidSizeCalc :: GameState -> Float
asteroidSizeCalc gs = 2 + 0.1 * fromIntegral (difficulty gs)
--asteroidSizeCalc gs = 1

{-
  Zeigt die Verbleibenden Leben an/ game over
-}
drawLivesLeft :: GameState -> Picture 
drawLivesLeft gs = 
  if(livesLeft gs > 0) then
  Color (makeColor 1 1 1 1) (pictures([
    translate (-150) (300) (Scale (0.3) (0.3) (Text "Leben"))
  ] ++ [
    translate (-200) (300) (Scale (0.3) (0.3) (Text (show (livesLeft gs))))
  ]))
  else 
    translate (-200) 0 (Color (makeColor 1 1 1 1) (Scale 0.5 0.5 (Text "Game Over")))