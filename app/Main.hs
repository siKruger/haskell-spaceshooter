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
  if((unsafePerformIO (asteroidSpawnChance gs)) + (difficulty gs * 3)> 295 && length (asteroids gs) < 8 + 1 * fromIntegral (difficulty gs)) then [(unsafePerformIO (asteroidSpawnPos gs), 320)] else []






--haben aste 24x24
-- schiff hat 50x53


{-
  Überprüft die Kollisionen mit einem Punkt


  {-
(1,2) (3,4)
1 geht nach rechts
3 geht nach links

2 geht nach oben
4 geht nach unten

-}
-- 30 15
-}









-- Kollidieren wir mit irgendeinem Asteroiden?
isCollidingWithAsteroideList :: Point -> [Asteroid] -> Int -> Bool
isCollidingWithAsteroideList playerPos asteri diffi = isCollidingWithAsteroideListNum playerPos asteri diffi > 0

-- Mit wievielen Asteroiden kollidieren wir?
isCollidingWithAsteroideListNum :: Point -> [Asteroid] -> Int -> Int
isCollidingWithAsteroideListNum playerPos asteri diffi = length [aste | aste <- asteri, isCollidingAsteroide playerPos aste diffi]

-- Kolledieren wir mit diesem Asteroiden?
isCollidingAsteroide :: Point -> Point -> Int -> Bool 
isCollidingAsteroide p1 p2 diffi = pointInBox p1 (fst p2 * 2 + 50 , snd p2 * 2 + 40) (fst p2 * 2 - 50, snd p2 * 2 - 40) -- Für Difficulty 1

-- Asteroid innerhalb des angezeigten Bereiches?
asteroidInsideGame :: Asteroid -> Bool 
asteroidInsideGame aste = snd aste > -320

-- Geschwindigkeit des Asteroiden
calcAsteroideSpeed :: Int -> Float
calcAsteroideSpeed diffi = asteroidBaseSpeed + 0.2 * fromIntegral diffi

-- Check, ob der Asteroid weiterhin exisitieren soll
renderCheckAsteroide :: Asteroid -> Point -> Int -> Bool
renderCheckAsteroide aste pos diffi = asteroidInsideGame aste && not (isCollidingAsteroide pos aste diffi)


{-
  Updaten des Gamestates!
-}
update :: Float -> GameState -> GameState
update _ gs = 
  if isCollidingWithAsteroideList (position gs) (asteroids gs) (difficulty gs) then
  gs {
    asteroids = [(fst aste, snd aste - calcAsteroideSpeed (difficulty gs)) | aste <- asteroids gs, renderCheckAsteroide aste (position gs) (difficulty gs)] ++ generateAsteroid gs,
    position = movePlayer (direction gs) gs,
    livesLeft = livesLeft gs - isCollidingWithAsteroideListNum (position gs) (asteroids gs) (difficulty gs)
    }
    else
  gs {
    asteroids = [(fst aste, snd aste - calcAsteroideSpeed (difficulty gs)) | aste <- asteroids gs, renderCheckAsteroide aste (position gs) (difficulty gs)] ++ generateAsteroid gs,
    position = movePlayer (direction gs) gs
  }




--Generiert random Positionen für unsere Asteroiden
getRandomNum :: IO Float
getRandomNum =  randomRIO ((0),20) --150 --120?


--Random int zur bestimmung, ob ein Asteroid spawn (decluttern)
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
          , livesLeft = 5000
          --, asteroids = [(300, -300), (300, -200), (300, 0), (300, -100), (300, 200), (300, 300), (300, 400), (300, 500), (300, 600), (00, 50)]
          , asteroids = [(80, 90), (-80, 90), (50, 90), (-50, 90), (20, 80), (-20, 80), (00, 80)] -- SHOULD COLLIDE WITH ALL OF THEM
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
drawAsteroids gs imgs = pictures [scale (asteroidSizeCalc (difficulty gs)) (asteroidSizeCalc (difficulty gs)) (translate (fst aste) (snd aste) (imgs !! 0)) | aste <- asteroids gs]

asteroidSizeCalc :: Int -> Float
asteroidSizeCalc diffi = 2 + 0.1 * fromIntegral diffi
--asteroidSizeCalc gs = 1

{-
  Zeigt die Verbleibenden Leben an/ game over
-}
drawLivesLeft :: GameState -> Picture 
drawLivesLeft gs = 
  if(livesLeft gs > 0) then
  Color (makeColor 1 1 1 1) (pictures([
    translate (0) (300) (Scale (0.3) (0.3) (Text (show (position gs))))
  ] ++ [
    translate (-200) (300) (Scale (0.3) (0.3) (Text (show (livesLeft gs))))
  ]))
  else 
    translate (-200) 0 (Color (makeColor 1 1 1 1) (Scale 0.5 0.5 (Text "Game Over")))