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
    , speedX :: Float
    , speedY :: Float
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
    (fst (position gs) + speedX gs, snd (position gs))
    else
      (fst (position gs), snd (position gs))
  
movePlayerProc West gs =
  if(isCollisionWithGameWallLeft (fst (position gs), snd (position gs))) 
    then
      (fst (position gs) + speedX gs * (-1), snd (position gs))
    else
      (fst (position gs), snd (position gs))

movePlayerProc North gs =
  if(isCollisionWithGameWallUp (fst (position gs), snd (position gs)))
    then
      (fst (position gs), snd (position gs) + speedY gs * (-1))
    else 
      (fst (position gs), snd (position gs))


movePlayerProc South gs =
  if(isCollisionWithGameWallDown (fst (position gs), snd (position gs)))
    then
      (fst (position gs), snd (position gs) + speedY gs * 1)
    else 
      (fst (position gs), snd (position gs))
      
movePlayerProc _ gs = (fst (position gs), snd (position gs))



{-
  Asteroiden erstellen wir hier
-}
generateAsteroid :: GameState -> [Asteroid]
generateAsteroid gs = 
  --if((unsafePerformIO (asteroidSpawnChance gs)) * (difficulty gs)> 295) then
  if((unsafePerformIO (asteroidSpawnChance gs)) + (difficulty gs * 3)> 295) then
       [(unsafePerformIO (asteroidSpawnPos gs), 320)]
    else 
      []


{-
  Kollisionsabfrage zwischen Spieler und Asteroiden

  TODO: gute werte für hitboxe (PAIN)
  Asteroid löschen, nachdem wir kollidiert sind (PAIN ^2)
-}
hasCollisionWithAsteroidWithSpawn :: GameState -> GameState
hasCollisionWithAsteroidWithSpawn gs = 
  if(isCollidingWithAsteroideList gs) then
   gs { 
          speedY = -5,
          speedX = 5, 
          position = movePlayer (direction gs) gs, 
          asteroids = [(fst aste, snd aste - (0.8 +  0.2 * fromIntegral (difficulty gs))) | aste <- asteroids gs, snd aste > -320 && not (isCollidingWithAsteroide aste (position gs))] ++ generateAsteroid gs,
          livesLeft = (livesLeft gs - 1)
        }
    else
    gs { 
          speedY = -5,
          speedX = 5, 
          position = movePlayer (direction gs) gs, 
          asteroids = [(fst aste, snd aste - (0.8 +  0.2 * fromIntegral (difficulty gs))) | aste <- asteroids gs, snd aste > -320 && not (isCollidingWithAsteroide aste (position gs))] ++ generateAsteroid gs
        }


{-
  Überprüft die Kollision mit allen Asteroiden
-}
isCollidingWithAsteroideList :: GameState -> Bool
isCollidingWithAsteroideList gs = length [pos | pos <- asteroids gs, isCollidingWithAsteroide pos (position gs)] > 0


{-
  Überprüft die Kollisionen mit einem Punkt
-}

collisionSize :: Float
collisionSize = 20

isCollidingWithAsteroide :: Point -> Point -> Bool 
isCollidingWithAsteroide playerPos astePos = pointInBox (fst playerPos, snd playerPos) (fst astePos + collisionSize, snd astePos + collisionSize) (fst astePos + (-1) * collisionSize, snd astePos + (-1) * collisionSize)




hasCollisionWithAsteroid :: GameState -> GameState
hasCollisionWithAsteroid gs = 
  if(isCollidingWithAsteroideList gs) then
        gs { 
          speedY = -5,
          speedX = 5,
          position = movePlayer (direction gs) gs, 
          asteroids = [(fst aste, snd aste - (0.8 +  0.2 * fromIntegral (difficulty gs))) | aste <- asteroids gs, snd aste > -320 && not (isCollidingWithAsteroide aste (position gs))],
          livesLeft = (livesLeft gs - 1)
        }
    else
      gs { 
          speedY = -5,
          speedX = 5,
          position = movePlayer (direction gs) gs, 
          asteroids = [(fst aste, snd aste - (0.8 + 0.2 * fromIntegral (difficulty gs))) | aste <- asteroids gs, snd aste > -320 && not (isCollidingWithAsteroide aste (position gs))]
        }



-- Update Funktion
update :: Float -> GameState -> GameState
update _ gs = 
  if(length (asteroids gs) < 8 + 1 * fromIntegral (difficulty gs)) then --Anzahl der Asteroids
     hasCollisionWithAsteroidWithSpawn gs
     else
     hasCollisionWithAsteroid gs


{-
  Generiert random Positionen für unsere Asteroiden
-}
getRandomNum :: IO Float
getRandomNum =  randomRIO ((-120),120) --150

getRandomInt :: IO Int
getRandomInt = randomRIO(0, 300)


main :: IO ()
main = do
  asteroidImg <- loadBMP "assets/asteroid.bmp"
  spaceshipImg <- loadBMP "assets/SpaceshipTest1.bmp"
  let state =
        GameState
          { position = (0.0, 0.0)
          , direction = None
          , livesLeft = 5
          , speedX = 0
          , speedY = 0
          , asteroids = [(300, -300), (300, -200), (300, 0), (300, -100), (300, 200), (300, 300), (300, 400), (300, 500), (300, 600), (00, 50)]
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