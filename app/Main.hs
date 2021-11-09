module Main where

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed (Pico)
import System.IO.Unsafe
import Text.Read.Lex (Number)

import Data.Int
import System.Random (randomRIO, getStdGen, Random (randomR))

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
    }

windowHeight :: Int
windowHeight = 700

windowWidth :: Int
windowWidth = 500


tileSize :: Float
tileSize = 32.0

window :: Display
window = InWindow "Space Shooter" (windowWidth, windowHeight) (50, 50)

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
fst, snd geben das erste/letzte Element einer Liste zurück
-}
movePlayer :: MoveDirection -> GameState -> Point
movePlayer East gs = 
  if(isCollisionWithGameWallRight (fst (position gs), snd (position gs))) 
    then  
    (fst (position gs) + speedX gs, snd (position gs))
    else
      (fst (position gs), snd (position gs))
  
movePlayer West gs =
  if(isCollisionWithGameWallLeft (fst (position gs), snd (position gs))) 
    then
      (fst (position gs) + speedX gs * (-1), snd (position gs))
    else
      (fst (position gs), snd (position gs))

movePlayer North gs =
  if(isCollisionWithGameWallUp (fst (position gs), snd (position gs)))
    then
      (fst (position gs), snd (position gs) + speedY gs * (-1))
    else 
      (fst (position gs), snd (position gs))


movePlayer South gs =
  if(isCollisionWithGameWallDown (fst (position gs), snd (position gs)))
    then
      (fst (position gs), snd (position gs) + speedY gs * 1)
    else 
      (fst (position gs), snd (position gs))
      
movePlayer _ gs = (fst (position gs), snd (position gs))



{-
  Asteroiden erstellen wir hier
-}
generateAsteroid :: Int -> GameState -> [Asteroid]
generateAsteroid 0 _ = []
generateAsteroid n gs = [(
  unsafePerformIO (asteroidSpawnPos gs)
  ,50)] ++ generateAsteroid (n-1) gs






-- Update Funktion
update :: Float -> GameState -> GameState
update _ gs = 
  if(length (asteroids gs) < 5) then --Anzahl der Asteroids
     gs { speedY = -5, speedX = 5, position = movePlayer (direction gs) gs, 
     asteroids = [(fst aste, snd aste - 1) | aste <- asteroids gs, snd aste > (fromIntegral windowHeight) * (-1) + 300] ++ generateAsteroid  3 gs}
     else
      gs { speedY = -5, speedX = 5, position = movePlayer (direction gs) gs, 
      asteroids = [(fst aste, snd aste - 1) | aste <- asteroids gs, snd aste > (fromIntegral windowHeight) * (-1) + 300]}


{-
  Generiert random Positionen für unsere Asteroiden
-}
getRandomNum :: IO Float
getRandomNum =  randomRIO ((-230),230)


main :: IO ()
main = do
  foodImg <- loadBMP "assets/food.bmp"
  left1 <- loadBMP "assets/left1.bmp"
  let state =
        GameState
          { position = (0.0, 0.0)
          , direction = None
          , livesLeft = 5
          , speedX = 0
          , speedY = 0
          , asteroids = [(50, 50)]
          , difficulty = 1
          , asteroidSpawnPos = getRandomNum
          }
  play
    window
    background
    fps
    state
    (`render` [foodImg, left1])
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
drawAsteroids gs imgs = pictures [(translate (fst aste) (snd aste) (imgs !! 0)) | aste <- asteroids gs]


{-
  Zeigt die Verbleibenden Leben an. 
-}
drawLivesLeft :: GameState -> Picture 
drawLivesLeft gs = Color (makeColor 1 1 1 1) (pictures([
    translate (-150) (300) (Scale (0.3) (0.3) (Text "Leben"))
  ] ++ [
    translate (-200) (300) (Scale (0.3) (0.3) (Text (show (livesLeft gs))))
  ]))