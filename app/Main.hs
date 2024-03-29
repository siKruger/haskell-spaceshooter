module Main where

{-
  +++++ Imports +++++
-}

import Data.Char (GeneralCategory (Space))
import Data.Fixed (Pico)
import Data.Int
import Data.Semigroup (diff)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, DiffTime, NominalDiffTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Time.LocalTime (TimeOfDay)
import Graphics.Gloss
import Graphics.Gloss.Data.Point (pointInBox)
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe
import System.Random (Random (randomR), getStdGen, randomRIO)
import Text.Read.Lex (Number)

{-
  +++++ Asteroiden Logik +++++
-}

-- Updaten der Asteroiden-pos
updateAsteroidPos :: [Asteroid] -> Int -> [LaserShot] -> Point -> IO Float -> IO Int -> Int -> [Asteroid]
updateAsteroidPos asteroids diffi lasers position asteroidSpawnPos asteroidSpawnChance livesLeft = [(fst aste, snd aste - calcAsteroideSpeed diffi) | aste <- asteroids, renderCheckAsteroide aste position diffi lasers livesLeft] ++ generateAsteroid asteroidSpawnPos asteroidSpawnChance diffi asteroids

-- Erstellung eines Asteroiden, sofern RNG und Anzahl passt.
generateAsteroid :: IO Float -> IO Int -> Int -> [Asteroid] -> [Asteroid]
--generateAsteroid gs = if(unsafePerformIO (asteroidSpawnChance gs)) + (difficulty gs * 3)> 295 && length (asteroids gs) < 8 + 1 * fromIntegral (difficulty gs) then [(unsafePerformIO (asteroidSpawnPos gs), 320)] else []
generateAsteroid asteroidSpawnPos asteroidSpawnChance difficulty asteroids = [(unsafePerformIO asteroidSpawnPos, 320) | unsafePerformIO asteroidSpawnChance + (difficulty * 3) > 295 && length asteroids < 8 + 1 * fromIntegral difficulty]

-- Kollidieren wir mit irgendeinem Asteroiden?
isCollidingWithAsteroideList :: Point -> [Asteroid] -> Int -> Int -> Bool
isCollidingWithAsteroideList playerPos asteri diffi livesLeft = isCollidingWithAsteroideListNum playerPos asteri diffi livesLeft > 0

-- Mit wievielen Asteroiden kollidieren wir?
isCollidingWithAsteroideListNum :: Point -> [Asteroid] -> Int -> Int -> Int
isCollidingWithAsteroideListNum playerPos asteri diffi livesLeft = length [aste | aste <- asteri, isCollidingAsteroide playerPos aste diffi livesLeft]

-- Kolledieren wir mit diesem Asteroiden?
isCollidingAsteroide :: Point -> Point -> Int -> Int -> Bool
--isCollidingAsteroide p1 p2 diffi = pointInBox p1 (fst p2 * 2 + 50 , snd p2 * 2 + 40) (fst p2 * 2 - 50, snd p2 * 2 - 40) -- Für Difficulty 1
isCollidingAsteroide p1 p2 diffi livesLeft = pointInBox p1 (fst p2 * asteroidSizeCalc diffi + 50, snd p2 * asteroidSizeCalc diffi + 40) (fst p2 * asteroidSizeCalc diffi - 50, snd p2 * asteroidSizeCalc diffi - 40) && livesLeft > 0 -- Für Difficulty 1

-- Asteroid innerhalb des angezeigten Bereiches?
asteroidInsideGame :: Asteroid -> Bool
asteroidInsideGame aste = snd aste > -320

-- Geschwindigkeit des Asteroiden
calcAsteroideSpeed :: Int -> Float
calcAsteroideSpeed diffi = asteroidBaseSpeed + 0.2 * fromIntegral diffi

-- Check, ob der Asteroid weiterhin exisitieren soll
renderCheckAsteroide :: Asteroid -> Point -> Int -> [LaserShot] -> Int -> Bool
renderCheckAsteroide aste pos diffi lasers livesLeft = asteroidInsideGame aste && not (isCollidingAsteroide pos aste diffi livesLeft) && not (isCollidingWithLaser aste lasers diffi)

-- Check, ob der Asteroid vom Laser getroffen wurde
isCollidingWithLaser :: Asteroid -> [LaserShot] -> Int -> Bool
isCollidingWithLaser asteroid laser diffi = not (null ([lase | lase <- laser, asteroidLaserCollision asteroid lase diffi]))

-- Kollision zwischen Laser und Aste?
asteroidLaserCollision :: Asteroid -> LaserShot -> Int -> Bool
asteroidLaserCollision aste las diffi = pointInBox las (fst aste * asteroidSizeCalc diffi + 20, snd aste * asteroidSizeCalc diffi + 40) (fst aste * asteroidSizeCalc diffi - 20, snd aste * asteroidSizeCalc diffi - 40)

{-
  +++++ Laser Logik +++++
-}

-- Updaten der Laserposition
updateLaserPos :: [LaserShot] -> MoveDirection -> Point -> [Asteroid] -> Int -> Int -> [LaserShot]
updateLaserPos laser moveDir playerPos asteroids diffi livesLeft = [(fst las, snd las + 6) | las <- laser, not (checkLaserAsteroidCollision asteroids las diffi)] ++ checkLaserSpawn moveDir playerPos livesLeft

-- Kollisionsabfrage zwischen Laser und Asteroid
checkLaserAsteroidCollision :: [Asteroid] -> LaserShot -> Int -> Bool
checkLaserAsteroidCollision asteroids laserShot diffi = not (null ([aste | aste <- asteroids, asteroidLaserCollision aste laserShot diffi]))

-- Laser Spawn
checkLaserSpawn :: MoveDirection -> Point -> Int -> [LaserShot]
checkLaserSpawn dir playerPos livesLeft
  | dir == Shoot && livesLeft > 0 = [playerPos]
  | otherwise = []

{-
  +++++ Bewegen Logik +++++
-}

-- Bewegungsrichtung "löschen" bei Schuss
clearMoveDir :: MoveDirection -> MoveDirection
clearMoveDir dir
  | dir == Shoot = None
  | otherwise = dir

-- Bewegt den Spieler, wenn mehr als 0 Leben
movePlayer :: MoveDirection -> Point -> Int -> Point
movePlayer dir playerPos livesLeft =
  if livesLeft > 0
    then movePlayerProc dir playerPos
    else (0, -50)

-- Bewegt den Spieler, falls keine Kollision
movePlayerProc :: MoveDirection -> Point -> Point
movePlayerProc moveDir playerPos =
  if isWallCollision moveDir playerPos
    then calcNewPlayerPos moveDir playerPos
    else playerPos

-- Berechnen der neuen Position
calcNewPlayerPos :: MoveDirection -> Point -> Point
calcNewPlayerPos North pos = (fst pos, snd pos + 5)
calcNewPlayerPos East pos = (fst pos + 5, snd pos)
calcNewPlayerPos South pos = (fst pos, snd pos - 5)
calcNewPlayerPos West pos = (fst pos - 5, snd pos)
calcNewPlayerPos _ pos = pos

-- Kollisionsabfrage mit der Wand
isWallCollision :: MoveDirection -> Point -> Bool
isWallCollision dir pos
  | dir == East = fst pos < 200
  | dir == West = fst pos > -200
  | dir == North = snd pos < 300
  | dir == South = snd pos > -300
  | otherwise = False

{-
  +++++ Schwierigkeit Update Logik +++++
-}

-- Updated alle 30 Sekunden die Schwierigkeit
updateDifficulty :: UTCTime -> IO UTCTime -> Int -> Int -> Int
updateDifficulty time currentTime diff livesLeft =
  if diffUTCTime (unsafePerformIO currentTime) time >= 30 && livesLeft > 0
    then diff + 1
    else diff

-- Updated die Zeit, bei der das letze mal geändert wurde.
updateLastTime :: UTCTime -> IO UTCTime -> UTCTime
updateLastTime lastTime currentTime =
  if diffUTCTime (unsafePerformIO currentTime) lastTime > 30.01
    then unsafePerformIO currentTime
    else lastTime


-- Update die Zeit, die wir überlebt haben
finalizeSurviveTime :: UTCTime -> IO UTCTime -> Int -> NominalDiffTime -> NominalDiffTime
finalizeSurviveTime startedTime currentTime livesLeft currentSurvive = if livesLeft > 0 then diffUTCTime (unsafePerformIO currentTime) startedTime else currentSurvive

{-
  +++++ Gloss Logik +++++
-}

-- Update Funktion
update :: Float -> GameState -> GameState
update _ gs =
  gs
    { asteroids = updateAsteroidPos (asteroids gs) (difficulty gs) (lasers gs) (position gs) (asteroidSpawnPos gs) (asteroidSpawnChance gs) (livesLeft gs),
      position = movePlayer (direction gs) (position gs) (livesLeft gs),
      livesLeft = livesLeft gs - isCollidingWithAsteroideListNum (position gs) (asteroids gs) (difficulty gs) (livesLeft gs),
      lasers = updateLaserPos (lasers gs) (direction gs) (position gs) (asteroids gs) (difficulty gs) (livesLeft gs),
      direction = clearMoveDir (direction gs),
      difficulty = updateDifficulty (lastLevelChange gs) (currentTime gs) (difficulty gs) (livesLeft gs),
      lastLevelChange = updateLastTime (lastLevelChange gs) (currentTime gs),
      timeSurvived = finalizeSurviveTime (gameStart gs) (currentTime gs) (livesLeft gs) (timeSurvived gs)
    }

-- Render Funktion
render :: GameState -> [Picture] -> Picture
render gs imgs =
  pictures
    ( [ uncurry
          translate
          (position gs)
          (imgs !! 1) -- Position 2 des render arrays stellt den Spieler da
      ]
        ++ [ drawLivesLeft gs
           ]
        ++ [ drawAsteroids gs imgs
           ]
        ++ [ drawLasers gs imgs
           ]
    )

-- Main Funktion
main :: IO ()
main = do
  asteroidImg <- loadBMP "assets/Asteroid.bmp"
  spaceshipImg <- loadBMP "assets/SpaceshipTest1.bmp"
  laserImg <- loadBMP "assets/laser.bmp"
  let state =
        GameState
          { position = (0.0, 0.0),
            direction = None,
            livesLeft = 5,
            asteroids = [(300, -300), (300, -200), (300, 0), (300, -100), (300, 200), (300, 300), (300, 400), (300, 500), (300, 600)],
            difficulty = 1,
            asteroidSpawnPos = getRandomNum,
            asteroidSpawnChance = getRandomInt,
            lasers = [],
            lastLevelChange = unsafePerformIO getCurrentTime,
            currentTime = getCurrentTime,
            gameStart = unsafePerformIO getCurrentTime,
            timeSurvived = diffUTCTime (unsafePerformIO (currentTime state)) (gameStart state)
          }
  play
    window
    background
    fps
    state
    (`render` [asteroidImg, spaceshipImg, laserImg])
    handleKeys
    update

{-
  +++++ Render related Stuff +++++
-}

-- Laser darstellen
drawLasers :: GameState -> [Picture] -> Picture
--drawLasers gs imgs = pictures [translate (fst las) (snd las) (imgs !! 2) | las <- lasers gs]
drawLasers gs imgs = pictures [uncurry translate las (imgs !! 2) | las <- lasers gs]

-- Lebensanzeige
drawLivesLeft :: GameState -> Picture
drawLivesLeft gs =
  if livesLeft gs > 0
    then
      Color
        (makeColor 1 1 1 1)
        ( pictures
            ( translate (-150) 300 (Scale 0.3 0.3 (Text "Leben")) :
              [ translate (-200) 300 (Scale 0.3 0.3 (Text (show (livesLeft gs))))
              ]
            )
        )
    else
      pictures
        ( [ translate (-200) 0 (Color (makeColor 1 1 1 1) (Scale 0.5 0.5 (Text "Game Over")))
          ]
            ++ [ translate (-150) (-100) (Color (makeColor 1 1 1 1) (Scale 0.3 0.3 (Text "Level:")))
               ]
            ++ [ translate 50 (-100) (Color (makeColor 1 1 1 1) (Scale 0.3 0.3 (Text (show (difficulty gs)))))
               ]


            ++ [ translate (-150) (-200) (Color (makeColor 1 1 1 1) (Scale 0.3 0.3 (Text "Zeit:")))
               ]
            ++ [ translate (-50) (-200) (Color (makeColor 1 1 1 1) (Scale 0.3 0.3 (Text (take 6 (show (timeSurvived gs))))))
               ]
            ++ [ translate 80 (-200) (Color (makeColor 1 1 1 1) (Scale 0.3 0.3 (Text "s")))
               ]
        )

-- Zeichnet die Asteroiden
drawAsteroids :: GameState -> [Picture] -> Picture
drawAsteroids gs imgs = pictures [scale (asteroidSizeCalc (difficulty gs)) (asteroidSizeCalc (difficulty gs)) (uncurry translate aste (head imgs)) | aste <- asteroids gs]

-- Größe der Asteroiden
asteroidSizeCalc :: Int -> Float
asteroidSizeCalc diffi = 2 + 0.1 * fromIntegral diffi

{-
  +++++ Monden misc +++++
-}

--Generiert random Positionen für unsere Asteroiden
getRandomNum :: IO Float
getRandomNum = randomRIO (-120, 120) --150 --120?

--Random int zur bestimmung, ob ein Asteroid spawn (decluttern)
getRandomInt :: IO Int
getRandomInt = randomRIO (0, 300)

{-
  +++++ Konstanten misc +++++
-}

asteroidBaseSpeed :: Float
asteroidBaseSpeed = 0.8 --0.8

window :: Display
window = InWindow "Space Shooter" (500, 700) (50, 50)

background :: Color
background = makeColor 0.033 0.04 0.26 1

fps :: Int
fps = 60

{-
  +++++ Typen +++++
-}

data MoveDirection = East | West | North | South | Shoot | None
  deriving (Eq)

type Asteroid = Point

type LaserShot = Point

data GameState = GameState
  { position :: Point,
    direction :: MoveDirection,
    livesLeft :: Int,
    difficulty :: Int,
    asteroids :: [Asteroid],
    asteroidSpawnPos :: IO Float,
    asteroidSpawnChance :: IO Int,
    lasers :: [LaserShot],
    lastLevelChange :: UTCTime,
    currentTime :: IO UTCTime,
    gameStart :: UTCTime,
    timeSurvived :: NominalDiffTime
  }

{-
  +++++ Input zeugs +++++
-}

-- Key Input abarbeiten und handeln
handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs = gs {direction = West}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs = gs {direction = East}
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gs = gs {direction = North}
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gs = gs {direction = South}
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gs = gs {direction = Shoot}
handleKeys _ gs = gs {direction = None}