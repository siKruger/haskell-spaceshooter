{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed (Pico)

data MoveDirection = East | West | North | South | None
  deriving (Eq)

type CellType = Char

type Cell = (Point, CellType)

type Level = [Cell]

type Asteroid = Point

data GameState =
  GameState
    { position :: Point
    , direction :: MoveDirection
    , currentLevel :: Level
    , livesLeft :: Int
    , speedX :: Float
    , speedY :: Float
    , asteroids :: [Asteroid]
    }


tileSize :: Float
tileSize = 32.0

window :: Display
window = InWindow "Space Shooter" (500, 700) (50, 50)

background :: Color
background = makeColor 0.033 0.04 0.26 1

fps :: Int
fps = 60

-- Eine Zeile bauen
makeRow :: String -> Int -> Level
makeRow row y =
  [ ( ( (fromIntegral x * tileSize) - ((1024 / 2) - (tileSize / 2))
      , (fromIntegral y * tileSize) - ((768 / 2) - (tileSize / 2)))
    , row !! x)
  | x <- [0 .. length row - 1]
  , row !! x == '*' || row !! x == '%'
  ]

-- Das Feld an sich bauen
prepareData :: [String] -> Level
prepareData rawData =
  concat [makeRow (rawData !! y) y | y <- [0 .. length rawData - 1]]

-- Entscheide ob Food oder Wand
whatImg :: Cell -> Picture -> Picture -> Picture
whatImg (_, cellType) tile food =
  if cellType == '*'
    then tile
    else food

-- Tile zeichnen
drawTile :: Cell -> Picture -> Picture -> Picture
drawTile cell tileImg foodImg =
  uncurry translate (fst cell) (whatImg cell tileImg foodImg)




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


-- Update Funktion
update :: Float -> GameState -> GameState
update _ gs =
  gs
    { speedY = -5
    , speedX = 5
    , position = movePlayer (direction gs) gs
    , asteroids = [(fst aste, snd aste - 1) | aste <- asteroids gs]
    }

main :: IO ()
main = do
  tileImg <- loadBMP "assets/tile.bmp"
  foodImg <- loadBMP "assets/food.bmp"
  left1 <- loadBMP "assets/left1.bmp"
  rawData <- readFile "assets/level"
  let level = prepareData $ reverse $ lines rawData
  let state =
        GameState
          { position = (0.0, 0.0)
          , direction = None
          , currentLevel = level
          , livesLeft = 5
          , speedX = 0
          , speedY = 0
          , asteroids = [(50, 50)]
          }
  play
    window
    background
    fps
    state
    (`render` [ tileImg
              , foodImg
              , left1
              ])
    handleKeys
    update


-- render Methode von Gloss
render :: GameState -> [Picture] -> Picture
render gs imgs = pictures
    ([drawTile cell (head imgs) (imgs !! 1) | cell <- currentLevel gs
    ] ++ [ translate
         (fst (position gs))
         (snd (position gs))
         (imgs !! 2) -- Position 2 des render arrays stellt den Spieler da
     ] ++ [
       drawLivesLeft gs
       ] ++ [
         drawAsteroids gs imgs
       ])

{-
  Liefert alle Asteroiden als Picture
-}
drawAsteroids :: GameState -> [Picture] -> Picture
drawAsteroids gs imgs = pictures [(translate (fst aste) (snd aste) (imgs !! 1)) | aste <- asteroids gs]


{-
  Zeigt die Verbleibenden Leben an. 
-}
drawLivesLeft :: GameState -> Picture 
drawLivesLeft gs = Color (makeColor 1 1 1 1) (pictures([
    translate (-150) (300) (Scale (0.3) (0.3) (Text "Leben"))
  ] ++ [
    translate (-200) (300) (Scale (0.3) (0.3) (Text (show (livesLeft gs))))
  ]))