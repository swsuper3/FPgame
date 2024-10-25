-- | This module contains the data types
--   which represent the state of the game
module Model where

import Data.Set (Set, empty, toList)
import Graphics.Gloss.Interface.IO.Game (Key (Char))
import Data.Maybe (catMaybes)
import Data.Ord (clamp)

screenDims :: Dimensions
screenDims = (400, 400)

intScreenDims :: (Int, Int)
intScreenDims = (round a, round b)
  where (a, b) = screenDims

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

-- nO_SECS_BETWEEN_CYCLES :: Float
-- nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   pressedKeys :: Set Key,
                   elapsedTime :: Float,
                   score :: Score,
                   status :: PlayingStatus,
                   paused :: IsPaused,
                   enemies :: AliveEnemies,
                   bullets :: ShotBullets,
                   player :: Player
                 }

initialState :: GameState
initialState = GameState {pressedKeys = empty, elapsedTime = 0, score = Score 0, status = MainMenu, paused = NotPaused, enemies = [],
bullets = [], player = initialPlayer}

initialPlayer :: Player
initialPlayer = Player {playerPosition = Point 0 0, playerDims = (50, 10), playerLives = Lives 3}


--Now the data types we made ourselves:

data Player = Player {playerPosition :: Point, playerDims :: Dimensions, playerLives :: Lives}

data Enemy = Enemy {enemyPosition :: Point, enemyDims :: Dimensions, enemyLives :: Lives}
type AliveEnemies = [Enemy]

data Bullet = Bullet {bulletPosition :: Point, bulletDims :: Dimensions, bulletDirection :: Vector, bulletOwner :: Owner}
type ShotBullets = [Bullet]
data Owner = Friendly | Hostile

newtype Lives = Lives Int
newtype Score = Score Int
type Dimensions = (Float, Float) --width, height

data IsPaused = NotPaused | Paused

data PlayingStatus = MainMenu | LevelMenu | PlayingLevel Level

--TECHNICALLY OPTIONAL
data Wall          = Wall Point BoundingBox
type MovingEnemy   = Enemy
type Walls         = [Wall]
type MovingEnemies = [MovingEnemy]

data Level = Level LevelNr Enemies
type LevelNr = Int
type Enemies = [(Enemy, EnterTime)]
type EnterTime = Int
--END TECHNICALLY OPTIONAL

data Point  = Point Float Float
data Vector = Vector Float Float deriving(Show, Eq, Ord)

type Playtime = Float

data BoundingBox = BoundingBox { 
                    lowerLeft :: Point
                  , width     :: Float
                  , height    :: Float 
                  }


class CanMove a where
  getPos :: a -> Point
  setPos :: a -> Point -> a

move :: CanMove a => a -> Vector -> a
move x (Vector vecx vecy) = setPos x newPoint
  where newPoint = Point (posx + vecx) (posy + vecy)
        (Point posx posy) = getPos x

class HasCollision a where
  getBB :: a -> BoundingBox

intersects :: (HasCollision a, HasCollision b) => a -> b -> Bool
intersects one two = any (`inBox` boxOne) (corners boxTwo) || any (`inBox` boxTwo) (corners boxOne)
  where boxOne = getBB one
        boxTwo = getBB two 

inBox :: Point -> BoundingBox -> Bool
inBox (Point px py) (BoundingBox (Point x y) w h) = and [px >= x, px <= x + w, py >= y, py <= y + h]

corners :: BoundingBox -> [Point]
corners (BoundingBox ll@(Point x y) w h) = [ll, Point (x+w) y, Point (x+h) (y+h), Point x (y+h)]



--Player-related:

instance CanMove Player where
  getPos (Player p _ _) = p
  setPos (Player _ pDims l) q = Player newPos pDims l
    where newPos = Point (clamp (-xBound, xBound) x) (clamp (-yBound, yBound) y)
          (Point x y) = q 
          (screenX, screenY) = screenDims
          xBound = screenX / 2
          yBound = screenY / 2

instance HasCollision Player where
  getBB p = BoundingBox {lowerLeft = playerPosition p, width = w, height = h}
    where (w, h) = playerDims p

moveDirection :: Char -> Vector
moveDirection 'w' = Vector 0 1
moveDirection 'a' = Vector (-1) 0
moveDirection 's' = Vector 0 (-1)
moveDirection 'd' = Vector 1 0
moveDirection _ = Vector 0 0

scalarMult :: Float -> Vector -> Vector
a `scalarMult` (Vector x y) = Vector (a*x) (a*y)

vectorSum :: Vector -> Vector -> Vector
vectorSum (Vector a b) (Vector c d) = Vector (a + c) (b + d)

--This function figures out how what direction to move the player in depending on the pressed keys.
getPlayerMovementVector :: Set Key -> Vector
getPlayerMovementVector pressedKeys = foldr vectorSum (Vector 0 0) vectors
  where chars = map extractCharacter (toList pressedKeys)
        parsedChars = catMaybes chars
        vectors = map moveDirection parsedChars

extractCharacter :: Key -> Maybe Char
extractCharacter (Char c) = Just c
extractCharacter _ = Nothing
