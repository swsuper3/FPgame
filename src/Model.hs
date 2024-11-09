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
                   player :: Player,
                   playtime :: Playtime
                 }

initialState :: GameState
initialState = GameState {pressedKeys = empty, elapsedTime = 0, score = Score 0, status = MainMenu, paused = Paused, enemies = [dummyEnemy],
bullets = [], player = initialPlayer, playtime = 0}

initialPlayer :: Player
initialPlayer = Player {playerPosition = Point 0 0, playerDims = (50, 10), playerLives = Lives 3}

dummyEnemy :: Enemy
dummyEnemy = Enemy {enemyPosition = Point (0.6 * w) 0, enemyDims = (10, 10), enemyLives = Lives 1}
  where (w, h) = screenDims


--Now the data types we made ourselves:

data Player = Player {playerPosition :: Point, playerDims :: Dimensions, playerLives :: Lives}

data Enemy = Enemy {enemyPosition :: Point, enemyDims :: Dimensions, enemyLives :: Lives}
type AliveEnemies = [Enemy]

data Bullet = Bullet {bulletPosition :: Point, bulletDims :: Dimensions, bulletDirection :: Vector, bulletOwner :: Owner, bulletLives :: Lives}
type ShotBullets = [Bullet]
data Owner = Friendly | Hostile

newtype Lives = Lives Int deriving (Show, Eq, Ord)
newtype Score = Score Int
type Dimensions = (Float, Float) --width, height

data IsPaused = NotPaused | Paused
  deriving Eq

data PlayingStatus = MainMenu | LevelMenu | PlayingLevel Level

--TECHNICALLY OPTIONAL
data Wall          = Wall Point BoundingBox
type MovingEnemy   = Enemy
type Walls         = [Wall]
type MovingEnemies = [MovingEnemy]

data Level = Level LevelNr Enemies
type LevelNr = Int
type Enemies = [(Enemy, EnterTime, SpawnStatus)]
data SpawnStatus = Upcoming | Spawning | Spawned
  deriving Eq
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
corners (BoundingBox ll@(Point x y) w h) = [ll, Point (x+w) y, Point (x+w) (y+h), Point x (y+h)]

class HasCollision a => CanHurtPlayer a where
  hurtSelf :: a -> a        --Remove a life from the enemy/bullet that hurt the player
  clearDeads :: [a] -> [a]  --Clear all the enemies/bullets with zero lives

--Player-related:

instance CanMove Player where
  getPos (Player p _ _) = p
  setPos (Player _ pDims l) q = Player newPos pDims l
    where newPos = Point (clamp (-xBound, xBound) x) (clamp (-yBound, yBound) y)
          (Point x y) = q 
          (screenX, screenY) = screenDims
          (playerWidth, playerHeight) = pDims
          xBound = (screenX / 2) - (playerWidth / 2)
          yBound = (screenY / 2) - (playerHeight / 2)

instance HasCollision Player where
  getBB p = BoundingBox {lowerLeft = lowerLeftPosition, width = w, height = h}
    where (w, h) = playerDims p
          Point x y = playerPosition p
          lowerLeftPosition = Point (x - 0.5*w) (y - 0.5*h)

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

loseLife :: Player -> Player
loseLife p = p {playerLives = Lives(n - 1)}
  where (Lives n) = playerLives p

--Enemy-related:

instance CanMove Enemy where
  getPos = enemyPosition
  setPos e p = e {enemyPosition = p}

instance HasCollision Enemy where
  getBB e = BoundingBox {lowerLeft = lowerLeftPosition, width = w, height = h}
    where (w, h) = enemyDims e
          Point x y = enemyPosition e
          lowerLeftPosition = Point (x - 0.5*w) (y - 0.5*h)

instance CanHurtPlayer Enemy where
  hurtSelf e = e {enemyLives = newLives}
    where newLives = Lives (oldLives - 1)
          (Lives oldLives) = enemyLives e
  clearDeads [] = []
  clearDeads (x:xs) = case (enemyLives x) of
                    Lives 0 -> clearDeads xs
                    _ -> x : clearDeads xs

--Bullet-related

instance CanMove Bullet where
  getPos = bulletPosition
  setPos b p = b {bulletPosition = p}

instance HasCollision Bullet where
  getBB b = BoundingBox {lowerLeft = lowerLeftPosition, width = w, height = h}
    where (w, h) = bulletDims b
          Point x y = bulletPosition b
          lowerLeftPosition = Point (x - 0.5*w) (y - 0.5*h)

instance CanHurtPlayer Bullet where
  hurtSelf b = b {bulletLives = newLives}
    where newLives = Lives (oldLives - 1)
          Lives oldLives = bulletLives b
  clearDeads [] = []
  clearDeads (x:xs) = case (bulletLives x) of
                      Lives 0 -> clearDeads xs
                      _ -> x : clearDeads xs

friendlyBullet :: Player -> Bullet
friendlyBullet p = Bullet {bulletPosition = Point (playerX + (0.5 * playerWidth)) playerY, bulletDims = (5, 5), bulletDirection = Vector 1 0, bulletOwner = Friendly, bulletLives = Lives 1}
  where Point playerX playerY = playerPosition p
        (playerWidth, _) = playerDims p


-- Switching playingStatus
toggleStatus :: GameState -> PlayingStatus -> GameState
toggleStatus gstate newStatus = case newStatus of PlayingLevel _ -> gstate {status = newStatus, paused = NotPaused, enemies = [], playtime = 0}
                                                  _              -> gstate {status = newStatus, paused = Paused}


-- Playtime functionality:
updatePlaytime :: GameState -> Float -> Playtime
updatePlaytime gstate secs
  | (paused gstate) == Paused   = playtime gstate
  | otherwise                   = (playtime gstate) + secs

togglePause :: IsPaused -> IsPaused
togglePause NotPaused = Paused
togglePause Paused    = NotPaused
