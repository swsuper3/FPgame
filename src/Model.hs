-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

-- nO_SECS_BETWEEN_CYCLES :: Float
-- nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   elapsedTime :: Float,
                   score :: Score,
                   status :: PlayingStatus,
                   paused :: IsPaused,
                   enemies :: AliveEnemies,
                   bullets :: ShotBullets,
                   player :: Player
                 }

initialState :: GameState
initialState = GameState 0 (Score 0) MainMenu NotPaused [] [] (Player {playerPosition = Point 0 0, playerDims = (50, 10), playerLives = Lives 3})


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
data Vector = Vector Float Float

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

intersects :: HasCollision a => a -> a -> Bool
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
  setPos (Player p bb l) q = Player q bb l

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