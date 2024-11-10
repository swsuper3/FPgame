-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Set (insert, delete, empty)
import Debug.Trace (trace)
import Data.Maybe (catMaybes)
import Data.List (partition)
import Data.Char

-- -- | Handle one iteration of the game
-- step :: Float -> GameState -> IO GameState
-- step secs gstate
--   | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
--   = -- We show a new random number
--     do randomNumber <- randomIO
--        let newNumber = abs randomNumber `mod` 10
--        return $ GameState (ShowANumber newNumber) 0
--   | otherwise
--   = -- Just update the elapsed time
--     return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | (gameEnd gstate) && ((playtime gstate) > 5) -- If the game just finished, set the playtime to 0
    = return $ gstate { elapsedTime = elapsedTime gstate + secs,
                        playtime = 0,
                        paused = Paused
                      }
  | (gameEnd gstate) && ((playtime gstate) > 3) -- If the game is finished and it has been more than 3 seconds, go back to LevelMenu
    = return $ gstate { elapsedTime = elapsedTime gstate + secs,
                        paused = Paused,
                        status = LevelMenu,
                        gameEnd = False
                      }
  | (gameEnd gstate)                            -- If the game is finished
    = return $ gstate { elapsedTime = elapsedTime gstate + secs,
                        playtime = playtime gstate + secs,
                        paused = Paused,
                        animations = newAnimations ++ stepAnimations (animations gstate)
                      }
  | (paused gstate) == Paused
    = return $ gstate { elapsedTime = elapsedTime gstate + secs
                      }
  | otherwise
    = do seed <- randomIO
         return $ checkedCollisionGstate { elapsedTime = elapsedTime checkedCollisionGstate + secs,
                                        player = stepPlayer checkedCollisionGstate,
                                        enemies = stepEnemies checkedCollisionGstate secs,
                                        status = stepStatus checkedCollisionGstate,
                                        bullets = newBullets ++ stepBullets checkedCollisionGstate,
                                        playtime = stepPlaytime gstate secs,
                                        gameEnd = stepGameEnd gstate,
                                        generator = mkStdGen seed,
                                        animations = newAnimations ++ stepAnimations (animations gstate)
                                        }
      where checkedCollisionGstate = collision gstate
            newBullets = addedBullets (enemies checkedCollisionGstate) (player checkedCollisionGstate)
            newAnimations = map (explodeAnimation . getPos) (filter (`notElem` enemies checkedCollisionGstate) (enemies gstate))

stepPlayer :: GameState -> Player
stepPlayer gstate = move (player gstate) (10 `scalarMult` (getPlayerMovementVector (pressedKeys gstate)))

collision :: GameState -> GameState
collision = checkCollisionPlayer . friendlyBulletCollision

checkCollisionPlayer :: GameState -> GameState
checkCollisionPlayer gstate = gstate {player = newPlayer', enemies = withoutDeadEnemies, bullets = withoutDeadBullets}
  where (newEnemies, newPlayer) = hostileCollisionCheck (enemies gstate) (player gstate)
        (newBullets, newPlayer') = hostileCollisionCheck hostileBullets newPlayer
        (hostileBullets, friendlyBullets) = partition (\b -> (bulletOwner b) == Hostile) (bullets gstate)
        withoutDeadEnemies = clearDeads newEnemies
        withoutDeadBullets = friendlyBullets ++ clearDeads newBullets

friendlyBulletCollision :: GameState -> GameState
friendlyBulletCollision gstate = gstate {enemies = withoutDeadEnemies, bullets = withoutDeadBullets}
  where (newBullets, newEnemies) = multiBulletCollision (bullets gstate) (enemies gstate)
        withoutDeadEnemies = clearDeads newEnemies
        withoutDeadBullets = clearDeads newBullets

multiBulletCollision :: ShotBullets -> AliveEnemies -> (ShotBullets, AliveEnemies)
multiBulletCollision bulletList enemyList = foldr f ([], enemyList) bulletList
  where f b (bs, es) = 
                  let (checkedBullet, checkedEnemies) = singleBulletCollision b es
                  in (checkedBullet : bs, checkedEnemies)
        

singleBulletCollision :: Bullet -> AliveEnemies -> (Bullet, AliveEnemies)
singleBulletCollision b = foldr processEnemy (b, [])
  where processEnemy e (b, es) | e `intersects` b && (bulletOwner b) == Friendly = (hurtSelf b, hurtSelf e : es)
                               | otherwise                                       = (         b,          e : es)

hostileCollisionCheck :: CanHurtPlayer a => [a] -> Player -> ([a], Player)
hostileCollisionCheck enemyList p = foldr processEnemy ([], p) enemyList
  where processEnemy e (es, p') | e `intersects` p' = (hurtSelf e : es, loseLife p')
                                | otherwise         = (         e : es,          p')

stepEnemies :: GameState -> Float -> AliveEnemies
stepEnemies gstate secs = (spawnEnemies (status gstate)) ++ existingEnemies
  where existingEnemies = filter inBounds $ map (moveEnemy . addTime . resetEnemyCooldown) (enemies gstate)
        addTime e = e {enemyCooldown = (enemyCooldown e) + secs}
        spawnEnemies (PlayingLevel (Level _ enemyList)) = assignPositions (generator gstate) $ map getFirst (filter shouldSpawn enemyList)
        spawnEnemies _                                  = undefined -- stepEnemies should not be called if the playingStatus is not of type PlayingLevel
        shouldSpawn (_, _, c) = c == Spawning
        getFirst  (a, _, _) = a

moveEnemy :: Enemy -> Enemy
moveEnemy e@(Enemy {enemyType = Dummy}) = e `move` (Vector (-5) 0)
moveEnemy e                             = move e $ 5 `scalarMult` (vectorNormalize $ Vector (-1) (-3 * cos x))
    where Point x _ = enemyPosition e

assignPositions :: StdGen -> AliveEnemies -> AliveEnemies
assignPositions gen [] = []
assignPositions gen (e:es) = (setPos e (Point x y)) : assignPositions newGen es
  where (Point x _) = getPos e
        (y, newGen) = randomHeight gen
        
inBounds :: CanMove a => a -> Bool
inBounds a = (distanceFromOrigin (getPos a)) <= (0.75 * w)
  where (w, _) = screenDims

stepStatus :: GameState -> PlayingStatus
stepStatus gstate = case (status gstate) of (PlayingLevel (Level nr enemyList)) -> PlayingLevel (Level nr (map updateSpawnStatus enemyList))
                                            otherState                          -> undefined -- stepStatus should not be called if the playingStatus is not of type PlayingLevel
  where updateSpawnStatus (a, b, Spawning) = (a, b, Spawned)  -- If an enemy was spawning last step, it has now spawned
        updateSpawnStatus (a, b, c)
          | (b == (round (playtime gstate))) && (c == Upcoming) = (a, b, Spawning)
          | otherwise                                           = (a, b, c)

stepBullets :: GameState -> ShotBullets
stepBullets gstate = map (\b -> move b (10 `scalarMult` bulletDirection b)) (bullets gstate)

eNEMYCOOLDOWNTHRESHOLD :: Float
eNEMYCOOLDOWNTHRESHOLD = 4

addedBullets :: AliveEnemies -> Player -> [Bullet]
addedBullets es p = catMaybes $ map (`enemyFiresBullet` p) es

enemyFiresBullet :: Enemy -> Player -> Maybe Bullet
enemyFiresBullet e p | (enemyCooldown e) >= eNEMYCOOLDOWNTHRESHOLD    = Just $ hostileBullet e p
                     | otherwise                                      = Nothing

resetEnemyCooldown :: Enemy -> Enemy
resetEnemyCooldown e | (enemyCooldown e) >= eNEMYCOOLDOWNTHRESHOLD    = e {enemyCooldown = 0}
                     | otherwise                                      = e

stepGameEnd :: GameState -> GameEnd
stepGameEnd gstate = (playerDead (player gstate)) || enemiesGone (status gstate)
  where enemiesGone (PlayingLevel (Level _ enemyList)) = ((finalEnemySpawn enemyList) + traversalTime) <= (round (playtime gstate))
        finalEnemySpawn enemyList = maximum (map second enemyList)
        second (_, b, _) = b
        traversalTime = 10 -- How much time it takes for an enemy to move past the player until it despawns

stepPlaytime :: GameState -> Float -> Playtime
stepPlaytime gstate secs
  | (paused gstate) == Paused   = playtime gstate
  | otherwise                   = (playtime gstate) + secs

stepAnimations :: [Animation] -> [Animation]
stepAnimations as = filter (\(_, n) -> n > 0) $ map (\(p, i) -> (p, i-1)) as

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e@(EventKey (Char 'p') Down _ _) gstate = return (inputKey e gstate)
input e@(EventKey (SpecialKey KeySpace) Down _ _) gstate@(GameState {status = MainMenu}) = return (gstate {status = LevelMenu})  
input e@(EventKey (Char c) Down _ _) gstate@(GameState {status = LevelMenu}) | isDigit c && (digitToInt c) <= 3 && (digitToInt c) > 0 = do let fileName = ("Levels/level" ++ [c]) ++ ".txt"
                                                                                                                                           fileContent <- readFile fileName
                                                                                                                                           return (loadLevel fileContent (digitToInt c) gstate)
                                                                             | otherwise = case paused gstate of
                                                                                            NotPaused -> return (inputKey e gstate)
                                                                                            Paused    -> return gstate
input e gstate = case paused gstate of
                  NotPaused -> return (inputKey e gstate)
                  Paused    -> return gstate

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') Down _ _) gstate@(GameState {status = PlayingLevel _}) = gstate {paused = togglePause (paused gstate), pressedKeys = empty}   -- P; (Un)pausing, only if in a level
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = gstate {bullets = friendlyBullet (player gstate) : bullets gstate} -- Space; Shooting bullets in a level
inputKey (EventKey key Down _ _) gstate = gstate {pressedKeys = insert key (pressedKeys gstate)} -- When any other key is pressed;  for handling holding keys down (pressedKeys)
inputKey (EventKey key Up _ _) gstate = gstate {pressedKeys = delete key (pressedKeys gstate)}   -- When any other key is released; for handling holding keys down (pressedKeys)
inputKey _ gstate = gstate


-- | Loading levels
-- A level file contains lines that specify which enemy should spawn at which time, in format: spawnTime enemyType nrOfLives
loadLevel :: String -> LevelNr -> GameState -> GameState
loadLevel fileContent nr gstate = gstate { status = PlayingLevel (Level nr (parseTuples (((map words) . lines) fileContent))), paused = NotPaused, enemies = [], bullets = [], player = initialPlayer, playtime = 0, gameEnd = False, generator = mkStdGen 1, animations = []}
  where parseTuples :: [[String]] -> [(Enemy, Int, SpawnStatus)]
        parseTuples enemyList = map tuplify enemyList
        tuplify e = (enemy (e!!2) (e!!1), (read (e!!0) :: Int), Upcoming) -- explain enemy format
        enemy livesNr "m" = Enemy {enemyPosition = Point (0.6 * w) 0, enemyDims = (10, 10), enemyLives = Lives (read livesNr :: Int), enemyCooldown = 0, enemyType = Moving}
        enemy livesNr _ = Enemy {enemyPosition = Point (0.6 * w) 0, enemyDims = (10, 10), enemyLives = Lives (read livesNr :: Int), enemyCooldown = 0, enemyType = Dummy}
        (w, h) = screenDims

-- | Loading progress
loadProgress :: String -> Progress
loadProgress fileContent = map parseProgress (lines fileContent)
  where parseProgress levelInfo = read levelInfo :: Int
   
