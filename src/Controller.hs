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

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | gameEnd gstate && (playtime gstate > gameEndDowntime*2) -- If the game just finished (playtime has not been reset yet), set the playtime to 0
    = return $ gstate { elapsedTime = elapsedTime gstate + secs,
                        playtime = 0,
                        paused = Paused
                      }
  | gameEnd gstate && playerDead (player gstate) && playtime gstate > gameEndDowntime -- If the game is lost and it has been more than 3 seconds, go back to LevelMenu
    = return gstate { elapsedTime = elapsedTime gstate + secs,
                        paused = Paused,
                        status = LevelMenu,
                        gameEnd = False
                      }
  | gameEnd gstate && (playtime gstate > gameEndDowntime) -- If the game is won and it has been more than 3 seconds, go back to LevelMenu and update progress
    = do appendFile "Levels/progress.txt" ('\n' : show currentLevelNr)
         return gstate { elapsedTime = elapsedTime gstate + secs,
                        progress = currentLevelNr : progress gstate,
                        paused = Paused,
                        status = LevelMenu,
                        pressedKeys = empty,
                        gameEnd = False
                      }
  | gameEnd gstate  -- If the game is finished
    = return $ gstate { elapsedTime = elapsedTime gstate + secs,
                        playtime = playtime gstate + secs,
                        paused = Paused,
                        animations = newAnimations ++ stepAnimations (animations gstate)
                      }
  | paused gstate == Paused -- If the game is paused and/or in a menu
    = return $ gstate { elapsedTime = elapsedTime gstate + secs
                      }
  | otherwise -- If a level is running
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
            (PlayingLevel (Level currentLevelNr _)) = status gstate
            gameEndDowntime = 3 -- the amount of seconds waited between a winning/losing a level and returning to LevelMenu

-- How the player updates in a step
stepPlayer :: GameState -> Player
stepPlayer gstate = move (player gstate) (playerSpeed `scalarMult` getPlayerMovementVector (pressedKeys gstate))
  where playerSpeed = 10

-- Below are a lot of functions dealing with collision

--This function gives us the 'final product' of our collision checks
collision :: GameState -> GameState
collision = checkCollisionPlayer . friendlyBulletCollision

--This function checks collision between the player and enemies / enemies' bullets
checkCollisionPlayer :: GameState -> GameState
checkCollisionPlayer gstate = gstate {player = newPlayer', enemies = withoutDeadEnemies, bullets = withoutDeadBullets}
  where (newEnemies, newPlayer) = hostileCollisionCheck (enemies gstate) (player gstate)
        (newBullets, newPlayer') = hostileCollisionCheck hostileBullets newPlayer
        (hostileBullets, friendlyBullets) = partition (\b -> bulletOwner b == Hostile) (bullets gstate)
        withoutDeadEnemies = clearDeads newEnemies
        withoutDeadBullets = friendlyBullets ++ clearDeads newBullets

--This function checks collision between the bullets shot by the player and enemies
friendlyBulletCollision :: GameState -> GameState
friendlyBulletCollision gstate = gstate {enemies = withoutDeadEnemies, bullets = withoutDeadBullets}
  where (newBullets, newEnemies) = multiBulletCollision (bullets gstate) (enemies gstate)
        withoutDeadEnemies = clearDeads newEnemies
        withoutDeadBullets = clearDeads newBullets

--A helper function for the above 'friendlyBulletCollision' function. Checks a list of bullets against a list of enemies.
multiBulletCollision :: ShotBullets -> AliveEnemies -> (ShotBullets, AliveEnemies)
multiBulletCollision bulletList enemyList = foldr f ([], enemyList) bulletList
  where f b (bs, es) = 
                  let (checkedBullet, checkedEnemies) = singleBulletCollision b es
                  in (checkedBullet : bs, checkedEnemies)
        
--A helper function for the above 'multiBulletCollision' function. Checks a single bullet against a list of enemies.
singleBulletCollision :: Bullet -> AliveEnemies -> (Bullet, AliveEnemies)
singleBulletCollision b = foldr processEnemy (b, [])
  where processEnemy e (b, es) | e `intersects` b && bulletOwner b == Friendly = (hurtSelf b, hurtSelf e : es)
                               | otherwise                                       = (         b,          e : es)

--A generalized helper function for 'checkCollisionPlayer'. Can check the player against anything that can hurt it. (Meaning enemies and their bullets)
hostileCollisionCheck :: CanHurtPlayer a => [a] -> Player -> ([a], Player)
hostileCollisionCheck enemyList p = foldr processEnemy ([], p) enemyList
  where processEnemy e (es, p') | e `intersects` p' = (hurtSelf e : es, loseLife p')
                                | otherwise         = (         e : es,          p')

--This function defines what happens to enemies each frame.
-- 1. We reset the enemy's cooldown back to zero if it has reached the threshold.
-- 2. We add time to the enemy's cooldown.
-- 3. We move the enemy.
-- 4. We remove enemies that have gone out of bounds.
-- 5. We spawn enemies if necessary.
stepEnemies :: GameState -> Float -> AliveEnemies
stepEnemies gstate secs = spawnEnemies (status gstate) ++ existingEnemies
  where existingEnemies = filter inBounds $ map (moveEnemy . addTime . resetEnemyCooldown) (enemies gstate)
        addTime e = e {enemyCooldown = enemyCooldown e + secs}
        spawnEnemies (PlayingLevel (Level _ enemyList)) = assignPositions (generator gstate) $ map getFirst (filter shouldSpawn enemyList)
        spawnEnemies _                                  = undefined -- stepEnemies should not be called if the playingStatus is not of type PlayingLevel
        shouldSpawn (_, _, c) = c == Spawning
        getFirst  (a, _, _) = a

--Moves the enemy depending on the type.
moveEnemy :: Enemy -> Enemy
moveEnemy e@(Enemy {enemyType = Dummy}) = e `move` (enemySpeed `scalarMult` Vector (-1) 0)
   where enemySpeed = 5
moveEnemy e                             = move e $ enemySpeed `scalarMult` (vectorNormalize $ Vector (-1) (-3 * cos x))
    where Point x _ = enemyPosition e
          enemySpeed = 15

--This function assigns a position with a randomized height to each enemy in a list
assignPositions :: StdGen -> AliveEnemies -> AliveEnemies
assignPositions gen [] = []
assignPositions gen (e:es) = setPos e (Point x y) : assignPositions newGen es
  where (Point x _) = getPos e
        (y, newGen) = randomHeight gen

--This functioon checks if a given movable object is in bounds or not.        
inBounds :: CanMove a => a -> Bool
inBounds a = distanceFromOrigin (getPos a) <= (boundsThreshold * w)
  where (w, _) = screenDims
        boundsThreshold = 0.75

--This function handles the spawning of enemies during play.
stepStatus :: GameState -> PlayingStatus
stepStatus gstate = case status gstate of (PlayingLevel (Level nr enemyList)) -> PlayingLevel (Level nr (map updateSpawnStatus enemyList))
                                          _                                   -> error "stepStatus should not be called if the playingStatus is not of type PlayingLevel"
  where updateSpawnStatus (a, b, Spawning) = (a, b, Spawned)  -- If an enemy was spawning last step, it has now spawned
        updateSpawnStatus (a, b, c)
          | (b == (round (playtime gstate))) && (c == Upcoming) = (a, b, Spawning)
          | otherwise                                           = (a, b, c)

--This function moves the bullets at each iteration.
stepBullets :: GameState -> ShotBullets
stepBullets gstate = map (\b -> move b (bulletSpeed `scalarMult` bulletDirection b)) (bullets gstate)
  where bulletSpeed = 10

--The below couple functions have to do with the enemies' ability to shoot bullets.
eNEMYCOOLDOWNTHRESHOLD :: Float
eNEMYCOOLDOWNTHRESHOLD = 4

--This function gives a list of the newly shot bullets.
addedBullets :: AliveEnemies -> Player -> [Bullet]
addedBullets es p = catMaybes $ map (`enemyFiresBullet` p) es

enemyFiresBullet :: Enemy -> Player -> Maybe Bullet
enemyFiresBullet e p | (enemyCooldown e) >= eNEMYCOOLDOWNTHRESHOLD    = Just $ hostileBullet e p
                     | otherwise                                      = Nothing

resetEnemyCooldown :: Enemy -> Enemy
resetEnemyCooldown e | (enemyCooldown e) >= eNEMYCOOLDOWNTHRESHOLD    = e {enemyCooldown = 0}
                     | otherwise                                      = e

--This function handles the end of the game.
stepGameEnd :: GameState -> GameEnd
stepGameEnd gstate = (playerDead (player gstate)) || enemiesGone (status gstate)
  where enemiesGone (PlayingLevel (Level _ enemyList)) = ((finalEnemySpawn enemyList) + traversalTime) <= (round (playtime gstate))
        finalEnemySpawn enemyList = maximum (map second enemyList)
        second (_, b, _) = b
        traversalTime = 10 -- How much time it takes for an enemy to move past the player until it despawns

--This function simply updates the game time.
stepPlaytime :: GameState -> Float -> Playtime
stepPlaytime gstate secs
  | paused gstate == Paused   = playtime gstate
  | otherwise                   = playtime gstate + secs

--This function furthers on the animations, and removes the ones that are done.
stepAnimations :: [Animation] -> [Animation]
stepAnimations as = filter (\(_, n) -> n > 0) $ map (\(p, i) -> (p, i-1)) as

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e@(EventKey (Char 'p') Down _ _) gstate = return (inputKey e gstate)  --The 'p' key should not simply be ignored when the game is paused for obvious reasons.
input e@(EventKey (SpecialKey KeySpace) Down _ _) gstate@(GameState {status = MainMenu}) = return (gstate {status = LevelMenu})  --Spacebar in the main menu moves us to the level menu
input e@(EventKey (Char c) Down _ _) gstate@(GameState {status = LevelMenu}) | isDigit c && digitToInt c <= 3 && digitToInt c > 0 = do let fileName = ("Levels/level" ++ [c]) ++ ".txt"   --Pressing a number on the level menu loads the corresponding level.
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
loadLevel fileContent nr gstate = gstate { status = PlayingLevel (Level nr (parseTuples ((map words . lines) fileContent))), paused = NotPaused, enemies = [], bullets = [], player = initialPlayer, playtime = 0, gameEnd = False, generator = mkStdGen 1, animations = []}
  where parseTuples :: [[String]] -> [(Enemy, Int, SpawnStatus)]
        parseTuples = map tuplify
        tuplify e = (enemy (e!!2) (e!!1), read (e!!0) :: Int, Upcoming)
        enemy livesNr "m" = Enemy {enemyPosition = Point (0.6 * w) 0, enemyDims = (10, 10), enemyLives = Lives (read livesNr :: Int), enemyCooldown = 0, enemyType = Moving}
        enemy livesNr _ = Enemy {enemyPosition = Point (0.6 * w) 0, enemyDims = (10, 10), enemyLives = Lives (read livesNr :: Int), enemyCooldown = 0, enemyType = Dummy}
        (w, h) = screenDims

-- | Loading progress
loadProgress :: String -> Progress
loadProgress fileContent = map parseProgress (lines fileContent)
  where parseProgress levelInfo = read levelInfo :: Int