-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Set (insert, delete, empty)
import Debug.Trace (trace)

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
  | (paused gstate) == Paused
    = return $ gstate { elapsedTime = elapsedTime gstate + secs
                      }
  | otherwise
    = do seed <- randomIO
         return $ checkedCollisionGstate { elapsedTime = elapsedTime checkedCollisionGstate + secs,
                                        player = stepPlayer checkedCollisionGstate,
                                        enemies = stepEnemies checkedCollisionGstate,
                                        bullets = stepBullets checkedCollisionGstate,
                                        playtime = updatePlaytime gstate secs,
                                        generator = mkStdGen seed
                                        }
      where checkedCollisionGstate = collision gstate

stepPlayer :: GameState -> Player
stepPlayer gstate = move (player gstate) (10 `scalarMult` (getPlayerMovementVector (pressedKeys gstate)))

collision :: GameState -> GameState
collision = checkCollisionPlayer . friendlyBulletCollision

checkCollisionPlayer :: GameState -> GameState
checkCollisionPlayer gstate = gstate {player = newPlayer, enemies = withoutDeadEnemies}
  where (newEnemies, newPlayer) = hostileCollisionCheck (enemies gstate) (player gstate)
        withoutDeadEnemies = clearDeads newEnemies

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
  where processEnemy e (b, es) | e `intersects` b = (hurtSelf b, hurtSelf e : es)
                               | otherwise        = (         b,          e : es)

hostileCollisionCheck :: CanHurtPlayer a => [a] -> Player -> ([a], Player)
hostileCollisionCheck enemyList p = foldr processEnemy ([], p) enemyList
  where processEnemy e (es, p') | e `intersects` p' = (hurtSelf e : es, loseLife p')
                                | otherwise         = (         e : es,          p')

stepEnemies :: GameState -> AliveEnemies
stepEnemies gstate = map (`move` (Vector (-5) 0)) (enemies gstate)

stepBullets :: GameState -> ShotBullets
stepBullets gstate = map (\b -> move b (10 `scalarMult` bulletDirection b)) (bullets gstate)


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e@(EventKey (Char 'p') Down _ _) gstate = return (inputKey e gstate)
input e gstate = case paused gstate of
                  NotPaused -> return (inputKey e gstate)
                  Paused    -> return gstate

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'p') Down _ _) gstate = gstate {paused = togglePause (paused gstate), pressedKeys = empty}
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = gstate {bullets = friendlyBullet (player gstate) : bullets gstate}
inputKey (EventKey key Down _ _) gstate = gstate {pressedKeys = insert key (pressedKeys gstate)}
inputKey (EventKey key Up _ _) gstate = gstate {pressedKeys = delete key (pressedKeys gstate)}
inputKey _ gstate = gstate
    
--     -- If the user presses a character key, show that one
--     gstate { infoToShow = ShowAChar c }
-- inputKey _ gstate = gstate -- Otherwise keep the same