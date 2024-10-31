-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Set

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
    = return $ gstate { elapsedTime = elapsedTime gstate + secs,
                        paused      = updatePause gstate
                      }
  | otherwise
    = return $ gstate { elapsedTime = elapsedTime gstate + secs,
                      player      = move (player gstate) (10 `scalarMult` (getPlayerMovementVector (pressedKeys gstate))),
                      playtime    = updatePlaytime gstate secs,
                      paused      = updatePause gstate
                      }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey key Down _ _) gstate = gstate {pressedKeys = insert key (pressedKeys gstate)}
inputKey (EventKey key Up _ _) gstate = gstate {pressedKeys = delete key (pressedKeys gstate)}
inputKey _ gstate = gstate
    
--     -- If the user presses a character key, show that one
--     gstate { infoToShow = ShowAChar c }
-- inputKey _ gstate = gstate -- Otherwise keep the same