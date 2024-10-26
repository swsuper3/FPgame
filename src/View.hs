-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

-- viewPure :: GameState -> Picture
-- viewPure gstate = case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])

viewPure :: GameState -> Picture  --To draw something alongside the player, add another view function, and at it to the list in this function.
viewPure gstate = pictures [viewPlayer gstate, viewLives gstate]


viewPlayer :: GameState -> Picture
viewPlayer gstate = translate playerX playerY (color red playerBox)
  where (w, h) = playerDims (player gstate)
        Point playerX playerY = getPos (player gstate)
        playerBox = rectangleSolid w h

viewLives :: GameState -> Picture
viewLives gstate = translate (-0.5 * screenX) (-0.5 * screenY) (scale 0.5 0.5 livesPicture)
  where livesPicture = color green (text (show n))
        (Lives n) = playerLives (player gstate)
        (screenX, screenY) = screenDims