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

viewPure :: GameState -> Picture  --To draw something alongside the player, add another view function, and add it to the list in this function.
viewPure gstate = case status gstate of
                    MainMenu    
                        -> pictures [viewMainMenu]
                    LevelMenu
                        -> pictures []
                    PlayingLevel _
                        -> pictures [viewPlayer gstate,
                                      viewLives gstate,
                                      viewEnemies gstate,
                                      viewBullets gstate,
                                      viewTime gstate,
                                      viewAnimations gstate]

viewMainMenu :: Picture
viewMainMenu = translate (-0.4 *screenX) (0.2 * screenY) (scale 0.2 0.2 title)
  where title = color white (text (show "Press [Space] to start!"));
        (screenX, screenY) = screenDims

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

viewEnemies :: GameState -> Picture
viewEnemies gstate = pictures $ map viewEnemy (enemies gstate)

viewEnemy :: Enemy -> Picture
viewEnemy e = translate eX eY (color green enemyBox)
  where (w, h) = enemyDims e
        Point eX eY = getPos e
        enemyBox = rectangleSolid w h

viewBullets :: GameState -> Picture
viewBullets gstate = pictures $ map viewBullet (bullets gstate)

viewBullet :: Bullet -> Picture
viewBullet b = translate bX bY (color blue bulletBox)
  where (w, h) = bulletDims b
        Point bX bY = bulletPosition b
        bulletBox = rectangleSolid w h

-- viewTime is currently unused; it can be displayed to show the current playtime
viewTime :: GameState -> Picture
viewTime gstate = translate (0.4 * screenX) (0.4 * screenY) (scale 0.2 0.2 time)
    where time = color white (text (show (round (playtime gstate))))
          (screenX, screenY) = screenDims


viewAnimations :: GameState -> Picture
viewAnimations gstate = pictures $ map viewAnimation (animations gstate)

viewAnimation :: Animation -> Picture
viewAnimation (Point x y, n) = translate x y $ color orange $ circleSolid (0.5*n)
