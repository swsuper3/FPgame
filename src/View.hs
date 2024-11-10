-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case status gstate of
                    MainMenu    
                        -> pictures [viewTitle,
                                     viewMainMenu]
                    LevelMenu
                        -> pictures (viewLevelMenu : viewLevels gstate)
                    PlayingLevel _
                        -> pictures [viewPlayer gstate,
                                      viewLives gstate,
                                      viewEnemies gstate,
                                      viewBullets gstate,
                                      viewAnimations gstate,
                                      viewEndMessage gstate,
                                      viewPaused gstate]

viewTitle :: Picture
viewTitle = translate (-0.4 *screenX) (0.2 * screenY) (scale 0.2 0.2 title)
  where title = color white (text "Block Blast!");

viewMainMenu :: Picture
viewMainMenu = translate (-0.4 *screenX) (0.1 * screenY) (scale 0.15 0.15 subtitle)
  where subtitle = color white (text "Press [Space] to start");

viewLevelMenu :: Picture
viewLevelMenu = translate (-0.4 *screenX) (0.2 * screenY) (scale 0.14 0.14 title)
  where title = color white (text "Press a key to load that level!");

-- This function draws the numbers of the levels in the game.
-- It draws the levels that have not yet been completed in white, and those that are completed in green.
viewLevels :: GameState -> [Picture]
viewLevels gstate = map viewLevel levelList
  where viewLevel lvl = translate ((-0.4 + (0.1 * fromIntegral lvl)) * screenX) (0.1 * screenY) (scale 0.15 0.15 (displayText lvl))
        displayText lvl | lvl `elem` progress gstate = color green (text (show lvl))
                        | otherwise                  = color white (text (show lvl))

viewPlayer :: GameState -> Picture
viewPlayer gstate = translate playerX playerY (color red playerBox)
  where (w, h) = playerDims (player gstate)
        Point playerX playerY = getPos (player gstate)
        playerBox = rectangleSolid w h

viewLives :: GameState -> Picture
viewLives gstate = translate (-0.5 * screenX) (-0.5 * screenY) (scale 0.2 0.2 livesPicture)
  where livesPicture = color green (text ("Lives: " ++ show n))
        (Lives n) = playerLives (player gstate)

viewEnemies :: GameState -> Picture
viewEnemies gstate = pictures $ map viewEnemy (enemies gstate)

--This function draws enemies depending on their type: dummies are drawn as squares, movings are drawn as cirlces
viewEnemy :: Enemy -> Picture
viewEnemy e@(Enemy{enemyType = Dummy}) = translate eX eY (color (livesColor (enemyLives e)) enemyBox)
  where (w, h) = enemyDims e
        Point eX eY = getPos e
        enemyBox = rectangleSolid w h
viewEnemy e                            = translate eX eY (color (livesColor (enemyLives e)) enemyCircle)
  where (_, h) = enemyDims e
        Point eX eY = getPos e
        enemyCircle = circleSolid (0.6*h)

--This function assigns a color to an enemy's live count.
livesColor :: Lives -> Color
livesColor (Lives 1) = green
livesColor (Lives 2) = yellow
livesColor _         = red

viewBullets :: GameState -> Picture
viewBullets gstate = pictures $ map viewBullet (bullets gstate)

--This function distinguishes between friendly and hostile bullets, giving them a different color
viewBullet :: Bullet -> Picture
viewBullet b | bulletOwner b == Friendly = translate bX bY (color blue bulletBox)
             | otherwise                   = translate bX bY (color magenta bulletBox)
  where (w, h) = bulletDims b
        Point bX bY = bulletPosition b
        bulletBox = rectangleSolid w h

-- viewTime is currently unused; it can be displayed to show the current playtime, which is useful for testing purposes
viewTime :: GameState -> Picture
viewTime gstate = translate (0.4 * screenX) (0.4 * screenY) (scale 0.2 0.2 time)
    where time = color white (text (show (round (playtime gstate))))


viewAnimations :: GameState -> Picture
viewAnimations gstate = pictures $ map viewAnimation (animations gstate)

viewAnimation :: Animation -> Picture
viewAnimation (Point x y, n) = translate x y $ color orange $ circleSolid (0.8*n)


viewEndMessage :: GameState -> Picture
viewEndMessage gstate@(GameState {gameEnd = True}) = translate (-0.4 *screenX) (0.2 * screenY) (scale 0.2 0.2 title)
  where title | playerDead (player gstate) = color white (text "You died :(")
              | otherwise                  = color white (text "You've completed the level!")
viewEndMessage _ = blank


viewPaused :: GameState -> Picture
viewPaused (GameState {paused = Paused, gameEnd = False}) = pictures [icon 0, icon 20]
    where icon x = translate (-0.45*screenX + x) (0.4*screenY) $ color white $ rectangleSolid 10 40
viewPaused _ = blank
