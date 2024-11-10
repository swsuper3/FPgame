module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import System.Random (mkStdGen)

main :: IO ()
main =  do progressContent <- readFile "Levels/progress.txt"
           playIO (InWindow "Counter" intScreenDims (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState {progress = loadProgress progressContent}    -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function