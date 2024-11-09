module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import System.Random (mkStdGen)

main :: IO ()
main = playIO (InWindow "Counter" intScreenDims (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function