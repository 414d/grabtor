-----------------------------------------------------------------------------
-- Copyright : (c) mhd aka 414d
-- License : GPL3
-- 
-- Maintainer : mhd <414dbox at Gmail>
-- Stability : unstable
-- Portability : unportable
-- 
-- A graphic battery status monitor adapted for xmobar.
-- 
-----------------------------------------------------------------------------

module Main where

import GraphicBatteryMonitor
import System.Environment

main = do
  args <- getArgs
  showMonitor args
