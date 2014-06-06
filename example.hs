module Main where

import Graphics.UI.SDL as SDL
import Control.Monad
import Foreign.Storable
import Foreign.Ptr
import Data.Foldable (toList)

import ElementryAutomaton hiding (main)

createColor screen r g b = SDL.mapRGB (SDL.surfaceGetPixelFormat screen) r g b
data Player = PlayerFactory {xy :: (Int,Int) ,isDead :: Bool}
width = 400
height = 400
gameinit = do
     SDL.init [SDL.InitEverything]
     SDL.setVideoMode width height 32 [SDL.HWSurface,DoubleBuf]
     SDL.setCaption "Video Test!" "video test"
     mainscreen <- getVideoSurface
     initcolor <- createColor mainscreen 0 0 0
     playercolor <-  createColor mainscreen 255 255 255
     pixels <- castPtr `liftM` surfaceGetPixels mainscreen
     return (PlayerFactory (320,240) False, eg' width rule30 , mainscreen,playercolor,initcolor,pixels,surfaceGetWidth mainscreen)
     
ruleNumbers = cycle (replicateM 20 [True,False])

putPixel32 :: Int -> Int -> Pixel -> Surface -> IO ()
putPixel32 x y (Pixel pixel) s = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    pokeElemOff pixels ((y * (surfaceGetWidth s)) + x) pixel	 
    

putPixel32' x y (Pixel pixel) pixels width = do
    pokeElemOff pixels ((y * width) + x) pixel	 

getPixel32' x y pixels width = do
    Pixel `liftM` peekElemOff pixels ((y * width) + x)	     

gameend = SDL.quit

main = do
       print "start"
       gameinit >>= mainLoop >> gameend
    
mainLoop (player,dat,mainscreen,playercolor,initcolor,pixels,surfaceWidth) = do
     
     
     forM_ (zip [0..] (head $ dat)) ( \(x1,x2) ->
        if x2 then putPixel32' x1 (height-1) (Pixel 0xFFFFFF) pixels surfaceWidth
              else putPixel32' x1  (height-1) (Pixel 0x000000) pixels surfaceWidth)
      -- shift pixels 
     doFromTo 1 (height-1) $ \y -> 
       doFromTo 0 (width-1) $ \x ->
         do
         p <- getPixel32' x y pixels width
         putPixel32' x (y-1) p pixels surfaceWidth
       
       
     event <- SDL.pollEvent
     SDL.flip mainscreen
     result (checkEvent event)
     where
     checkEvent (KeyUp (Keysym SDLK_ESCAPE _ _)) = (player { isDead = True })
     checkEvent _ = player
     result obj | isDead obj == True = return ()
                | otherwise = mainLoop (obj,drop 1 dat,mainscreen,playercolor,initcolor,pixels,surfaceWidth)
                
{-# INLINE doFromTo #-}
-- do the action for [from..to], ie it's inclusive.
doFromTo :: Int -> Int -> (Int -> IO ()) -> IO ()
doFromTo from to action =
  let loop n | n > to   = return ()
             | otherwise = do action n
                              loop (n+1)
   in loop from               