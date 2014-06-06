module Main where

import Graphics.UI.SDL as SDL
import Control.Monad
import Foreign.Storable
import Foreign.Ptr

import ElementryAutomaton hiding (main)

createColor screen r g b = SDL.mapRGB (SDL.surfaceGetPixelFormat screen) r g b

data Player = PlayerFactory {
                     xy :: (Int,Int)
                    ,isDead :: Bool}

gameinit = do
     SDL.init [SDL.InitEverything]
     SDL.setVideoMode 640 480 32 []
     SDL.setCaption "Video Test!" "video test"
     mainscreen <- getVideoSurface
     initcolor <- createColor mainscreen 0 0 0
    
     playercolor <-  createColor mainscreen 255 255 255
     return (PlayerFactory (320,240) False, eg' rule110, mainscreen,playercolor,initcolor)
     
ruleNUmbers = cycle (replicateM 300 [True,False])

putPixel32 :: Int -> Int -> Pixel -> Surface -> IO ()
putPixel32 x y (Pixel pixel) s = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    pokeElemOff pixels ((y * (surfaceGetWidth s)) + x) pixel	 
    

putPixel32' x y (Pixel pixel) pixels width = do
    pokeElemOff pixels ((y * width) + x) pixel	 

gameend = SDL.quit

main = do
       print "start"
       gameinit >>= mainLoop >> gameend
    
mainLoop (player,dat,mainscreen,playercolor,initcolor) = do
      let surfaceWidth = surfaceGetWidth mainscreen
      pixels <- castPtr `liftM` surfaceGetPixels mainscreen
      forM_ (zip [0..] (take 450 $ dat )) ( \(x1,x2) ->
        forM_ (zip [0..] x2) ( \(y1,y2) ->
            if y2 then putPixel32' (y1+10) (x1+10) (Pixel 0xFFFFFF) pixels surfaceWidth
                  else putPixel32' (y1+10) (x1+10) (Pixel 0x333333) pixels surfaceWidth
            )
       )
      -- event <- SDL.waitEvent
      event <- SDL.pollEvent
      SDL.flip mainscreen
      result (checkEvent event)
     where
     checkEvent (KeyDown (Keysym SDLK_UP _ _)) = (player { xy = ((fst(xy player)),(snd(xy player)) - 6) })
     checkEvent (KeyDown (Keysym SDLK_DOWN _ _)) = (player { xy = ((fst(xy player)),(snd(xy player)) + 6) })
     checkEvent (KeyUp (Keysym SDLK_ESCAPE _ _)) = (player { isDead = True })
     checkEvent _ = player
     result obj | isDead obj == True = return ()
                | otherwise = mainLoop (obj,drop 1 dat,mainscreen,playercolor,initcolor)