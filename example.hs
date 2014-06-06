module Main where

import Graphics.UI.SDL as SDL
import Control.Monad
import Foreign.Storable
import Foreign.Ptr

import ElementryAutomaton hiding (main)

createColor screen r g b = SDL.mapRGB (SDL.surfaceGetPixelFormat screen) r g b

data Player = PlayerFactory {
                    xy :: (Int,Int),
                    isDead :: Bool
                    }

gameinit = do
     SDL.init [SDL.InitEverything]
     SDL.setVideoMode 640 480 32 []
     SDL.setCaption "Video Test!" "video test"
     image <- createRGBSurface [] 640 480 16 0 0 0 0
     return (PlayerFactory (320,240) False,image)    

putPixel32 :: Int -> Int -> Pixel -> Surface -> IO ()
putPixel32 x y (Pixel pixel) s = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    pokeElemOff pixels ((y * (surfaceGetWidth s)) + x) pixel	 

gameend = SDL.quit

main = do
       print "start"
       -- line <- getLine 
       -- print line
       gameinit >>= mainLoop >> gameend

initscreen = do
    mainscreen <- getVideoSurface
    initcolor <- createColor mainscreen 0 0 0
    SDL.fillRect mainscreen (Just(SDL.Rect 0 0 640 480)) initcolor

mainLoop (player,image) = do
      mainscreen <-  getVideoSurface
      initscreen
      playercolor <-  createColor mainscreen 255 255 255
      
      forM_ (zip [1..] (take 300 $ eg' rule110)) ( \(x1,x2) ->
        forM_ (zip [1..] x2) ( \(y1,y2) ->
            if y2 then putPixel32 (y1) (x1+1) (Pixel 0xFFFFFF) mainscreen
                  else putPixel32 (y1) (x1+1) (Pixel 0xAAAAAA) mainscreen
            )
       )     
      putPixel32 20 20 (Pixel 0xFFFFFF) image
      putPixel32 21 21 (Pixel 0xFFFFFF) image
    --  blitSurface image Nothing mainscreen Nothing
     -- SDL.fillRect mainscreen (Just(SDL.Rect (fst(xy player)) (snd(xy player)) 32 32)) playercolor
      event <- SDL.waitEventBlocking
      SDL.flip mainscreen
      result (checkEvent event)
     where
     checkEvent (KeyDown (Keysym SDLK_UP _ _)) = (player { xy = ((fst(xy player)),(snd(xy player)) - 6) })
     checkEvent (KeyDown (Keysym SDLK_DOWN _ _)) = (player { xy = ((fst(xy player)),(snd(xy player)) + 6) })
     checkEvent (KeyUp (Keysym SDLK_ESCAPE _ _)) = (player { isDead = True })
     checkEvent _ = player
     result obj | isDead obj == True = return ()
                | otherwise = mainLoop (obj,image)