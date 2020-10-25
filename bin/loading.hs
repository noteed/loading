-- | This is (mostly) the example at the top of this documentation page:
-- https://hackage.haskell.org/package/sdl2-2.5.2.0/docs/SDL.html.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Int (Int32)
import Data.List (foldl')
import Foreign.C.Types (CInt)
import SDL
import SDL.Primitive (fillTriangle)
import Linear (V4(..))
import Control.Monad (unless)


--------------------------------------------------------------------------------
main :: IO ()
main = do
  initializeAll
  window <- createWindow "Loading..." defaultWindow
    { windowInitialSize = V2 1920 1200 }
  renderer <- createRenderer window (-1) defaultRenderer

  putStrLn "Press `q` to quit."
  loop renderer initialState
  putStrLn "Bye."

  -- -1 means "initialize the first rendering driver supporting the requested
  -- flags": see https://wiki.libsdl.org/SDL_CreateRenderer.
  --
  -- TODO How to implement a fullscreen mode ?
  -- I tried to change the defaultWindow { windowMode = Fullscreen } or
  -- { windowMode = FullscreenDesktop } but this didn't work...


--------------------------------------------------------------------------------
data State = State
  { sQuit :: Bool
  , sPoints :: [Point V2 CInt]
  }

initialState = State
  { sQuit = False
  , sPoints = []
  }


--------------------------------------------------------------------------------
loop :: Renderer -> State -> IO ()
loop renderer st = do
  events <- pollEvents

  withLowResolution renderer (draw st)

  mapM_ (print . eventPayload) events

  let st' = foldl' processEvent st events
  unless (sQuit st') (loop renderer st')

withLowResolution renderer drawingFunction = do
  -- Create a render target with a low resolution and big pixels, and set it as
  -- the current render target. Instead of 320x240, I use something similar but
  -- with a 1.6 aspect ratio (instead of 1.33). Another way is to use
  -- rendererLogicalSize but unfortunately, this doesn't affect sdl2-gfx's
  -- `triangle` routine, nor even the normal `drawLine` routine.
  target <- createTexture renderer RGBA8888 TextureAccessTarget (V2 384 240)
  rendererRenderTarget renderer $= (Just target)

  -- Clear to blue.
  rendererDrawColor renderer $= V4 0 0 204 255
  clear renderer

  drawingFunction renderer

  -- Reset the render target to the default.
  rendererRenderTarget renderer $= Nothing

  -- Render the previsous render target to the default render target and
  -- present it.
  copy renderer target
    (Just (SDL.Rectangle (P (V2 0 0)) (V2 384 240)))
    (Just (SDL.Rectangle (P (V2 0 0)) (V2 1920 1200)))
  present renderer

draw st renderer = do
  -- How to draw a point.
  rendererDrawColor renderer $= V4 255 255 255 255
  drawPoint renderer (P (V2 10 10))
  mapM_ (drawPoint renderer) (sPoints st)

  -- How to draw a rectangle outline.
  drawRect renderer (Just (SDL.Rectangle (P (V2 20 10)) (V2 20 20)))

  -- How to draw a filled rectangle.
  fillRect renderer (Just (SDL.Rectangle (P (V2 50 10)) (V2 20 20)))

  -- How to draw a line.
  drawLine renderer (P (V2 10 40)) (P (V2 70 60))

  -- How to draw a triangle with sdl2-gfx.
  fillTriangle renderer (V2 10 50) (V2 70 70) (V2 60 100) (V4 255 255 0 255)


--------------------------------------------------------------------------------
processEvent st event | isQPressed event = st { sQuit = True }

processEvent st event = case eventPayload event of
  MouseButtonEvent (MouseButtonEventData {..}) ->
    if mouseButtonEventButton == ButtonLeft &&
       mouseButtonEventMotion == Pressed
    then st { sPoints = int32ToCInt mouseButtonEventPos : sPoints st }
    else st

  _ -> st

isQPressed event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
    _ -> False

-- | Convert from mouse button data, at real window resolution, to drawPoint
-- data, at low resolution.
int32ToCInt :: Point V2 Int32 -> Point V2 CInt
int32ToCInt (P (V2 x y)) =
  P (V2 (fromIntegral x `div` 5) (fromIntegral y `div` 5))
