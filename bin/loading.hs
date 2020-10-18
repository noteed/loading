-- | This is (mostly) the example at the top of this documentation page:
-- https://hackage.haskell.org/package/sdl2-2.5.2.0/docs/SDL.html.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
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
  loop renderer
  putStrLn "Bye."

  -- -1 means "initialize the first rendering driver supporting the requested
  -- flags": see https://wiki.libsdl.org/SDL_CreateRenderer.
  --
  -- TODO How to implement a fullscreen mode ?
  -- I tried to change the defaultWindow { windowMode = Fullscreen } or
  -- { windowMode = FullscreenDesktop } but this didn't work...


--------------------------------------------------------------------------------
loop :: Renderer -> IO ()
loop renderer = do
  events <- pollEvents

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

  -- How to draw a point.
  rendererDrawColor renderer $= V4 255 255 255 255
  drawPoint renderer (P (V2 10 10))

  -- How to draw a rectangle outline.
  drawRect renderer (Just (SDL.Rectangle (P (V2 20 10)) (V2 20 20)))

  -- How to draw a filled rectangle.
  fillRect renderer (Just (SDL.Rectangle (P (V2 50 10)) (V2 20 20)))

  -- How to draw a line.
  drawLine renderer (P (V2 10 40)) (P (V2 70 60))

  -- Reset the render target to the default.
  rendererRenderTarget renderer $= Nothing

  -- Render the previsous render target to the default render target and
  -- present it.
  copy renderer target
    (Just (SDL.Rectangle (P (V2 0 0)) (V2 384 240)))
    (Just (SDL.Rectangle (P (V2 0 0)) (V2 1920 1200)))
  present renderer

  let qPressed = any isQPressed events
  unless qPressed (loop renderer)

isQPressed event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
    _ -> False
