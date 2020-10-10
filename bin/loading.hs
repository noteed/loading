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
  renderer <- createRenderer window (-1) defaultRenderer

  putStrLn "Press `q` to quit."
  loop renderer
  putStrLn "Bye."

  -- -1 means "initialize the first rendering driver supporting the requested
  -- flags": see https://wiki.libsdl.org/SDL_CreateRenderer.


--------------------------------------------------------------------------------
loop :: Renderer -> IO ()
loop renderer = do
  events <- pollEvents
  rendererDrawColor renderer $= V4 255 0 0 255
  clear renderer
  present renderer
  let qPressed = any isQPressed events
  unless qPressed (loop renderer)

isQPressed event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
    _ -> False
