-- | This is (mostly) the example at the top of this documentation page:
-- https://hackage.haskell.org/package/sdl2-2.5.2.0/docs/SDL.html.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (unless, when)
import Data.Int (Int32)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Vector as V
import Foreign.C.Types (CInt)
import Linear (V4(..))
import SDL
import SDL.Primitive (fillTriangle)


--------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Loading..."
  initializeAll
  window <- createWindow "Loading..." defaultWindow
    { windowInitialSize = V2 1920 1200 }
  renderer <- createRenderer window (-1) defaultRenderer

  -- Enumerate gamepads, then open the first one. After doing so, `pollEvents`
  -- below will reports gamepad events.
  n <- numJoysticks
  putStrLn ("Detected " ++ show n ++ " gamepads.")
  gamepads <- availableJoysticks
  mapM_ (putStrLn . logGamepad) gamepads
  gamepad <-
    if not (V.null gamepads)
    then Just <$> openJoystick (V.head gamepads)
    else return Nothing

  putStrLn "Press `q` to quit."
  loop renderer initialState

  maybe (return ()) closeJoystick gamepad
  putStrLn "Bye."

  -- -1 means "initialize the first rendering driver supporting the requested
  -- flags": see https://wiki.libsdl.org/SDL_CreateRenderer.
  --
  -- TODO How to implement a fullscreen mode ?
  -- I tried to change the defaultWindow { windowMode = Fullscreen } or
  -- { windowMode = FullscreenDesktop } but this didn't work...

logGamepad JoystickDevice {..} =
  "Gamepad #" ++ show joystickDeviceId ++ " is " ++
    T.unpack joystickDeviceName ++ "."

--------------------------------------------------------------------------------
data State = State
  { sQuit :: Bool
    -- ^ When True, exit the main loop.
  , sShowEvents :: Bool
    -- ^ When True, log the polled events.
  , sMagnifiedPos :: Point V2 CInt
  , sMagnified :: Bool
    -- ^ When True, show a mignified zone of the screen.
  , sCursor :: Point V2 CInt
    -- ^ The main controlling point to interact with the application.
  , sPoints :: [Point V2 CInt]
    -- ^ Points to draw. They are added by left-clicking.
  }

initialState = State
  { sQuit = False
  , sShowEvents = False
  , sMagnifiedPos = P (V2 0 0)
  , sMagnified = False
  , sCursor = P (V2 (384 `div` 2) ( 240 `div` 2))
  , sPoints = []
  }


--------------------------------------------------------------------------------
loop :: Renderer -> State -> IO ()
loop renderer st = do
  events <- pollEvents

  withLowResolution st renderer draw

  when (sShowEvents st) $
    mapM_ print events

  let st' = foldl' processEvent st events
  unless (sQuit st') (loop renderer st')

withLowResolution st renderer drawingFunction = do
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

  drawingFunction st renderer

  -- Reset the render target to the default.
  rendererRenderTarget renderer $= Nothing

  -- Render the previous render target to the default render target and
  -- present it. 1 pixel becomes a 5x5 square.
  copy renderer target
    (Just (SDL.Rectangle (P (V2 0 0)) (V2 384 240)))
    (Just (SDL.Rectangle (P (V2 0 0)) (V2 1920 1200)))

  -- In addition, a 48x60 zone is magnified 20x and rendered on the right half
  -- of the screen.
  when (sMagnified st) $
    copy renderer target
      (Just (SDL.Rectangle (sMagnifiedPos st) (V2 48 60)))
      (Just (SDL.Rectangle (P (V2 960 0)) (V2 960 1200)))

  present renderer

draw st renderer = do
  -- How to draw a point.
  rendererDrawColor renderer $= V4 255 255 255 255
  drawPoint renderer (sCursor st)
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


  KeyboardEvent keyboardEvent |
    keyboardEventKeyMotion keyboardEvent == Pressed &&
    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeE ->
    st { sShowEvents = not (sShowEvents st) }
                              |
    keyboardEventKeyMotion keyboardEvent == Pressed &&
    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeM ->
    st { sMagnified = not (sMagnified st) }
                              |
    keyboardEventKeyMotion keyboardEvent == Pressed &&
    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeSpace ->
    st { sPoints = sCursor st : sPoints st }
                              |
    keyboardEventKeyMotion keyboardEvent == Pressed ->
    let arrow = keysymKeycode (keyboardEventKeysym keyboardEvent)
        magpos = if sMagnified st
                 then moveMagnifyingZone (sMagnifiedPos st) (arrowToDelta arrow)
                 else sMagnifiedPos st
        curpos = if not (sMagnified st)
                 then moveCursor (sCursor st) (arrowToDelta' arrow)
                 else sCursor st
    in
    st { sMagnifiedPos = magpos, sCursor = curpos }

  JoyButtonEvent (JoyButtonEventData {..}) |
    joyButtonEventWhich == 0 &&
    joyButtonEventButton == 5 &&
    joyButtonEventState == JoyButtonPressed ->
    -- Right shoulder button.
    st { sMagnified = not (sMagnified st) }

  JoyHatEvent (JoyHatEventData {..}) | joyHatEventWhich == 0 ->
    st { sMagnifiedPos =
         moveMagnifyingZone (sMagnifiedPos st) (hatToDelta joyHatEventValue) }

  _ -> st

isQPressed event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
    _ -> False

arrowToDelta KeycodeLeft = V2 (-12) 0
arrowToDelta KeycodeUp = V2 0 (-15)
arrowToDelta KeycodeRight = V2 12 0
arrowToDelta KeycodeDown = V2 0 15
arrowToDelta _ = V2 0 0

arrowToDelta' KeycodeLeft = V2 (-1) 0
arrowToDelta' KeycodeUp = V2 0 (-1)
arrowToDelta' KeycodeRight = V2 1 0
arrowToDelta' KeycodeDown = V2 0 1
arrowToDelta' _ = V2 0 0

hatToDelta HatLeft = V2 (-12) 0
hatToDelta HatUp = V2 0 (-15)
hatToDelta HatRight = V2 12 0
hatToDelta HatDown = V2 0 15
hatToDelta _ = V2 0 0

moveMagnifyingZone (P (V2 x y)) (V2 dx dy) = P (V2 x3 y3)
  where
    x1 = x + dx
    x2 = if x1 > 384 - 48 then 384 - 48 else x1
    x3 = if x2 < 0 then 0 else x2
    y1 = y + dy
    y2 = if y1 > 240 - 60 then 240 - 60 else y1
    y3 = if y2 < 0 then 0 else y2

moveCursor (P (V2 x y)) (V2 dx dy) = P (V2 x3 y3)
  where
    x1 = x + dx
    x2 = if x1 > 384 then 384 else x1
    x3 = if x2 < 0 then 0 else x2
    y1 = y + dy
    y2 = if y1 > 240 then 240 else y1
    y3 = if y2 < 0 then 0 else y2


--------------------------------------------------------------------------------
-- | Convert from mouse button data, at real window resolution, to drawPoint
-- data, at low resolution.
int32ToCInt :: Point V2 Int32 -> Point V2 CInt
int32ToCInt (P (V2 x y)) =
  P (V2 (fromIntegral x `div` 5) (fromIntegral y `div` 5))
