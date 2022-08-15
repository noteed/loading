-- | This is (mostly) the example at the top of this documentation page:
-- https://hackage.haskell.org/package/sdl2-2.5.2.0/docs/SDL.html.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int32)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Vector as V
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Linear (V4(..))
import SDL
import qualified SDL.Internal.Types as Types
import SDL.Primitive (fillTriangle)
import qualified SDL.Raw as Raw
import System.Environment (getArgs)


--------------------------------------------------------------------------------
-- | We want a fixed 60 frames per second. Since graphics are low-res, maybe
-- this should be 30...
fps = 60

-- | Duration of a frame, in milliseconds.
frameDuration = 1000 `div` fps


--------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Loading..."
  args <- getArgs
  case args of
    ["headless"] -> headless
    _ -> run


--------------------------------------------------------------------------------
-- | Call `draw` once using `initialState`, then save a screenshot.
headless = do
  initializeAll
  -- It is possible to use a hidden window with
  --
  --     window <- createWindow "Loading..." defaultWindow
  --       { windowInitialSize = V2 384 240, windowVisible = False }
  --
  -- then a renderer as usual, but it seems the rendering is not done unless the
  -- window is showed. Instead for headless rendering, we use
  -- createSoftwareRenderer.
  surface <- createRGBSurface (V2 384 240) RGBA8888
  renderer <- createSoftwareRenderer surface

  draw initialState renderer
  present renderer

  putStrLn "Saving screenshot to screenshot.png..."
  writeRendererToPNG renderer "screenshot.png"

  destroyRenderer renderer
  putStrLn "Done."


--------------------------------------------------------------------------------
run = do
  initializeAll
  window <- createWindow "Loading..." defaultWindow
    { windowInitialSize = V2 1920 1200 }
  renderer <- createRenderer window (-1) defaultRenderer

  -- Create a render target with a low resolution and big pixels. See
  -- withLowResolution for details.
  target <- createTexture renderer RGBA8888 TextureAccessTarget (V2 384 240)

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
  loop target renderer initialState

  maybe (return ()) closeJoystick gamepad
  destroyTexture target
  destroyRenderer renderer
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

initialState :: State
initialState = State
  { sQuit = False
  , sShowEvents = False
  , sMagnifiedPos = P (V2 0 0)
  , sMagnified = False
  , sCursor = P (V2 (384 `div` 2) ( 240 `div` 2))
  , sPoints = []
  }


--------------------------------------------------------------------------------
loop :: Texture -> Renderer -> State -> IO ()
loop target renderer st = do
  t1 <- ticks
  events <- pollEvents

  withLowResolution st target renderer draw

  when (sShowEvents st) $
    mapM_ print events

  let st' = foldl' processEvent st events
  -- Save a screen capture when exiting.
  when (sQuit st') (do
    putStrLn "Saving screenshot to screenshot.png..."
    rendererRenderTarget renderer $= (Just target)
    writeRendererToPNG renderer "screenshot.png"
    rendererRenderTarget renderer $= Nothing)

  t2 <- ticks
  unless (sQuit st') $ do
    -- Convert from milliseconds to microseconds.
    -- I guess this can drift over time. TODO Something more solid.
    threadDelay (fromIntegral ((frameDuration - (t2 - t1)) * 1000))
    loop target renderer st'

-- | Use a low resolution texture as a rendering target. Instead of 320x240, I
-- use something similar but with a 1.6 aspect ratio (instead of 1.33). Another
-- way is to use rendererLogicalSize but unfortunately, this doesn't affect
-- sdl2-gfx's `triangle` routine, nor even the normal `drawLine` routine.
withLowResolution st target renderer drawingFunction = do
  rendererRenderTarget renderer $= (Just target)

  drawingFunction st renderer

  -- Reset the render target to the default.
  rendererRenderTarget renderer $= Nothing

  -- Render the previous render target to the default render target.
  -- When magnified, a 96x60 zone is magnified 20x and rendered on the screen.
  if (sMagnified st)
  then do
    copy renderer target
      (Just (SDL.Rectangle (sMagnifiedPos st) (V2 96 60)))
      (Just (SDL.Rectangle (P (V2 0 0)) (V2 1920 1200)))
  else do
  -- Otherwise, the whole previous target is rendered. 1 pixel becomes a 5x5
  -- square.
    copy renderer target
      (Just (SDL.Rectangle (P (V2 0 0)) (V2 384 240)))
      (Just (SDL.Rectangle (P (V2 0 0)) (V2 1920 1200)))

  present renderer

draw st renderer = do
  -- Clear to blue.
  rendererDrawColor renderer $= V4 0 0 204 255
  clear renderer

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
    then st { sCursor = int32ToCInt mouseButtonEventPos
            , sMagnifiedPos = if sMagnified st then sMagnifiedPos st
                                               else int32ToCInt mouseButtonEventPos
            }
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
        curpos = moveCursor (sCursor st) (arrowToDelta' arrow)
    in
    st { sCursor = curpos }

  JoyButtonEvent (JoyButtonEventData {..}) |
    joyButtonEventWhich == 0 &&
    joyButtonEventButton == 5 &&
    joyButtonEventState == JoyButtonPressed ->
    -- Right shoulder button.
    st { sMagnified = not (sMagnified st) }

  JoyButtonEvent (JoyButtonEventData {..}) |
    joyButtonEventWhich == 0 &&
    joyButtonEventButton == 0 &&
    joyButtonEventState == JoyButtonPressed ->
    -- "A" button.
    st { sPoints = sCursor st : sPoints st }

  JoyHatEvent (JoyHatEventData {..}) | joyHatEventWhich == 0 ->
    st { sCursor = moveCursor (sCursor st) (hatToDelta joyHatEventValue) }

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

hatToDelta HatLeft = V2 (-1) 0
hatToDelta HatUp = V2 0 (-1)
hatToDelta HatRight = V2 1 0
hatToDelta HatDown = V2 0 1
hatToDelta _ = V2 0 0

moveMagnifyingZone p v = P (V2 x3 y3)
  where
    P (V2 x1 y1) = p .+^ v
    x2 = if x1 > 384 - 96 then 384 - 96 else x1
    x3 = if x2 < 0 then 0 else x2
    y2 = if y1 > 240 - 60 then 240 - 60 else y1
    y3 = if y2 < 0 then 0 else y2

moveCursor p v = P (V2 x3 y3)
  where
    P (V2 x1 y1) = p .+^ v
    x2 = if x1 > 384 then 384 else x1
    x3 = if x2 < 0 then 0 else x2
    y2 = if y1 > 240 then 240 else y1
    y3 = if y2 < 0 then 0 else y2


--------------------------------------------------------------------------------
-- | Convert from mouse button data, at real window resolution, to drawPoint
-- data, at low resolution.
int32ToCInt :: Point V2 Int32 -> Point V2 CInt
int32ToCInt (P (V2 x y)) =
  P (V2 (fromIntegral x `div` 5) (fromIntegral y `div` 5))


--------------------------------------------------------------------------------
writeRendererToPNG :: Renderer -> FilePath -> IO ()
writeRendererToPNG (Types.Renderer r) fn = do
  surface <- Raw.createRGBSurface 0 384 240 32 0 0 0 0
  Raw.lockSurface surface
  format <- Raw.surfaceFormat <$> peek surface
  format' <- Raw.pixelFormatFormat <$> peek format
  pixels <- Raw.surfacePixels <$> peek surface
  Raw.renderReadPixels r nullPtr
    format'
    pixels
    (384 * 4) -- Surface pitch: width x bytes per pixel. This is normally a
              -- field within the SDL surface but is hidden in the current
              -- bindings.
  withCString fn (savePNG surface)
  Raw.unlockSurface surface

savePNG v1 v2 = liftIO (savePNGFFI v1 v2)

foreign import ccall "SDL_image.h IMG_SavePNG"
  savePNGFFI :: Ptr Raw.Surface -> CString -> IO ()
