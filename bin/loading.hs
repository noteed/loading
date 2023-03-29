-- | This is (mostly) the example at the top of this documentation page:
-- https://hackage.haskell.org/package/sdl2-2.5.2.0/docs/SDL.html.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Applicative
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( when )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Foldable                  ( foldlM )
import           Data.Int                       ( Int32 )
import           Data.Maybe                     ( isJust )
import           Data.Primitive.Ptr
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Data.Word                      ( Word32
                                                , Word8
                                                )
import           Foreign.C.String               ( CString
                                                , withCString
                                                )
import           Foreign.C.Types                ( CInt )
import           Foreign.Marshal.Array          ( pokeArray )
import           Foreign.Marshal.Utils          ( copyBytes )
import           Foreign.Ptr                    ( Ptr
                                                , castPtr
                                                , nullPtr
                                                , plusPtr
                                                )
import           Foreign.Storable               ( peek, poke )
import           Linear                         ( V4(..) )
import qualified Options.Applicative           as A
import           SDL                     hiding ( initialize )
import           SDL.Image                      ( load )
import qualified SDL.Image
import qualified SDL.Internal.Types            as Types
import           SDL.Primitive                  ( fillTriangle )
import qualified SDL.Raw                       as Raw


--------------------------------------------------------------------------------
-- | We want a fixed 60 frames per second. Since graphics are low-res, maybe
-- this should be 30...
fps = 60

-- | Duration of a frame, in milliseconds.
frameDuration = 1000 `div` fps

lowRes = V2 384 240
lowResRect = SDL.Rectangle (P (V2 0 0)) lowRes


--------------------------------------------------------------------------------
main = A.execParser parserInfo >>= run

parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> A.helper)
    $  A.fullDesc
    <> A.header "loading - playing with low-res graphics"
    <> A.progDesc "`loading` is just a silly experiment."


--------------------------------------------------------------------------------
data Command =
  Edit RendererType
  | Run RendererType
  | Headless

parser :: A.Parser Command
parser =
  A.subparser
    $  A.command "edit"
                 (A.info (editParser <**> A.helper) $ A.progDesc "Edit a PNG.")
    <> A.command
         "run"
         ( A.info (runParser <**> A.helper)
         $ A.progDesc "Run the interactive program."
         )
    <> A.command
         "headless"
         ( A.info (pure Headless <**> A.helper)
         $ A.progDesc "Generate a PNG image."
         )

editParser :: A.Parser Command
editParser = Edit <$> vsyncParser

runParser :: A.Parser Command
runParser = Run <$> vsyncParser

vsyncParser :: A.Parser RendererType
vsyncParser =
  A.flag AcceleratedVSyncRenderer AcceleratedRenderer
    $  A.long "no-vsync"
    <> A.help "Disable vsync."
       -- One way I saw that vsync did help, was letting my finger on the "m"
       -- key, setting and unsetting the magnification repeatedly.


--------------------------------------------------------------------------------
run :: Command -> IO ()
run (Edit rType) = edit rType
run (Run rType) =
  interactive rType initialState (\st r -> example st r >> drawState st r)
run Headless = headless initialState example


--------------------------------------------------------------------------------
edit :: RendererType -> IO ()
edit rType = do
  (renderer, target, mgamepad) <- initialize rType
  streaming                    <- loadStreaming renderer "editable.png"
  t1                           <- ticks
  loop
    (renderer, target)
    t1
    initialState { sEditing = Just streaming }
    (\st r -> do
      copy r streaming (Just lowResRect) (Just lowResRect)
      drawState st r
    )
  destroy (renderer, target, mgamepad)


--------------------------------------------------------------------------------
-- | Run the main interactive game loop, using an initial state and a drawing
-- function used as a background.
interactive :: RendererType -> State -> (State -> Renderer -> IO ()) -> IO ()
interactive rType initial background = do
  (renderer, target, mgamepad) <- initialize rType
  t1                           <- ticks
  loop (renderer, target) t1 initial background
  destroy (renderer, target, mgamepad)


--------------------------------------------------------------------------------
-- | Call a drawing function once, using an initial state, then save a
-- screenshot.
headless :: State -> (State -> Renderer -> IO ()) -> IO ()
headless initial background = do
  initializeAll
  -- It is possible to use a hidden window with
  --
  --     window <- createWindow "Loading..." defaultWindow
  --       { windowInitialSize = V2 384 240, windowVisible = False }
  --
  -- then a renderer as usual, but it seems the rendering is not done unless the
  -- window is showed. Instead for headless rendering, we use
  -- createSoftwareRenderer.
  surface  <- createRGBSurface (V2 384 240) RGBA8888
  renderer <- createSoftwareRenderer surface

  background initial renderer
  present renderer

  putStrLn "Saving screenshot to screenshot.png..."
  writeRendererToPNG renderer "screenshot.png"

  destroyRenderer renderer
  freeSurface surface
  putStrLn "Done."


--------------------------------------------------------------------------------
data State = State
  { sQuit         :: Bool
    -- ^ When True, exit the main loop.
  , sShowEvents   :: Bool
    -- ^ When True, log the polled events.
  , sMagnifiedPos :: Point V2 CInt
  , sMagnified    :: Bool
    -- ^ When True, show a mignified zone of the screen.
  , sCursor       :: Point V2 CInt
    -- ^ The main controlling point to interact with the application.
  , sPoints       :: [Point V2 CInt]
    -- ^ Points to draw. They are added by left-clicking.
  , sEditing      :: Maybe Texture
    -- ^ In Edit mode, the streaming texture being edited.
  }

initialState :: State
initialState = State { sQuit         = False
                     , sShowEvents   = False
                     , sMagnifiedPos = P (V2 0 0)
                     , sMagnified    = False
                     , sCursor       = P (V2 (96 `div` 2) (60 `div` 2))
                     , sPoints       = []
                     , sEditing      = Nothing
                     }

data Operation =
    Click (Point V2 CInt)
  | MoveCursor (V2 CInt)
  | ToggleShowEvents
  | ToggleMagnification
  | AddPoint
  | Clear
  | Screenshot
  | Quit
  | Nop
  deriving (Eq, Show)

-- | Applying an operation is mostly pure (returns a new State), but parts of
-- the state can be e.g. a streaming texture, and changing it is done in IO.
applyOperation :: State -> Operation -> IO State
applyOperation st Quit  = pure $ st { sQuit = True }

applyOperation st Clear = do
  case sEditing st of
    Just streaming -> do
      let P (V2 x y) = sCursor st
      (pixels, pitch) <- lockTexture streaming Nothing
      -- let color = 0x0000FF00 :: Word32 -- RGBA, so blue.
      -- setPtr (castPtr pixels :: Ptr Word32) (384 * 240) color
      let
        color1 = V4 0 255 34 34 -- blue. This seems to be ABGR ...
        color2 = V4 0 255 0 0 -- blue. This seems to be ABGR ...
        band1 =
          take (8 * 384) $ cycle (replicate 8 color1 ++ replicate 8 color2)
        band2 =
          take (8 * 384) $ cycle (replicate 8 color2 ++ replicate 8 color1)
      pokeArray (castPtr pixels :: Ptr (V4 Word8)) $ take (384 * 240) $ cycle
        (band1 ++ band2)
      unlockTexture streaming
      pure st
    Nothing -> pure $ addPoint st

applyOperation st Screenshot = pure st -- This is handled directly in the loop.

applyOperation st (Click p)  = pure $ st
  { sCursor       = p
  , sMagnifiedPos = if sMagnified st then sMagnifiedPos st else p
  }

applyOperation st (MoveCursor d) =
  pure $ st { sCursor = moveCursor (sCursor st) d }

applyOperation st AddPoint = do
  case sEditing st of
    Just streaming -> do
      let P (V2 x y) = sCursor st
      (pixels, pitch) <- lockTexture streaming Nothing
      let pixels' =
            plusPtr (castPtr pixels :: Ptr Word8)
              $ fromIntegral $ y
              * pitch
              + x
              * 4
      poke (castPtr pixels' :: Ptr Word32) 0xFFFFFF00
      unlockTexture streaming
      pure st
    Nothing -> pure $ addPoint st

applyOperation st ToggleShowEvents =
  pure $ st { sShowEvents = not (sShowEvents st) }

applyOperation st ToggleMagnification =
  pure $ st { sMagnified = not (sMagnified st) }

applyOperation st Nop = pure $ st


--------------------------------------------------------------------------------
loop
  :: (Renderer, Texture)
  -> Word32
  -> State
  -> (State -> Renderer -> IO ())
  -> IO ()
loop (renderer, target) t1 st background = do
  pumpEvents -- This is supposed to be implicitely called by pollEvents
             -- but pollEvents returns nothing without it.
  events <- pollEvents
  when (sShowEvents st) $ mapM_ print events
  let operations = map processEvent events

  when (Screenshot `elem` operations) $ do
    screenshot (renderer, target) "screenshot.png"

  st' <- foldlM applyOperation st operations

  withLowResolution (renderer, target) st' background

  if sQuit st'
    then do
      -- Save a screen capture when exiting.
      when (isJust $ sEditing st) $ screenshot (renderer, target) "editable.png"
    else do
      -- In milliseconds.
      -- I guess this can drift over time. TODO Something more solid.
      t2 <- ticks
      let frameCompletion_ = frameDuration - (t2 - t1)
          frameCompletion =
            -- t2 and t1 are Word32. If we have to wait for more than
            -- frameDuration, it means wrap-around occurred and actually means
            -- we-re running late.
            if frameCompletion_ > frameDuration then 0 else frameCompletion_
      delay $ fromIntegral frameCompletion
      loop (renderer, target) (t2 + frameCompletion) st' background

example :: State -> Renderer -> IO ()
example _ renderer = do
  -- Clear to blue.
  rendererDrawColor renderer $= V4 0 0 204 255
  clear renderer

  rendererDrawColor renderer $= V4 255 255 255 255

  -- How to draw a rectangle outline.
  drawRect renderer (Just (SDL.Rectangle (P (V2 20 10)) (V2 20 20)))

  -- How to draw a filled rectangle.
  fillRect renderer (Just (SDL.Rectangle (P (V2 50 10)) (V2 20 20)))

  -- How to draw a line.
  drawLine renderer (P (V2 10 40)) (P (V2 70 60))

  -- How to draw a triangle with sdl2-gfx.
  fillTriangle renderer (V2 10 50) (V2 70 70) (V2 60 100) (V4 255 255 0 255)

blue8x8 :: State -> Renderer -> IO ()
blue8x8 _ renderer = do
  rendererDrawColor renderer $= V4 205 205 255 255
  clear renderer
  rendererDrawColor renderer $= V4 0 0 204 255
  fillRect renderer (Just (SDL.Rectangle (P (V2 44 26)) (V2 8 8)))

blue :: State -> Renderer -> IO ()
blue _ renderer = do
  -- Clear to blue.
  rendererDrawColor renderer $= V4 0 0 204 255
  clear renderer

drawState :: State -> Renderer -> IO ()
drawState st renderer = do
  -- How to draw a point.
  rendererDrawColor renderer $= V4 255 255 255 255
  drawPoint renderer (sCursor st)
  mapM_ (drawPoint renderer) (sPoints st)


--------------------------------------------------------------------------------
processEvent :: Event -> Operation
processEvent event | isQPressed event = Quit

processEvent event | isCPressed event = Clear

processEvent event | isSPressed event = Screenshot

processEvent event                    = case eventPayload event of
  MouseButtonEvent (MouseButtonEventData {..}) ->
    if mouseButtonEventButton == ButtonLeft && mouseButtonEventMotion == Pressed
      then Click $ int32ToCInt mouseButtonEventPos
      else Nop

  KeyboardEvent keyboardEvent
    | keyboardEventKeyMotion keyboardEvent
      == Pressed
      && keysymKeycode (keyboardEventKeysym keyboardEvent)
      == KeycodeE
    -> ToggleShowEvents
    | keyboardEventKeyMotion keyboardEvent
      == Pressed
      && keysymKeycode (keyboardEventKeysym keyboardEvent)
      == KeycodeM
    -> ToggleMagnification
    | keyboardEventKeyMotion keyboardEvent
      == Pressed
      && keysymKeycode (keyboardEventKeysym keyboardEvent)
      == KeycodeSpace
    -> AddPoint
    | keyboardEventKeyMotion keyboardEvent == Pressed
    -> let arrow = keysymKeycode (keyboardEventKeysym keyboardEvent)
       in  MoveCursor $ arrowToDelta' arrow

  JoyButtonEvent (JoyButtonEventData {..})
    | joyButtonEventWhich
      == 0
      && joyButtonEventButton
      == 5
      && joyButtonEventState
      == JoyButtonPressed
    ->
    -- Right shoulder button.
       ToggleMagnification

  JoyButtonEvent (JoyButtonEventData {..})
    | joyButtonEventWhich
      == 0
      && joyButtonEventButton
      == 0
      && joyButtonEventState
      == JoyButtonPressed
    ->
    -- "A" button.
       AddPoint

  JoyHatEvent (JoyHatEventData {..}) | joyHatEventWhich == 0 ->
    MoveCursor $ hatToDelta joyHatEventValue

  _ -> Nop


--------------------------------------------------------------------------------
addPoint :: State -> State
addPoint st = if p `elem` sPoints st
  then st { sPoints = filter (/= p) (sPoints st) }
  else st { sPoints = sCursor st : sPoints st }
  where p = sCursor st

isQPressed :: Event -> Bool
isQPressed = isPressed KeycodeQ

isCPressed :: Event -> Bool
isCPressed = isPressed KeycodeC

isSPressed :: Event -> Bool
isSPressed = isPressed KeycodeS

isPressed :: Keycode -> Event -> Bool
isPressed keycode event = case eventPayload event of
  KeyboardEvent keyboardEvent ->
    keyboardEventKeyMotion keyboardEvent
      == Pressed
      && keysymKeycode (keyboardEventKeysym keyboardEvent)
      == keycode
  _ -> False

arrowToDelta :: Keycode -> V2 Integer
arrowToDelta KeycodeLeft  = V2 (-12) 0
arrowToDelta KeycodeUp    = V2 0 (-15)
arrowToDelta KeycodeRight = V2 12 0
arrowToDelta KeycodeDown  = V2 0 15
arrowToDelta _            = V2 0 0

arrowToDelta' :: Keycode -> V2 CInt
arrowToDelta' KeycodeLeft  = V2 (-1) 0
arrowToDelta' KeycodeUp    = V2 0 (-1)
arrowToDelta' KeycodeRight = V2 1 0
arrowToDelta' KeycodeDown  = V2 0 1
arrowToDelta' _            = V2 0 0

hatToDelta :: JoyHatPosition -> V2 CInt
hatToDelta HatLeft  = V2 (-1) 0
hatToDelta HatUp    = V2 0 (-1)
hatToDelta HatRight = V2 1 0
hatToDelta HatDown  = V2 0 1
hatToDelta _        = V2 0 0

moveMagnifyingZone :: Point V2 Integer -> V2 Integer -> Point V2 Integer
moveMagnifyingZone p v = P (V2 x3 y3)
 where
  P (V2 x1 y1) = p .+^ v
  x2           = if x1 > 384 - 96 then 384 - 96 else x1
  x3           = if x2 < 0 then 0 else x2
  y2           = if y1 > 240 - 60 then 240 - 60 else y1
  y3           = if y2 < 0 then 0 else y2

moveCursor :: Point V2 CInt -> V2 CInt -> Point V2 CInt
moveCursor p v = P (V2 x3 y3)
 where
  P (V2 x1 y1) = p .+^ v
  x2           = if x1 > 384 then 384 else x1
  x3           = if x2 < 0 then 0 else x2
  y2           = if y1 > 240 then 240 else y1
  y3           = if y2 < 0 then 0 else y2


--------------------------------------------------------------------------------
-- | Convert from mouse button data, at real window resolution, to drawPoint
-- data, at low resolution.
int32ToCInt :: Point V2 Int32 -> Point V2 CInt
int32ToCInt (P (V2 x y)) =
  P (V2 (fromIntegral x `div` 5) (fromIntegral y `div` 5))


--------------------------------------------------------------------------------
initialize :: RendererType -> IO (Renderer, Texture, Maybe Joystick)
initialize rType = do
  initializeAll
  window <- createWindow "Loading..."
                         defaultWindow { windowInitialSize = V2 1920 1200 }
  -- -1 means "initialize the first rendering driver supporting the requested
  -- flags": see https://wiki.libsdl.org/SDL_CreateRenderer.
  --
  -- TODO How to implement a fullscreen mode ?
  -- I tried to change the defaultWindow { windowMode = Fullscreen } or
  -- { windowMode = FullscreenDesktop } but this didn't work...
  renderer <- createRenderer window
                             (-1)
                             defaultRenderer { rendererType = rType }

  -- Create a render target with a low resolution and big pixels. See
  -- withLowResolution for details.
  target   <- createTexture renderer RGBA8888 TextureAccessTarget (V2 384 240)

  mgamepad <- openGamepad

  pure (renderer, target, mgamepad)

destroy :: (Renderer, Texture, Maybe Joystick) -> IO ()
destroy (renderer, target, mgamepad) = do
  maybe (return ()) closeJoystick mgamepad
  destroyTexture target
  destroyRenderer renderer
  SDL.Image.quit
  quit

-- | Use a low resolution texture as a rendering target. Instead of 320x240, I
-- use something similar but with a 1.6 aspect ratio (instead of 1.33). Another
-- way is to use rendererLogicalSize but unfortunately, this doesn't affect
-- sdl2-gfx's `triangle` routine, nor even the normal `drawLine` routine.
withLowResolution
  :: (Renderer, Texture) -> State -> (State -> Renderer -> IO ()) -> IO ()
withLowResolution (renderer, target) st drawingFunction = do
  rendererRenderTarget renderer $= (Just target)

  drawingFunction st renderer

  -- Reset the render target to the default.
  rendererRenderTarget renderer $= Nothing

  -- Render the previous render target to the default render target.
  -- When magnified, a 96x60 zone is magnified 20x and rendered on the screen.
  if (sMagnified st)
    then do
      copy renderer
           target
           (Just (SDL.Rectangle (sMagnifiedPos st) (V2 96 60)))
           (Just (SDL.Rectangle (P (V2 0 0)) (V2 1920 1200)))
    else do
  -- Otherwise, the whole previous target is rendered. 1 pixel becomes a 5x5
  -- square.
      copy renderer
           target
           (Just (SDL.Rectangle (P (V2 0 0)) (V2 384 240)))
           (Just (SDL.Rectangle (P (V2 0 0)) (V2 1920 1200)))

  present renderer

screenshot :: (Renderer, Texture) -> FilePath -> IO ()
screenshot (renderer, target) path = do
  putStrLn $ "Saving screenshot to " <> path <> "..."
  rendererRenderTarget renderer $= (Just target)
  writeRendererToPNG renderer path
  rendererRenderTarget renderer $= Nothing

openGamepad = do
  -- Enumerate gamepads, then open the first one. After doing so, `pollEvents`
  -- in `loop` will reports gamepad events.
  n <- numJoysticks
  putStrLn ("Detected " ++ show n ++ " gamepads.")
  gamepads <- availableJoysticks
  mapM_ (putStrLn . logGamepad) gamepads
  if not (V.null gamepads)
    then Just <$> openJoystick (V.head gamepads)
    else pure Nothing

logGamepad :: JoystickDevice -> String
logGamepad JoystickDevice {..} =
  "Gamepad #"
    ++ show joystickDeviceId
    ++ " is "
    ++ T.unpack joystickDeviceName
    ++ "."

--------------------------------------------------------------------------------
writeRendererToPNG :: Renderer -> FilePath -> IO ()
writeRendererToPNG (Types.Renderer r) fn = do
  surface <- Raw.createRGBSurface 0 384 240 32 0 0 0 0
  Raw.lockSurface surface
  format  <- Raw.surfaceFormat <$> peek surface
  format' <- Raw.pixelFormatFormat <$> peek format
  pixels  <- Raw.surfacePixels <$> peek surface
  Raw.renderReadPixels r nullPtr format' pixels (384 * 4)
              -- Surface pitch: width x bytes per pixel. This is normally a
              -- field within the SDL surface but is hidden in the current
              -- bindings.
  withCString fn (savePNG surface)
  Raw.unlockSurface surface
  Raw.freeSurface surface


--------------------------------------------------------------------------------
-- | Load a PNG to a streaming texture. The library's `loadTexture` is simpler
-- to use but doesn't return a streaming texture.
loadStreaming :: Renderer -> FilePath -> IO Texture
loadStreaming renderer path = do
  surface   <- loadSurface path
  streaming <- createTexture renderer RGBA8888 TextureAccessStreaming lowRes
  copySurface streaming surface
  pure streaming

-- | Load a PNG to a surface with the RGBA8888 format.
loadSurface :: FilePath -> IO Surface
loadSurface path = do
  -- We load a surface, convert it to make sure it is in RGBA8888 format.
  surface  <- load path
  -- Dummy surface, to create a SurfacePixelFormat because I don't how to
  -- create one otherwise.
  surface_ <- createRGBSurface (V2 32 32) RGBA8888
  format   <- surfaceFormat surface_
  surface' <- convertSurface surface format
  freeSurface surface_
  -- Note that it seems we can't free the dummy surface before we use its
  -- format.
  freeSurface surface
  return surface'

-- | Copy the pixels from a surface to a streaming texture.
copySurface :: Texture -> Surface -> IO ()
copySurface streaming surface = do
  lockSurface surface
  srcPixels           <- surfacePixels surface
  (destPixels, pitch) <- lockTexture streaming Nothing
  copyBytes destPixels srcPixels (fromIntegral pitch * 240)
  unlockTexture streaming
  unlockSurface surface
  freeSurface surface


--------------------------------------------------------------------------------
savePNG :: Ptr Raw.Surface -> CString -> IO ()
savePNG v1 v2 = liftIO (savePNGFFI v1 v2)

foreign import ccall "SDL_image.h IMG_SavePNG"
  savePNGFFI :: Ptr Raw.Surface -> CString -> IO ()
