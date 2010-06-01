{-|
A quick and simple way to run Elerea networks with GLFW window and event handling.

e.g.

> withGLFWExt
>   "Good Stuff!"
>   (myCreateSignal :: SignalCreator)
>   defaultConfig {
>
>           postOpenInit = do
>               clearColor $= Color4 0 0 0 0
>               clearDepth $= 1
>               depthFunc $= Just Less
>
>           resizeCallback = \size@(Size w h) -> do
>               viewport $= (Position 0 0, size)
>               matrixMode $= Projection
>               loadIdentity
>               perspective 45 (fromIntegral w / fromIntegral h) 0.1 100
>               matrixMode $= Modelview 0
>       }
-}

module FRP.Elerea.GLFW (

    SignalCreator,

    -- * Configuring
    GlfwAdapterConfig (..), defaultConfig

    -- * Running
    , withGLFWExt, withGLFW

    ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import FRP.Elerea

import Control.Monad ( join, when )
import Data.List ( delete )

import Data.IORef

import System.Exit


-- | The type of Elerea callbacks that receive event sources and create the network.

type SignalCreator =
       Signal [Key]                     -- ^ keyboard state
    -> Signal Position                  -- ^ current mouse position
    -> Signal [MouseButton]             -- ^ mouse button state
    -> Signal Int                       -- ^ current scroll-wheel value
    -> SignalMonad (Signal (IO ()))

-- | Extra knobs

data GlfwAdapterConfig =

    GAC { winSize :: Maybe Size
          -- ^ window size, 'FullScreen' if Nothing

        , displayBits :: [DisplayBits]
          -- ^ ... for 'openWindow'

        , preOpenInit :: IO ()
          -- ^ initialization to perform /before/ opening the window, after 'initialize'

        , postOpenInit :: IO ()
          -- ^ initialization to perform /after/ opening the window (typically, the usual initGL stuff)

        , extraCleanup :: IO ()
          -- ^ called just before 'terminate'

        , resizeCallback :: WindowSizeCallback
          -- ^ the raw 'WindowSizeCallback'
        }

-- | Default 'winSize'.
-- (8,8,8) rbg bits \/ 8 alpha bits \/ 24 depth bits for 'displayBits'.
-- 'preOpenInit', 'postOpenInit', 'resizeCallback' and 'extraCleanup' do nothing.

defaultConfig :: GlfwAdapterConfig
defaultConfig =

    GAC { winSize = Just (Size 0 0)

        , displayBits = [ DisplayRGBBits 8 8 8
                        , DisplayAlphaBits 8
                        , DisplayDepthBits 24 ]

        , preOpenInit = return ()

        , postOpenInit = return ()

        , resizeCallback = \_ -> return ()

        , extraCleanup = return ()
    }

-- | Just go with the default config.

withGLFW :: String -> SignalCreator -> IO ()
withGLFW t s = withGLFWExt t s defaultConfig

-- | Initialize GLFW, open a window, hook up external signal sources to GLFW keyboard/mouse,
-- maybe perform custom inits, create the signal and /spin it in a tight loop/.
-- Terminates on ESC or when the window is closed.

withGLFWExt :: String -> SignalCreator -> GlfwAdapterConfig -> IO ()
withGLFWExt title sgn cfg = do

    initialize

    preOpenInit cfg

    let (sz, fs) = maybe (Size 0 0, FullScreen)
                         (\s -> (s, Window))
                         (winSize cfg)
    openWindow sz (displayBits cfg) fs
    windowTitle $= title

    windowSizeCallback $= resizeCallback cfg
    windowCloseCallback $= ( endit >> exitSuccess )

    postOpenInit cfg


    (keys, keySink) <- external []
    keyMon <- mkPressReleaseMonitor
    keyCallback $= keyMon keySink

    (mbutt, mbuttSink) <- external []
    mbMon <- mkPressReleaseMonitor
    mouseButtonCallback $= mbMon mbuttSink

    (mouse, mouseSink) <- external (Position 0 0)
    (mwheel, mwheelSink) <- external 0
    let mousePosSnapshot = get mousePos >>= mouseSink
        mouseWheelSnapshot = get mouseWheel >>= mwheelSink


    net <- createSignal (sgn keys mouse mbutt mwheel)
    drive net (mousePosSnapshot >> mouseWheelSnapshot)

    endit

  where
    endit = closeWindow >> extraCleanup cfg >> terminate

drive :: Signal (IO ()) -> IO () -> IO ()
drive sgn preInit = do

    k <- getKey ESC
    when (not $ k == Press) $ do

        preInit
        t <- get time
        time $= 0

        join (superstep sgn t)
        drive sgn preInit

mkPressReleaseMonitor :: (Eq a) => IO (([a] -> IO ()) -> a -> KeyButtonState -> IO ())
mkPressReleaseMonitor = do
    accum <- newIORef []

    return $ \sink key state -> do

        prev <- readIORef accum
        let new = case state of
                Press   -> key : prev
                Release -> key `delete` prev
        writeIORef accum new
        sink new

