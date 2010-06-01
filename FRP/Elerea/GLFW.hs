{-|
A quick and simple way to run "Elerea" networks with "GLFW" window and even handling.

e.g.

> withGLFWExt
>   "Good Stuff!"
>   (myCreateSignal :: SignalCreator)
>   defaultConfig {
>
>           resizeCallback = \size@(Size w h) -> do
>               viewport $= (Position 0 0, size)
>               matrixMode $= Projection
>               loadIdentity
>               perspective 45 (fromIntegral w / fromIntegral h) 0.1 100
>               matrixMode $= Modelview 0
>
>           extraInit = do
>               clearColor $= Color4 0 0 0 0
>               clearDepth $= 1
>               depthFunc $= Just Less
>               lighting $= Enabled
>               light (Light 0) $= Enabled
>               shadeModel $= Flat
>       }

-}



module FRP.Elerea.GLFW (

    -- * Configuring
    SignalCreator, GlfwAdapterConfig (..), defaultConfig

    -- * Running
    , withGLFW, withGLFWExt

    ) where


import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import FRP.Elerea

import Control.Monad ( join, when )
import Data.List ( delete )

import Data.IORef

import System.Exit



-- | The type of Elerea callback that will create the network.

type SignalCreator =
       Signal [Key]                     -- ^ keyboard state
    -> Signal Position                  -- ^ current mouse position
    -> Signal [MouseButton]             -- ^ mouse button state
    -> Signal Int                       -- ^ current scroll-wheel value
    -> SignalMonad (Signal (IO ()))

-- | Extra knobs

data GlfwAdapterConfig =

    GAC { winSize :: Maybe Size                    -- ^ window size, 'FullScreen' if Nothing
        , displayBits :: [DisplayBits]             -- ^ ... for 'openWindow'
        , extraInit :: IO ()                       -- ^ initialization to perform after opening the window
        , extraCleanup :: IO ()                    -- ^ called just before terminate
        , resizeCallback :: WindowSizeCallback     -- ^ the raw 'WindowSizeCallback'
        }

-- | default 'winSize',
-- (8,8,8) rbg bits \/ 8 alpha bits \/ 24 depth bits for 'displayBits',
-- 'extraInit', 'resizeCallback' and 'extraCleanup' do nothing.

defaultConfig :: GlfwAdapterConfig
defaultConfig =

    GAC { winSize = Just (Size 0 0)

        , displayBits = [ DisplayRGBBits 8 8 8
                        , DisplayAlphaBits 8
                        , DisplayDepthBits 24 ]

        , extraInit = return ()

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

    let (sz, fs) = maybe (Size 0 0, FullScreen)
                         (\s -> (s, Window))
                         (winSize cfg)
    openWindow sz (displayBits cfg) fs
    windowTitle $= title

    (keys, keySink) <- external []
    keyMon <- mkPressReleaseMonitor
    keyCallback $= keyMon keySink

    (mbutt, mbuttSink) <- external []
    mbMon <- mkPressReleaseMonitor
    mouseButtonCallback $= mbMon mbuttSink


    windowSizeCallback $= resizeCallback cfg
    windowCloseCallback $= ( endit >> exitSuccess )

    (mouse, mouseSink) <- external (Position 0 0)
    (mwhell, mwheelSink) <- external 0
    let mousePosSnapshot = get mousePos >>= mouseSink
        mouseWheelSnapshot = get mouseWheel >>= mwheelSink

    extraInit cfg

    net <- createSignal (sgn keys mouse mbutt mwhell)
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

