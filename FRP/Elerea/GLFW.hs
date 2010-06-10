{-|
A quick and simple way to run Elerea networks with GLFW window and event handling.

e.g.

> runGLFWExt
>   "Good Stuff!"
>   (myCreateSignal :: GLFWSignal ())
>   defaultConfig {
>
>           postOpenInit = do
>               clearColor $= Color4 0 0 0 0
>               clearDepth $= 1
>               depthFunc $= Just Less
>               return ()
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

    GLFWSignal,

    -- * Configuring
    GLFWConfig (..), defaultConfig

    -- * Running
    , runGLFWExt, runGLFW

    ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import FRP.Elerea

import Control.Monad ( join, unless )
import Data.List ( delete )

import Data.IORef

import System.Exit


-- | The type of Elerea callbacks that receive initialization results and event
-- sources and create the network.

type GLFWSignal a =
       a
       -- ^ the 'postOpenInit' result
    -> Signal [Key]
       -- ^ keyboard state
    -> (Signal Position, Signal [MouseButton], Signal Int)
       -- ^ mouse position, button state and scroll-wheel value
    -> Signal Size
       -- ^ window size
    -> SignalMonad (Signal (IO ()))


data Step = Sliding     -- ^ feed delta time to the network, once each iteration
          | Fixed DTime -- ^ increment the net in fixed steps


-- | Extra knobs

data GLFWConfig a =

    GAC { winSize :: Maybe Size
          -- ^ window size, 'FullScreen' if Nothing

        , displayBits :: [DisplayBits]
          -- ^ ... for 'openWindow'

        , preOpenInit :: IO ()
          -- ^ initialization to perform /before/ opening the window, after 'initialize'

        , postOpenInit :: IO a
          -- ^ initialization to perform /after/ opening the window, hence, with full OpenGL
          -- context (typically, the usual initGL-type stuff + rendering list construction)

        , extraCleanup :: IO ()
          -- ^ called just before 'terminate'

        , resizeCallback :: WindowSizeCallback
          -- ^ the raw 'WindowSizeCallback'
        
        , timeStep :: Step
          -- ^ timing regimen
        }


-- | Default 'winSize'.
-- (8,8,8) rbg bits \/ 8 alpha bits \/ 24 depth bits for 'displayBits'.
-- 'preOpenInit', 'postOpenInit', 'resizeCallback' and 'extraCleanup' do nothing.
-- 'Sliding' timing.

defaultConfig :: GLFWConfig ()
defaultConfig =

    GAC { winSize = Just (Size 0 0)

        , displayBits = [ DisplayRGBBits 8 8 8
                        , DisplayAlphaBits 8
                        , DisplayDepthBits 24 ]

        , preOpenInit = return ()

        , postOpenInit = return ()

        , resizeCallback = \_ -> return ()

        , extraCleanup = return ()

        , timeStep = Sliding
    }

-- | Just go with the default config.

runGLFW :: String -> GLFWSignal () -> IO ()
runGLFW t s = runGLFWExt t s defaultConfig

-- | Initialize GLFW, open a window, hook up external signal sources to GLFW keyboard/mouse,
-- maybe perform custom inits, create the signal and /spin it in a tight loop/.
-- Terminates on ESC or when the window is closed.

runGLFWExt :: String -> GLFWSignal a -> GLFWConfig a -> IO ()
runGLFWExt title sgn cfg = do

    initialize

    preOpenInit cfg

    let (sz, fs) = maybe (Size 0 0, FullScreen)
                         (\s -> (s, Window))
                         (winSize cfg)
    openWindow sz (displayBits cfg) fs
    windowTitle $= title

    windowCloseCallback $= ( endit >> exitSuccess )

    a <- postOpenInit cfg


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

    (winSize, winSizeSink) <- external (Size 0 0)


    windowSizeCallback $= \s -> winSizeSink s >> resizeCallback cfg s


    net <- createSignal $
            sgn a keys (mouse, mbutt, mwheel) winSize
    drive net (timeStep cfg) (mousePosSnapshot >> mouseWheelSnapshot)

    endit

  where
    endit = closeWindow >> extraCleanup cfg >> terminate


drive :: Signal (IO ()) -> Step -> IO () -> IO ()
drive sgn tstep preInit = do

    k <- getKey ESC
    unless (k == Press) $ do

        preInit
        t <- get time
        time $= 0

        join $ case tstep of
                Sliding -> superstep sgn t
                Fixed d -> do
                    (act, t') <- stepper t (return ())
                    get time >>= (time $=) . (+ t')
                    return act

        drive sgn tstep preInit

  where
    stepper timeAcc act | timeAcc >= dt = superstep sgn dt >>= stepper (timeAcc - dt)
                        | otherwise     = return (act, timeAcc)
    Fixed dt = tstep


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

