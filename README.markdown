# GLFW-Elerea #

Quickly connect [Elerea][elerea] reactive networks to [GLFW][glfw] and start rendering.

Currently supports only default Elerea backend. **TODO.**

[elerea]: http://hackage.haskell.org/package/elerea "Elerea on Hackage"
[glfw]: http://hackage.haskell.org/package/GLFW "GLFW on Hackage"


    runGLFWExt
      "Good Stuff!"
      (myCreateSignal :: GLFWSignal ())

      defaultConfig {
              postOpenInit = do
                  clearColor $= Color4 0 0 0 0
                  clearDepth $= 1
                  depthFunc $= Just Less

              resizeCallback = \size@(Size w h) -> do
                  viewport $= (Position 0 0, size)

                  matrixMode $= Projection
                  loadIdentity
                  perspective 45 (fromIntegral w / fromIntegral h) 0.1 100
                  matrixMode $= Modelview 0
          }


