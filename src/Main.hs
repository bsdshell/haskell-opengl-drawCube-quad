{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Main where

import AronGraphic
import AronModule
import AronOpenGL
import Control.Arrow
import Control.Concurrent
import Control.Lens hiding (pre, re)
import Control.Monad
import Control.Monad (unless, when)
import qualified Control.Monad.State as CMS
-- import AronDevLib

import Data.Array.IO
import qualified Data.Array.IO as DAO
import Data.Complex
import Data.IORef
import Data.Int
import qualified Data.List as DL
import qualified Data.Map as DM
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.StateVar
import Data.Typeable
import Data.Typeable (typeOf)
import qualified Data.Vector as VU
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Float.RealFracMethods
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GLU.Matrix as GM
import qualified Graphics.UI.GLFW as G
import qualified Graphics.UI.GLUT as GLUT
import Language.Haskell.Interpreter
import System.Directory
import System.Environment
import System.Exit
import System.IO
import qualified Text.Printf as PR

-- |
--
--   | --------------------------------------------------------------------------------
--   | compile: run.sh
--   | ghc -i/Users/cat/myfile/bitbucket/haskelllib -o file file.hs
--   |
--   | KEY: keyboard example, keypress example, modifyIORef example,
--   |
--   | Tuesday, 09 November 2021 11:56 PST
--   |
--   | TODO: Combine Cam{..} and Step{..} in one function with Keyboard input
--   |     Current issue: try to implement orthogonal projective with key one press
--   |                    but Step{..} and Cam{..} are different type class.
--   |
--   | mainLoop w refCam refStep refCount lssVex
--   | keyboardRot refCam refStep (fromIntegral width) (fromIntegral height)
--
--   @
--   data Cam = Cam{alpha::Double, beta::Double, gramma::Double, dist::Double} deriving(Show)
--
--   data Step = Step{xx::Double, yy::Double, zz::Double, ww::Double} deriving(Show)
--   initCam = Cam{alpha=0.0, beta=0.0, gramma=0.0, dist = 0.0}
--   initStep = Step{xx=0.0, yy=0.0, zz=0.0, ww = 0.01}
--
--   SEE: Check the following code why the cube does not draw properly
--   NOTE: /Users/aaa/myfile/bitbucket/opengl/KeyBoardRotate/cube.c
--   @

renderText :: String -> IO ()
renderText str = do
  preservingMatrix $ do
    GL.scale (1 / scaleFont :: GL.GLdouble) (1 / scaleFont) 1
    GLUT.renderString GLUT.Roman str

mymain :: IO ()
mymain = do
  successfulInit <- G.init
  G.windowHint (G.WindowHint'DoubleBuffer True)
  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
    mw <- G.createWindow 1000 1000 "PlotGeometry" Nothing Nothing
    maybe' mw (G.terminate >> exitFailure) $ \window -> do
      G.makeContextCurrent mw

      ref <- newIORef initCam
      refStep <- newIORef initStep
      refGlobal <- newIORef initGlobal
      globalRef <- readIORef refGlobal
      writeIORef refGlobal globalRef
      refFrame <- timeNowMilli >>= \x -> newIORef FrameCount {frameTime = x, frameCount = 1, frameNewCount = 0, frameIndex = 0}

      let rr = initRectGrid
      let nx = div (xCount_ rr) 2
      let ny = div (yCount_ rr) 2

      let blockAttr = BlockAttr {isFilled_ = False, typeId_ = 0, tetrisNum_ = 0, color_ = green}
      ioArray <- DAO.newArray ((- nx, - ny, 0), (nx - 1, ny - 1, 0)) blockAttr :: IO (IOArray (Int, Int, Int) BlockAttr)
      animaStateArr <- initAnimaState

      -- mymain xxx
      mainLoopTest window ref refStep refGlobal refFrame animaStateArr [] ioArray
      G.destroyWindow window
      G.terminate
      exitSuccess

getModelviewMatrix :: IO [GLfloat]
getModelviewMatrix = do
  let stateVar = GL.matrix (Just $ Modelview 16) :: StateVar (GLmatrix GLfloat)
  m1 <- Data.StateVar.get stateVar
  pre m1
  -- ls <- getMatrixComponents RowMajor m1  -- [GLfloat]
  ls <- getMatrixComponents ColumnMajor m1 -- [GLfloat]
  pre ls
  writeFileList "/tmp/m1.x" $ map show ls
  return ls

flipAxis :: XYZAxis -> XYZAxis -> XYZAxis
flipAxis axisOld axisNew
  | x' = XYZAxis {xa = xor x x', ya = False, za = False}
  | y' = XYZAxis {xa = False, ya = xor y y', za = False}
  | z' = XYZAxis {xa = False, ya = False, za = xor z z'}
  | otherwise = XYZAxis {xa = False, ya = False, za = False}
  where
    x = xa axisOld
    y = ya axisOld
    z = za axisOld
    x' = xa axisNew
    y' = ya axisNew
    z' = za axisNew
    xor :: Bool -> Bool -> Bool
    xor True True = False
    xor True False = True
    xor False True = True
    xor False False = False

xAxis :: XYZAxis
xAxis = XYZAxis {xa = True, ya = False, za = False}

yAxis :: XYZAxis
yAxis = XYZAxis {xa = False, ya = True, za = False}

zAxis :: XYZAxis
zAxis = XYZAxis {xa = False, ya = False, za = True}

initXYZAxis :: XYZAxis
initXYZAxis = XYZAxis {xa = False, ya = False, za = False}

rotateN :: Int -> [[a]] -> [[a]]
rotateN n = foldl (\f g -> f . g) id $ take (abs n) $ repeat $ n >= 0 ? (reverse . DL.transpose) $ (DL.transpose . reverse)

--                                     shape
--                       color            |
--          tetris type     |             |
--     Global ID  |         |             |
--           |    |         |             |
mkTetris1 :: Int -> Int -> (Int, Int, Color3 GLdouble, [[Int]])
mkTetris1 bt bid = (bt, bid, white, bk)
  where
    bk =
      [ [0, 0, 1, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 0, 1, 1, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0]
      ]


initGlobal :: GlobalRef
initGlobal =
  GlobalRef
    { str_ = "",
      cursor_ = (0.0, 0.0),
      xyzAxis_ = initXYZAxis,
      mousePressed_ = (False, (0.0, 0.0)),
      drawRectX_ = (Vertex3 (-0.2) (-0.2) (0.0 :: GLfloat), Vertex3 0.2 0.2 (0.0 :: GLfloat)),
      tranDrawRectX_ = Vector3 0.0 0.0 (0.0 :: GLdouble),
      fovDegree_ = 100.0,
      drawPts_ = [[Vertex3 0.0 0.0 0.0]],
      randomWalk_ = [Vertex3 0.0 0.0 0.0, Vertex3 0.1 0.1 0.1],
      boardMap_ = DM.empty,
      boardMap1_ = DM.empty,
      moveX_ = 0,
      moveY_ = 0,
      block1_ =
        [ ((0 - 2, 0 + 8), gray),
          ((1 - 2, 0 + 8), gray),
          ((2 - 2, 0 + 8), gray),
          ((3 - 2, 0 + 8), gray),
          ((4 - 2, 0 + 8), gray)
        ],
      rectGrid_ = initRectGrid,
      centerBrick_ = map (\y -> map (\x -> (x - 2, y - 2)) [0 .. len (head $ bk1_ initGlobal) - 1]) $ reverse [0 .. len (bk1_ initGlobal) - 1],
      bk1_ =
        [ [0, 0, 0, 0, 0],
          [0, 0, 1, 0, 0],
          [0, 1, 1, 1, 0],
          [0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0]
        ],
      bk2_ =
        [ [(0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green)],
          [(0, 0, green), (0, 0, green), (1, 0, green), (0, 0, green), (0, 0, green)],
          [(0, 0, green), (1, 0, green), (1, 0, green), (1, 0, green), (0, 0, green)],
          [(0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green)],
          [(0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green), (0, 0, green)]
        ],
      rot_ = False,
      rotDeg_ = 0,
      time1_ = 0,
      count1_ = 10000000,
      rotN_ = 0,
      blockCount_ = 1,
      tetrisCount_ = 1,
      tetris1_ = mkTetris1 (blockCount_ initGlobal) 0,
      tetris1X_ = (BlockAttr {isFilled_ = True, typeId_ = 1, tetrisNum_ = (blockCount_ initGlobal), color_ = blue}, bk1_ initGlobal),
      isPaused_ = False
    }


-- |
--    KEY:
--    NOTE: USED
keyBoardCallBack2 :: IORef Step -> IORef GlobalRef -> IOArray (Int, Int, Int) BlockAttr -> G.KeyCallback
keyBoardCallBack2 refStep refGlobalRef ioArray window key scanCode keyState modKeys = do
  pp "keyBoardCallBack in $b/haskelllib/AronOpenGL.hs"
  putStrLn $ "inside =>" ++ show keyState ++ " " ++ show key
  globalRef <- readIORef refGlobalRef
  let axisOld = xyzAxis_ globalRef
  let fovOld = fovDegree_ globalRef
  logFileG ["fovOld=" ++ show fovOld]
  rr <- readIORef refGlobalRef <&> rectGrid_
  case keyState of
    ks
      | ks `elem` [G.KeyState'Pressed, G.KeyState'Repeating] -> do
        -- G.KeyState'Pressed -> do
        -- write Step{...} to ref
        case key of
          k
            | k == G.Key'Right -> modifyIORef refStep (\s -> s{xx = _STEP})
            --   | k == G.Key'Right -> writeIORef refStep Step {xx = _STEP, yy = 0.0, zz = 0.0, ww = 0.0}
            | k == G.Key'Left -> modifyIORef refStep (\s -> s{xx = - _STEP})
            --   | k == G.Key'Right -> writeIORef refStep Step {xx = _STEP, yy = 0.0, zz = 0.0, ww = 0.0}
  
            | k == G.Key'Up -> writeIORef refStep Step {xx = 0.0, yy = _STEP, zz = 0.0, ww = 0.0}
            | k == G.Key'Down -> writeIORef refStep Step {xx = 0.0, yy = - _STEP, zz = 0.0, ww = 0.0}
            | k == G.Key'9 -> writeIORef refStep Step {xx = 0.0, yy = 0.0, zz = _STEP, ww = 0.0}
            | k == G.Key'0 -> writeIORef refStep Step {xx = 0.0, yy = 0.0, zz = - _STEP, ww = 0.0}
            | k == G.Key'8 -> writeIORef refStep Step {xx = 0.0, yy = 0.0, zz = 0.0, ww = _STEP}
            | k == G.Key'7 -> writeIORef refStep Step {xx = 0.0, yy = 0.0, zz = 0.0, ww = - _STEP}
            | k == G.Key'X -> modifyIORef refGlobalRef (\s -> s {xyzAxis_ = flipAxis (xyzAxis_ s) xAxis})
            --                                  ↑
            --                                  + -> Update Coord to YZ-plane

            | k == G.Key'Y -> modifyIORef refGlobalRef (\s -> s {xyzAxis_ = flipAxis (xyzAxis_ s) yAxis})
            --                                  ↑
            --                                  + -> Update Coord to XZ-plane

            | k == G.Key'Z -> modifyIORef refGlobalRef (\s -> s {xyzAxis_ = flipAxis (xyzAxis_ s) zAxis})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane

            -- zoom out
            | k == G.Key'O -> modifyIORef refGlobalRef (\s -> s {fovDegree_ = fovDegree_ s + 5.0})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane
            -- zoom in
            | k == G.Key'I -> modifyIORef refGlobalRef (\s -> s {fovDegree_ = fovDegree_ s - 5.0})
            --                                  ↑
            --                                  + -> Update Coord to XY-plane

            -- TODO: In orthogonal projective status,
            | k == G.Key'Space -> writeIORef refStep initStep

            | k == G.Key'W -> do
              fw "Rotate Block"
              pp "rotate me"

            | k == G.Key'P -> do
              pp "Pause"
            | k == G.Key'A -> do
              pp "rotateN 1"
            | k == G.Key'L || k == G.Key'R -> do
              print "kk"

            | k == G.Key'U -> do
              bmap <- readIORef refGlobalRef <&> boardMap_
              bmapX <- readIORef refGlobalRef <&> boardMap1_
              centerBrick <- readIORef refGlobalRef <&> centerBrick_
              rotN <- readIORef refGlobalRef <&> rotN_
              bk1 <- readIORef refGlobalRef <&> bk1_
              tet <- readIORef refGlobalRef <&> tetris1_
              let bk1' = rotateN rotN bk1
              let bk1'X = rotateN rotN (tet ^. _4)

              -- let mk = (join . join) $ (map . map) fst $ (map . filter) (\(_, n) -> n == 1) $ (map . zip) centerBrick bk1
              let lz = join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk1'
              let ls = map fst lz
              let lzX = join $ (map . filter) (\(_, n) -> n == 1) $ (zipWith . zipWith) (\x y -> (x, y)) centerBrick bk1'X
              let lsX = map fst lzX

              print "kk"
            | k == G.Key'D -> do
              print "kk"
            | otherwise -> pp $ "Unknown Key Press" ++ show key
      | ks == G.KeyState'Released -> do
        -- G.KeyState'Released -> do
        if key == G.Key'Right then pp "Release Key => Right" else pp "Press No Right"
        if key == G.Key'Left then pp "Release Key => left" else pp "Press No Right"
        if key == G.Key'Up then pp "Release Key => up" else pp "Release No Up"
        if key == G.Key'Down then pp "Release Key => Down" else pp "Release No Down"
      -- if key == G.Key'R  then modifyIORef refGlobalRef (\x -> x{moveX_ = 0}) else pp "Release No Down"
      -- if key == G.Key'L  then modifyIORef refGlobalRef (\x -> x{moveY_ = 0}) else pp "Release No Down"
      | otherwise -> pp "Unknow keyState"
  when
    (key == G.Key'Escape && keyState == G.KeyState'Pressed)
    (G.setWindowShouldClose window True)


-- |
--    KEY: getter for fovDegree_
--    NOTE: zoom in, zoom out
getFOVDegree :: IORef GlobalRef -> IO GLdouble
getFOVDegree ioGlobalRef = readIORef ioGlobalRef <&> fovDegree_

-- |
--    KEY: getter for xyzAxis_
getXYZAxis :: IORef GlobalRef -> IO XYZAxis
getXYZAxis ioGlobalRef = readIORef ioGlobalRef <&> xyzAxis_


-- |
--    KEY:
--
--    t0 pressed
--       (x0, y0)
--
--    t1 released
--       (x1, y1)
mouseCallback :: IORef GlobalRef -> G.MouseButtonCallback
mouseCallback globalRef window but butState mk = do
  case butState of
    G.MouseButtonState'Pressed -> do
      case but of
        v
          | v == G.MouseButton'1 -> do
            (fbw, fbh) <- G.getFramebufferSize window
            pos <- G.getCursorPos window >>= \(x, y) -> return (rf x, rf y) :: IO (GLfloat, GLfloat)
            ws <- G.getWindowSize window
            let str = PR.printf "cx=%.2f cy=%.2f wx=%d wy=%d bx=%d by=%d" (fst pos) (snd pos) (fst ws) (snd ws) fbw fbh :: String

            gRef <- readIORef globalRef
            -- ↑
            -- +---------------------------+
            --                             ↓
            -- writeIORef globalRef $ setStr gRef str
            modifyIORef globalRef (\x -> x {str_ = str})

            -- newGlobalRef <- readIORef globalRef >>= return . setCursor pos
            -- readIORef globalRef >>= return . setCursor pos >>= \x -> writeIORef globalRef $ setMousePressed (True, pos) x
            -- readIORef globalRef >>= return . setCursor pos >>= \x -> writeIORef globalRef $ setMousePressed (True, pos) x
            modifyIORef globalRef (\x -> x {cursor_ = pos})
            modifyIORef globalRef (\x -> x {mousePressed_ = (True, pos)})

            --  ↑
            --  +--------------------------------------------------+
            --                                                     ↓
            -- writeIORef globalRef $ setMousePressed (True, pos) newGlobalRef

            pp str
          | otherwise -> pp "No button pressed"
    G.MouseButtonState'Released -> do
      -- pos <- G.getCursorPos window >>= \(x, y) -> return $ (rf x, rf y) :: IO (GLfloat, GLfloat)
      let pos = (0.0, 0.0)
      -- readIORef globalRef >>= \x -> writeIORef globalRef $ setMousePressed (False, pos) x
      modifyIORef globalRef (\x -> x {mousePressed_ = (False, pos)})

      pp "Button Released"





-- |
--
--               minY_
--                |
--        minX_ - +  - -> maxX_
--                |
--               maxY_
--
--        |<-    xCount_    ->|
--                20
initRectGrid :: RectGrid
initRectGrid =
  RectGrid
    { minX_ = -1.0,
      maxX_ = 1.0,
      minY_ = -1.0,
      maxY_ = 1.0,
      xCount_ = 20,
      yCount_ = 20,
      xEdge_ = 0.1,
      yEdge_ = 0.01,
      rotStep = 20
    }


  
-- |
--
--   The cube is from following URL
--   http://localhost/html/indexUnderstandOpenGL.html
drawCube :: IO ()
drawCube = do
  preservingMatrix $ do
    renderPrimitive Quads $
      mapM_
        ( \v -> do
            color green
            vertex v
        )
        ls_top
    renderPrimitive Quads $
      mapM_
        ( \v -> do
            color magenta
            vertex v
        )
        ls_bot
    renderPrimitive Quads $
      mapM_
        ( \v -> do
            color cyan
            vertex v
        )
        ls_front
  
    renderPrimitive Quads $
      mapM_
        ( \v -> do
            color blue
            vertex v
        )
        ls_back
  
    renderPrimitive Quads $
      mapM_
        ( \v -> do
            color yellow
            vertex v
        )
        ls_left
 
    renderPrimitive Quads $
      mapM_
        ( \v -> do
            color gray
            vertex v
        )
        ls_right
  where
    b = 0.3 :: GLfloat
    p0 = Vertex3 b    b    (-b)
    p1 = Vertex3 (-b) b    (-b)   
    p2 = Vertex3 (-b) b    b
    p3 = Vertex3 b    b    b
  
    q0 = Vertex3 b   (-b) (-b)
    q1 = Vertex3 (-b) (-b) (-b) 
    q2 = Vertex3 (-b) (-b) b
    q3 = Vertex3 b    (-b) b
    
    ls_top = [p0, p1, p2, p3]
    ls_bot = [q0, q1, q2, q3]
    ls_front = [p3, p2, q2, q3]
    ls_back = [p0, p1, q1, q0]
    ls_left = [p1, p2, q2, q1]
    ls_right = [p0, p3, q3, q0]


data AnimaState = AnimaState
  { animaTime_ :: Int,
    animaIndex_ :: Int,
    animaInterval_ :: Int,
    animaSlot_ :: Int
  }
  deriving (Show, Eq)

initAnimaState :: IO (IOArray Int AnimaState)
initAnimaState = do
  currTime <- timeNowMilli <&> fi
  let an = AnimaState {animaTime_ = currTime, animaIndex_ = 0, animaInterval_ = 1000, animaSlot_ = 0}
  let anx = AnimaState {animaTime_ = currTime, animaIndex_ = 0, animaInterval_ = 4000, animaSlot_ = 0}
  -- DAO.newArray (0, 5) an
  let ls = [an, anx, an, an, an, an]
  DAO.newListArray (0, 5) ls

readAnimaState :: IOArray Int AnimaState -> Int -> Int -> IO (Bool, Int, AnimaState)
readAnimaState arr ix interval= do
  currTime <- timeNowMilli <&> fi
  an <- DAO.readArray arr ix
  DAO.writeArray arr ix  an{animaInterval_ = interval}
  oldTime <- DAO.readArray arr ix <&> animaTime_
  interval <- DAO.readArray arr ix <&> animaInterval_
  oldIndex <- DAO.readArray arr ix <&> animaIndex_
  let newIndex = oldIndex + 1
  let isNext = currTime - oldTime >= interval
  if isNext
    then do
      return (isNext, newIndex, an {animaTime_ = currTime, animaIndex_ = newIndex, animaSlot_ = ix})
    else do
      return (isNext, oldIndex, an{animaSlot_ = ix})

writeAnimaState :: IOArray Int AnimaState -> AnimaState -> IO ()
writeAnimaState arr an = do
  let ix = animaSlot_ an
  DAO.writeArray arr ix an



mainLoopTest ::
  G.Window ->
  IORef Cam ->
  IORef Step ->
  IORef GlobalRef ->
  IORef FrameCount ->
  IOArray Int AnimaState ->
  [[Vertex3 GLfloat]] ->
  DAO.IOArray (Int, Int, Int) BlockAttr ->
  IO ()
mainLoopTest w refCam refStep refGlobal refGlobalFrame animaStateArr lssVex ioArray = unless' (G.windowShouldClose w) $ do
  (width, height) <- G.getFramebufferSize w
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  
  GL.clear [ColorBuffer, DepthBuffer]
  -- Enable income pixel depth test
  -- GL.depthFunc $= Just Less
  -- GL.depthFunc $= Just Lequal
  GL.depthFunc $= Just Lequal
  -- GL.cullFace  $= Just Back
  -- GL.depthFunc $= Just Less
  
  G.setKeyCallback w (Just $ keyBoardCallBack2 refStep refGlobal ioArray) -- AronOpenGL
  G.setMouseButtonCallback w (Just $ mouseCallback refGlobal) -- mouse event
  -- lightingInfo
  loadIdentity -- glLoadIdentity

  step <- readIORef refStep
  xyzAxis <- getXYZAxis refGlobal
  fovNew <- getFOVDegree refGlobal
  case xyzAxis of
    --                                 +---> YZ-plane
    --                                 ↓
    var
      | xa var -> GL.lookAt (Vertex3 1.0 0 0 :: Vertex3 GLdouble) (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 0 1 0 :: Vector3 GLdouble)
      --                               +---> XZ-plane
      --                               ↓
      | ya var -> GL.lookAt (Vertex3 0 1.0 0 :: Vertex3 GLdouble) (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 1 0 0 :: Vector3 GLdouble)
      --                                 +---> XY-plane
      --                                 ↓
      | za var -> do
        let zf = 0.5        
        perspective fovNew 1.0 zf (zf + 4.0)
        GL.lookAt (Vertex3 0 0 1.0 :: Vertex3 GLdouble) (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 0 1 0 :: Vector3 GLdouble)
        logFileG ["zAxis33"]
      | otherwise -> do
        GM.lookAt (Vertex3 0.2 0.2 0.2 :: Vertex3 GLdouble) (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 0 1 0 :: Vector3 GLdouble)
        keyboardRot refCam refStep (fromIntegral width) (fromIntegral height)

        preservingMatrix $ do
          loadIdentity
          fw "loadIdentity"
          ls <- getModelviewMatrix
          let lss = partList 4 ls
          printMat lss
          GM.lookAt (Vertex3 0 2 3 :: Vertex3 GLdouble) (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vector3 0 1 0 :: Vector3 GLdouble)
          fw "getModelviewMatrix"
          lr <- getModelviewMatrix
          let lt = partList 4 lr
          printMat lt

  renderCoordinates

  
  let anima0 = 0
  let interval = 100  -- larger number is slower
  (isNext0, index, animaState) <- readAnimaState animaStateArr anima0 interval
  logFileG ["index00=" ++ show index]
  logFileG ["isNext00=" ++ show isNext0]
  
  preservingMatrix $ do
    let del = 360/100.0
    case index of
          v | v < 100 -> do
                  rotate (del * fi index) (Vector3 1 0 0 :: Vector3 GLdouble)
                  drawCube
                  writeAnimaState animaStateArr animaState
            | v >= 100 && v < 200 -> do
                  rotate (del * fi index) (Vector3 0 1 0 :: Vector3 GLdouble)
                  drawCube
                  writeAnimaState animaStateArr animaState
            | v >= 200 && v < 300 -> do
                  rotate (del * fi index) (Vector3 0 0 1 :: Vector3 GLdouble)
                  drawCube
                  writeAnimaState animaStateArr animaState
            | v >= 300 -> do
                  drawCube
                  writeAnimaState animaStateArr animaState {animaIndex_ = 0}
            | otherwise -> do
                  drawCube
                  writeAnimaState animaStateArr animaState
    -- drawCube2
    -- drawCube
  -- writeAnimaState animaStateArr animaState
  
  drawFinal w ioArray initRectGrid

  let anima1 = 1
  let intervalx = 0  -- larger number is slower
  (isNext1, index1, animaState1) <- readAnimaState animaStateArr anima1 intervalx
  let fn = "/tmp/img_" ++ show (index1 + 1000) ++ ".png"
  saveImageOpenGL w fn
  writeAnimaState animaStateArr animaState1{animaIndex_ = index1}
  mainLoopTest w refCam refStep refGlobal refGlobalFrame animaStateArr lssVex ioArray

drawFinal :: G.Window -> IOArray (Int, Int, Int) BlockAttr -> RectGrid -> IO ()
drawFinal w arr rr = do
  G.swapBuffers w
  G.pollEvents

main = do
  argList <- getArgs
  if len argList > 0
    then do
      case head argList of
        "-h" -> pp "help" 
        _ -> do
          print $ "Wrong option => " ++ head argList ++ ", -h => Help"
    else mymain



