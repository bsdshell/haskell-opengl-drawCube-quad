{-# LANGUAGE MultiWayIf #-}

module AronGraphic where

import           Control.Concurrent (threadDelay)
import           Control.Monad                        (unless, when)
import           Control.Monad
import           Data.IORef
import           Data.Maybe
import           Data.Set                             (Set, delete, fromList)
                 
import qualified                            Data.Set         as S
import qualified                            Data.List        as L
import Graphics.Rendering.OpenGL            as               GL
import Graphics.Rendering.OpenGL.GLU.Matrix as               GM
import qualified                            Graphics.UI.GLFW as FW
import qualified                            Graphics.UI.GLUT as GLUT

import           System.Exit
import           System.IO
import           System.Random
import           GHC.Real
import           Data.Complex

import qualified Data.Vector as VU
import           AronModule
                 
-- import qualified Data.Vector.Unboxed as VU
-- Unboxed only support
{-|
Unboxed Arrays: Data.Vector.Unboxed

    Bool
    ()
    Char
    Double
    Float
    Int
    Int8, 16, 32, 64
    Word
    Word8, 16, 32, 64
    Complex a's, where 'a' is in Unbox
    Tuple types, where the elements are unboxable
-}



--import Linear.V3
--import Linear.V3(cross)
--import Linear.Vector
--import Linear.Matrix
--import Linear.Projection as P
--import Linear.Metric(norm, signorm)

epsilon_ = 0.00001

lightDiffuse ::Color4 GLfloat
lightDiffuse = Color4 0.6 1.0 0.5 0.6

lightAmbient ::Color4 GLfloat
lightAmbient = Color4 0.0 0.0 1.0 1.0

lightPosition ::Vertex4 GLfloat
lightPosition = Vertex4 1.0 1.0 1.2 0.0

lightSpecular ::Color4 GLfloat
lightSpecular = Color4 1.0 0.7 1.0 0.8

{-|
    === KEY: draw string, render string

    @
    GL.scale (1/scaleFont :: GL.GLdouble) (1/scaleFont) 1
    GLUT.renderString GLUT.Roman str

    strWidth <- GLUT.stringWidth GLUT.Roman str
    strHeight <- GLUT.stringHeight GLUT.Roman str
    @
-}
scaleFont::GLdouble
scaleFont = 3000.0
                
-- | --------------------------------------------------------------------------------
-- | Fri Dec  7 14:35:38 2018
-- | three colors: data Color3 a = Color3 !a !a !a
-- | Add more colors: Sun 27 Jun 23:56:15 2021 
red   = Color3 1 0 0 :: Color3 GLdouble
green = Color3 0 1 0 :: Color3 GLdouble
blue  = Color3 0 0 1 :: Color3 GLdouble
white = Color3 1 1 1 :: Color3 GLdouble
black = Color3 0 0 0 :: Color3 GLdouble
gray  = Color3 0.47 0.47 0.47 :: Color3 GLdouble
gray1  = Color3 0.8 0.8 0.8 :: Color3 GLdouble
cyan    = Color3 0 1 1 :: Color3 GLdouble
magenta = Color3 1.0 0 1.0 :: Color3 GLdouble
yellow  = Color3 1.0 1.0 0 :: Color3 GLdouble

data SegEndPt = No      -- no pt, just a segment
                | End   -- end pt
                | Beg   -- begin pt
                | Both  -- begin and end pts
                | Cen   -- center pt
                | All   -- all pts: begin, end and ceneter

--   /\
--   | ccw
--    -->
--   | cw
--   V
-- | counter clockwise | clock wise
data NormalDir = NCCW | NCW deriving (Eq)


{-|
    === Compute the distance between two points

    \( v = (x, y, z) \)

    \( \| v \| = \sqrt{ x^2 + y^2 + z^2} = \sqrt{ v \cdot v} \)

    >let v1 = Vertex3 1 2 3
    >let v2 = Vertex3 2 3 4
    >dist v1 v2
-}
dist::Vertex3 GLfloat -> Vertex3 GLfloat -> GLfloat
dist (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) = sqrt $ (x1 - x0)**2 + (y1 - y0)**2 + (z1 - z0)**2
  
{-|
    === Compute the distance between two points

    * Same as 'dist'

    \( v = (x, y, z) \)

    \( \| v \| = \sqrt{ x^2 + y^2 + z^2} = \sqrt{ v \cdot v} \)

    >let v1 = Vertex3 1 2 3
    >let v2 = Vertex3 2 3 4
    >distX v1 v2
-}  
distX::Vertex3 GLfloat -> Vertex3 GLfloat -> GLfloat
distX = dist

{-|
    === Compute the norm-squared

    \( v = (x, y, z)\)

    \( |v|^2 = x^2 + y^2 + z^2 \)

    >let v1 = Vertex3 1 2 3
    >let v2 = Vertex3 2 3 4
    >sqdist v1 v2
-}
sqdist::Vertex3 GLfloat-> Vertex3 GLfloat-> GLfloat
sqdist (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) = (x1 - x0)**2 + (y1 - y0)**2 + (z1 - z0)**2

-- | dot product for Vector3
dot3ve::(Num a)=>Vector3 a-> Vector3 a-> a
dot3ve v1 v2 = dot3vx (v2x v1) (v2x v2)

-- | dot product for Vertex3
dot3vx::(Num a)=>Vertex3 a-> Vertex3 a-> a
dot3vx (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) = x0*x1 + y0*y1 + z0*z1

{-|
    === KEY: add vertex to vector, vector to vertex, translate vectex to other vectex, affine transform

    0 + vector      => vector
    vextex + vector => vextex
    vextex - vectex => vector
    0      -      0 => 0 vector
-}
(+:)::(Num a)=>Vertex3 a-> Vector3 a-> Vertex3 a
(+:) (Vertex3 x0 y0 z0) (Vector3 x1 y1 z1) = Vertex3 (x0 + x1) (y0 + y1) (z0 + z1)

{-|
    === KEY: vertex to vector, vector from two Vertex3, vector = Vertex3 - Vertex3
-}
(-:)::(Num a)=>Vertex3 a-> Vertex3 a-> Vector3 a
(-:) (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) = Vector3 (x0 - x1) (y0 - y1) (z0 - z1)

(⊥)::Vector3 GLfloat -> Vector3 GLfloat
(⊥) (Vector3 x y z) = Vector3 y (-x) z

{-|
   === KEY: perpendicular vector

   URL: <http://xfido.com/image/perpendicular_vector.svg perpendicular_vector >
-}
perpcw::(Num a)=>Vector3 a->Vector3 a
perpcw (Vector3 x y z) = Vector3 y (-x) z
  
{-|
   === KEY: perpendicular vector

   URL: <http://xfido.com/image/perpendicular_vector.svg perpendicular_vector >
-}
perpccw::(Num a)=>Vector3 a->Vector3 a
perpccw (Vector3 x y z) = Vector3 (-y) x z

{-|
   === KEY: Scalar multiplies a vector
-}
(*:)::(Num a)=>a-> Vector3 a-> Vector3 a
(*:) k (Vector3 x y z) = Vector3 (k*x) (k*y) (k*z)

(/:)::(Num a, Fractional a)=> Vector3 a -> Integer -> Vector3 a
(/:) (Vector3 x y z) n = Vector3 x' y' z'
  where
    x' = x / (fi n)
    y' = y / (fi n)
    z' = z / (fi n)

{-|
  KEY: scalar multiplies vertex, vertex multiplies scalar
-}
(**:)::GLfloat -> Vertex3 GLfloat-> Vertex3 GLfloat
(**:) t (Vertex3 x0 y0 z0) = Vertex3 (t*x0) (t*y0) (t*z0)

(*>:)::GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat
(*>:) k (Vertex3 x y z) = Vertex3 (k*x) (k*y) (k*z)

(>>:)::GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
(>>:) k (Vector3 x y z) = Vector3 (k*x) (k*y) (k*z)

(.>)::Int
(.>) = 3

-- | dot product  odot
(⊙)::Vector3 GLfloat -> Vector3 GLfloat -> GLfloat
(⊙) (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) = (x0*x1) + (y0*y1) + (z0*z1)

(∈)::(Num a, Ord a)=>a -> [a]-> Bool
(∈) a [b, c] = a >= b && a <= c

(∘)::(Num a)=>a-> Vector3 a-> Vector3 a
(∘) k (Vector3 x y z) = Vector3 (k*x) (k*y) (k*z)


(∎)::GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
(∎) k (Vector3 x y z) = Vector3 (k*z) (k*y) (k*z)

v2x::(Num a)=>Vector3 a-> Vertex3 a
v2x (Vector3 x y z) = Vertex3 x y z

x2v::(Num a)=>Vertex3 a-> Vector3 a
x2v (Vertex3 x y z) = Vector3 x y z

-- Tue Jan  1 17:57:32 2019
-- remove it
--smulv::GLfloat -> Vector3 GLfloat ->Vector3 GLfloat
--smulv k (Vector3 x y z) = Vector3 (k*x) (k*y) (k*z)

-- | Shorten realToFrac
--rf::(Real a, Fractional b) => a -> b
--rf = realToFrac



{-|
    === Form a __skew symmetric matrix__ from \( \color{red}{Vertex3}  \)
    __cross product__ of a x b = [a] b where [a] is skew symmetric matrix from vector a
    colIndex x rowIndex = [c][r]

    >Vertex3 a1 a2 a3

    \[
     \begin{bmatrix}
            0    & -a_3 & a_2 \\
            a_3  & 0    & -a_1 \\
            -a_2 & a_1  & 0 \\
     \end{bmatrix}
    \]

-}
skew::(Num a)=>Vertex3 a->[[a]]
skew (Vertex3 a1 a2 a3) = [
                            [0,     -a3, a2],
                            [a3,    0,  -a1],
                            [-a2,   a1,   0]
                          ]


{-|
    === Form a __skew symmetric matrix__ from \( \color{red}{Vector3}  \)
    __cross product__ of a x b = [a] b where [a] is skew symmetric matrix from vector a
    colIndex x rowIndex = [c][r]

    >Vertex3 a1 a2 a3

    \[
     \begin{bmatrix}
            0    & -a_3 & a_2 \\
            a_3  & 0    & -a_1 \\
            -a_2 & a_1  & 0 \\
     \end{bmatrix}
    \]
-}
skew'::(Num a)=>Vector3 a->[[a]]
skew' (Vector3 a1 a2 a3) = [
                            [0,     -a3, a2],
                            [a3,    0,  -a1],
                            [-a2,   a1,   0]
                           ]


{-|
  === Cross product of two vectors. unicode code 
  * The directin is determinated by the the Right Hadle Rule
-}
(⊗)::Vector3 GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
(⊗) v0@(Vector3 a1 a2 a3) v1@(Vector3 b1 b2 b3) = Vector3 x y z
                    where
                        vs = [[b1], [b2], [b3]]
                        -- form a skew matrix from v0
                        sm = skew' v0
                        -- cross product: v0 X v1
                        vc  = multiMat sm vs
                        vp = Vector3 (head . head)

                        -- vc = [[x], [y], [z]]
                        x  = (head . head) vc
                        y  = (head . head) $ tail vc
                        z  = (last . last) vc



{-|
    === Check whether four points are coplanar
-}
isCoplanar::Vertex3 GLfloat ->Vertex3 GLfloat ->Vertex3 GLfloat ->Vertex3 GLfloat -> Bool
isCoplanar p0 p1 q0 q1 = if nis0 && nis1 then norm (cross v01 v12) == 0.0 else False
                where
                    nis0 = not $ isColinear p0 q0 q1
                    nis1 = not $ isColinear p1 q0 q1
                    v0  = q0 -: p0 -- p0 -> q0
                    v1  = q1 -: p0 -- p0 -> q1
                    v2  = q0 -: p1 -- p1 -> q0
                    v3  = q1 -: p1 -- p1 -> q1
                    v01 = cross v0 v1 -- v0 X v1
                    v12 = cross v2 v3 -- v2 X v3
                    norm v = sqrt $ dot3ve v v

cross::Vector3 GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
cross u v = u ⊗ v


{-|
    === KEY: compute the normal of three points
-}            
normal3::Vertex3 GLfloat ->Vertex3 GLfloat ->Vertex3 GLfloat ->Maybe (Vector3 GLfloat)
normal3 p0 q0 q1 = if isc then Nothing else (Just nr)
                where
                    isc = isColinear p0 q0 q1
                    v0  = q0 -: p0
                    v1  = q1 -: p0
                    nr  = v0 ⊗ v1

{-|
    === KEY: compute the normal of plane with two given vectors
-}
normal3'::Vector3 GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
normal3' v0 v1 = v0 ⊗ v1

-- | (Vertex3 a) operator: (-) (+)
instance (Num a)=>Num(Vertex3 a) where
    (-) (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) = Vertex3 (x0 - x1) (y0 - y1) (z0 - z1)
    (+) (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) = Vertex3 (x0 + x1) (y0 + y1) (z0 + z1)
    negate (Vertex3 x0 y0 z0)                 = Vertex3 (-x0) (-y0) (-z0)
    abs (Vertex3 x0 y0 z0)                    = Vertex3 (abs x0) (abs y0) (abs z0)
    signum _                                  = undefined
    (*) _ _                                   = undefined
    fromInteger _                             = undefined

-- | (Vector3 a) operator: (-) (+)
instance (Num a)=>Num(Vector3 a) where
    (-) (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) = Vector3 (x0 - x1) (y0 - y1) (z0 - z1)
    (+) (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) = Vector3 (x0 + x1) (y0 + y1) (z0 + z1)
    negate (Vector3 x0 y0 z0)                 = Vector3 (-x0) (-y0) (-z0)
    abs    (Vector3 x0 y0 z0)                 = Vector3 (abs x0) (abs y0) (abs z0)
    signum _                                  = undefined
    (*) _ _                                   = undefined
    fromInteger _                             = undefined

-- | Vector: p0 -> p1 = p1 - p0
vec::Vertex3 GLfloat -> Vertex3 GLfloat -> Vector3 GLfloat
vec p0 p1 = p1 -: p0


{-|
    KEY: Given a point and vector

    * Draw a line passes a point p₀ alone the vector v₀

    r(t) = p₀ + t(p₁ - p₀)

    @
    let p0 = Vertex3 0.0 0.0 0.0
    let v0 = Vector3 0.4 0.5 0.0
    let p1 = ray p0 1.0 v0
    drawSegmentls green [p0, p1]
    @
-}
ray::Vertex3 GLfloat -> GLfloat -> Vector3 GLfloat -> Vertex3 GLfloat
ray p0 t v = p0 +: (t *: v)
  
{-|
    KEY: Given a point and vector

    * Draw a line passes a point p₀ alone the vector v₀

    r(t) = p₀ + t(p₁ - p₀)

    @
    let p0 = Vertex3 0.0 0.0 0.0
    let v0 = Vector3 0.4 0.5 0.0
    let p1 = ray2 1.0 p0 v0
    drawSegmentls green [p0, p1]
    @
-}  
ray2::GLfloat -> Vertex3 GLfloat -> Vector3 GLfloat -> Vertex3 GLfloat
ray2 t p0 v = p0 +: (t *: v)



{-|
    === KEY: perpendicular line to c₀ center of p₀ p₁

    Given s, t, p₀ and p₁
    1. c₀ = center p₀ p₁
    2. p₁' = p₀ + s(⊥ p0 p1), p₂' = p₀ + t(⊥ p₀ p₁)
    3. Draw line from p₁' to p₂'

    @
    let vv0 = Vertex3 1   (negate 1  ) 0.0
    let vv1 = Vertex3 1.8 (negate 1.5) 0.0
    let v = [vv0, vv1]
    drawSegmentWithEndPt red v
    let vls = perpenLine 0.9 (-0.2) vv0 vv1

    drawSegmentWithEndPt green vls
    @

-}
perpenLine::GLfloat -> GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> [Vertex3 GLfloat]
perpenLine s t p0 p1 = [c0 +: (s *: v), c0 +: (t *: v)]
           where
             v = perpcw $ vec p0 p1 -- p₀ → p₁
             c0= cen p0 p1

{-|
    === KEY: draw curve from given function
-}
curvePt::(GLfloat -> GLfloat)->(GLfloat, GLfloat)->[Vertex3 GLfloat]
curvePt f (a, b) = [Vertex3 x (f x) 0 | x <- let n = 100; d = (b - a)/n; in map(\x -> a + x*d) [0..n]]
                   
curvePtD::(GLdouble -> GLdouble)->(GLdouble, GLdouble)->[Vertex3 GLdouble]
curvePtD f (a, b) = [Vertex3 x (f x) 0 | x <- let n = 100; d = (b - a)/n; in map(\x -> a + x*d) [0..n]]
                   
curvePtV::(GLfloat -> GLfloat)->(GLfloat, GLfloat)-> VU.Vector (Vertex3 GLfloat)
-- curvePtV f (a, b) = VU.fromList [Vertex3 0.1 10.1 10.0]
curvePtV f (a, b) = VU.map(\x -> Vertex3 x (f x) 0 ) $ let n = 100; d = (b - a)/(rf n) in VU.map(\x -> a + x*d) $ VU.enumFromN 0 n


{-| 
    === Given a function \(f\), interval \( (a, b) \)

    Draw the curve on xy-plane from \(a\) to \(b\)

    >mapM_ (\n -> drawCurve (\x -> x^n) (-1.0, 1.0) green) [1..20]

    @
    let f = \x -> negate (x - 0.5)*(x + 0.5)
    drawCurve f (negate 1.0, 1.0) green
    @

    <http://localhost/image/opengl_drawcurve.png drawcurve>

   
-} 
drawCurve::(GLfloat -> GLfloat) -> (GLfloat, GLfloat) ->Color3 GLdouble  -> IO()
drawCurve f (a, b) c = renderPrimitive LineStrip $ mapM_(\vx -> do
                                            color c
                                            vertex $ vx) $ curvePt f (a, b)
  
drawCurveList::[Vertex3 GLfloat] ->Color3 GLdouble  -> IO()
drawCurveList cx c = renderPrimitive LineStrip $ mapM_(\vx -> do
                                            color c
                                            vertex $ vx) cx
  
drawCurveListWithEndPt::[Vertex3 GLfloat] ->Color3 GLdouble  -> IO()
drawCurveListWithEndPt cx c = do
                              renderPrimitive LineStrip $ mapM_(\vx -> do
                                                          color c
                                                          vertex $ vx) cx
                              drawCircleColor' red   0.01 $ head cx
                              drawCircleColor' green 0.015 $ last cx        
  
drawCurveD::(GLdouble -> GLdouble) -> (GLdouble, GLdouble) ->Color3 GLdouble  -> IO()
drawCurveD f (a, b) c = renderPrimitive LineStrip $ mapM_(\vx -> do
                                            color c
                                            vertex $ vx) $ curvePtD f (a, b)                       

drawCurveV::(GLfloat -> GLfloat) -> (GLfloat, GLfloat) ->Color3 GLdouble  -> IO()
drawCurveV f (a, b) c = renderPrimitive LineStrip $ VU.mapM_(\vx -> do
                                            color c
                                            vertex $ vx) $ curvePtV f (a, b)
  

{-| 
    === draw Surface for equation \( f(x, y) = x^2 + y^2 \) form

    Draw \( f (x, y) = x^2 + y^2 \)

    > drawSurface (\x y -> x^2 + y^2)
-} 
drawSurface::(GLfloat -> GLfloat -> GLfloat) -> IO()
drawSurface f = do
    mapM_ (\row -> drawSegmentFromTo red row ) $ grid2 f 
    mapM_ (\row -> drawSegmentFromTo blue row ) $ tran $ grid2 f 

{-| 
    === draw Surface for equation \( f(x, y) = x^2 + y^2 \) form

    Draw \( f (x, y) = x^2 + y^2 \)

    > r = 2 => 1/(r*n) 
    > drawSurfaceR (\x y -> x^2 + y^2) r 
-} 
drawSurfaceR::(GLfloat -> GLfloat -> GLfloat) -> GLfloat -> IO()
drawSurfaceR f r = do
    mapM_ (\row -> drawSegmentFromTo red row ) $ grid2Ratio f r
    mapM_ (\row -> drawSegmentFromTo red row ) $ tran $ grid2Ratio f r


    

--drawParamSurf::(GLfloat -> GLfloat) -> (GLfloat -> GLfloat) -> (GLfloat -> GLfloat) -> IO() 
--drawParamSurf fx fy fz = do 
--    mapM_ (\row -> drawSegmentFromTo red row ) $ pts 
--    mapM_ (\row -> drawSegmentFromTo red row ) $ tran pts 
--        where 
--            n  = 10 
--            fa = 1/(1.5*n)
--            t = map(\x -> rf $ fa * x) [-n..n]
--            pts = [[ Vertex3 (fx t) (fy t) (fz t)]]


{-| 
    === Plot all pts

    <http://localhost/image/opengl_plot_pt.png Plot_Points>

    @
    plot 2d graphic

    Divide each pair of point as  (x, y, 0)

    interval from [-len/2.. len/2]
    let pts = [0.3, 0.4, 0.1, 0.2] 
    plotPts red pts 
    plotPts green $ quickSort1 pts
    @
-} 
plotPts::Color3 GLdouble -> [GLfloat] ->IO()
plotPts co cx = do
                let n = div (length cx) 2
                let xx = let del = 1/(rf n) in map(\x -> del*(rf x)) [-n..n]
                let xx' = map(\x -> rf x) xx
                let vl = zipWith(\x y -> Vertex3 x y 0) xx' cx 
                let pair = join $ zipWith(\x y -> [x, y]) (init vl) (tail vl)
                mapM_ (\x -> drawCircleColor x red 0.01) vl 
                drawPrimitive' Lines co pair 

{-|
  Draw a line that is tangent at pt (c, f(c)) where x in [x0, x1]

  <http://localhost/image/tangleline.png tangentLine>

  1. Given a function f, and x0 on x-Axis

  2. Derive a tangent line at (c, f(c)) with slop = f'(c)

  3. Interpolate (x0, f(x0)) and (x1, f(x1))

-}
tangentLine::(Fractional a)=>(a->a)->(a, a)->a->[Vertex3 a]
tangentLine f (x0, x1) c = interpolate' 1 p0 p1
        where
            y0 = tangent f x0 c  -- y = slop(x - c) + (f x0)
            y1 = tangent f x1 c  -- y = slop(x - c) + (f x1)
            p0 = Vertex3 x0 y0 0
            p1 = Vertex3 x1 y1 0

interpolate'::(Fractional a)=>Integer -> Vertex3 a->Vertex3 a->[Vertex3 a]
interpolate' n p0@(Vertex3 x0 y0 z0) p1@(Vertex3 x1 y1 z1) = map(\k -> let t = d*(fromIntegral k) in p0 +: (t *: ve)) [0..n]
                    where
                        ve = p1 -: p0 -- vector: p0 -> p1
                        d = 1/(fromInteger n)

{-|
    === Given a function f, interval (a, b),
        draw a tangent line at (x0, f(x0)) from a to b
    >f::x -> y

    (a, b) is interval

    differentiate at (x0, f x0)
-}
drawTangentLine::(GLfloat->GLfloat)->(GLfloat, GLfloat) -> GLfloat -> Color3 GLdouble -> IO()
drawTangentLine f (a, b) x0 c = renderPrimitive LineStrip $ mapM_(\vx -> do
                                            color c
                                            vertex $ vx) $ pts
                    where
                        pts = tangentLine f (a, b) x0

{-|
 === NormalDir is either counter clockwise or clockwise for normal

 (x0, x1) is interval for the normal
 f is  any function
 x=c is the tangent point at (c, f c)

 >data NormalDir = NCCW | NCW deriving (Eq)
-}
normalLine::NormalDir -> (GLfloat->GLfloat)->(GLfloat, GLfloat)->GLfloat->[Vertex3 GLfloat]
normalLine n f (x0, x1) c = interpolate' 1 p0 p1'
        where
            y0 = tangent f x0 c
            y1 = tangent f x1 c
            p0 = Vertex3 x0 y0 0
            p1 = Vertex3 x1 y1 0
            ve = p1 -: p0  -- p0 -> p1
            nr = if n == NCW then perpccw ve else perpcw ve
            p1'= p0 +: nr

normalLineNew::NormalDir -> (GLfloat->GLfloat)->GLfloat->GLfloat->[Vertex3 GLfloat]
normalLineNew n f x0 c = interpolate' 1 p0 p1'
        where
            y0 = tangent f x0 c
            y1 = tangent f (x0 + len) c
            p0 = Vertex3 x0 y0 0
            p1 = Vertex3 (x0 + len) y1 0
            ve = p1 -: p0  -- p0 -> p1
            nr = if n == NCW then perpccw ve else perpcw ve
            p1'= p0 +: nr
            len = 0.1

drawNormalLine::(GLfloat->GLfloat)->(GLfloat, GLfloat) -> GLfloat -> Color3 GLdouble -> IO()
drawNormalLine f (a, b) x0 c = renderPrimitive LineStrip $ mapM_(\vx -> do
                                            color c
                                            vertex $ vx) $ pts
                    where
                        pts = normalLine NCCW f (a, b) x0

drawNormalLineNew::(GLfloat->GLfloat)->GLfloat -> GLfloat -> Color3 GLdouble -> IO()
drawNormalLineNew f a x0 c = renderPrimitive LineStrip $ mapM_(\vx -> do
                                            color c
                                            vertex $ vx) $ pts
                    where
                        pts = normalLineNew NCCW f a x0
                        b = 0.1

-- | compute the center of two vertices/vertex
cen::Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat
cen (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) = Vertex3 ((x0 + x1)/2) ((y0 + y1)/2) ((z0 + z1)/2)

-- | check two vectors whether they are perpendicular
isPerpen::Vector3 GLfloat -> Vector3 GLfloat -> Bool
isPerpen v1 v2 =  v1 ⊙ v2 == 0

{-|
    === KEY: Check whether three pts are colinear in 2D plan.
    __NOTE__

    * If two points or three points are overlapped, then they are still colinear
    * It's only for 2D plan, Vertex3 x y z ⇒ Vertex3 x y 0

    @
    f(t) = p0 + t(p1 - p0)
    f(s) = p0 + s(p2 - p0)
    f(t) = f(s) =>
    t(p1 - p0) = s(p2 - p0)
    t v1 = s v2, let A = [v1 v2]
    det A =? 0
    det A = 0 => p0 p1 p2 are colinear
    @
-}
isColinear::Vertex3 GLfloat ->Vertex3 GLfloat ->Vertex3 GLfloat -> Bool
isColinear (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = det == 0
                where
                    -- v01 = Vertex3 (x1 - x0) (y1 - y0) (z1 - z0)
                    -- v02 = Vertex3 (x2 - x0) (y2 - y0) (z2 - z0)
                    det = (x1 - x0)*(y2 - y0) - (x2 - x0)*(y1 - y0)

{-|
    === Check whether a given point is inside the segment

    __NOTE__ use 'ptOnSegment' instead

    * Assume three pts are different pts

    * Given p0, q0 q1, check whether p0 is inside the segment of q0 q1

    * __If__ they are colinear
    * __then__ check the distance \( \overline{p_0 q_0} \)  \( \overline{p_0 q_1} \) and \( \overline{q_0 q_1} \)

    Using following __Affine combination on points__ formula

    === Linear combination on vector \( \{ x_1, x_2, \dots \} \)
    \[
        \begin{aligned}
            \sum_{i=1}^{n} \alpha_{i} x_i
        \end{aligned}
    \]

    If the sum of all the coefficients is 1
    \[
        \begin{aligned}
            \sum_{i=1}^{n} \alpha_{i} &= 1
        \end{aligned}
    \]
    is called __Affine Combination__ of \( \{x_1, x_2, \dots \} \)

    __NOTE__
    If we extend above definition to highter power on \(t\),
    \(t\) can have any power \( n > 0 \) or \( t^{n} \)

    \( [(1 - t) + t]^{n} \) is just Binomial Expansion

    <http://localhost/html/indexBezierCurve.html Bezier_Curve>

    \[
        \begin{aligned}
            1 &= [(1 - t) + t]^{1} \\
            Q &= (1 - t)p_0 + t p_1 \\ \\
            1 &= [(1 - t) + t]^{2} = (1 - t)^{2} + 2t(1-t) + t^2 \\
            Q &= (1-t)^{2} p_0 + 2t(1-t) p_1 + t^2 p_2  \\
        \end{aligned}
    \]



    In out case, there are only two points \( p_0, p_1 \)
    so \( p \) is the __Affine Combination__ of \( p_0, p_1 \)
    \[
        \begin{aligned}
            p &= (1 - t) p_0 + t p_1 \\
        \end{aligned}
    \]
    If \( 0 < t < 1 \) then the point on the segment \( \overline{p_0 p_1} \) but not overlapping on \( p_0, p_1 \)

    If \( t = 0 \) or \( t = 1 \) then the point \(p\) is overlapping \( p_0 \) or \( p_1 \)

    If \( t < 0 \) or \( t > 1 \) then the point \(p\) is NOT on the segment \( \overline{p_0 p_1} \)

    >p0 = Vertex3 0.5 0.5 0
    >q0 = Vertex3 1 1 0
    >q1 = Vertex3 0 0 0
    >isInSegment p0 q0 q1
    >Just True
-}
isInSegment::Vertex3 GLfloat ->Vertex3 GLfloat ->Vertex3 GLfloat -> Maybe Bool
isInSegment p0 q0 q1 = if is then (if d1 > ds || d2 > ds then Just False else Just True) else Nothing
                where
                    is = isColinear p0 q0 q1 -- has to be colinear
                    ds = dist q0 q1
                    d1 = dist p0 q0
                    d2 = dist p0 q1

data PtSeg = OnEndPt
             | InSeg
             | OutSeg  deriving (Eq, Show)

{-|
    === Better version of 'isInSegment'

    (1) If \(p_0\) is overlapped with \(q_0, q_1\) then OnPt
    (2) If \(\overline{p_0 q_0} + \overline{p_0 q_1} > 0\) then OutSeg
    (3) Else InSeg

    @
    data PtSeg = OnEndPt -- ^ Overlapped pt
                 | InSeg   -- ^ Inside the segment
                 | OutSeg  -- ^ Out the segment
    @

    Maybe use better name:
    crossSegments - No endpt is overlapped

   >1. Three pts are colinear
   >    1. one endpts is overlapped
   >    2. No endpt is overlapped, one endpt is "inside" the segment
   >    3. Not intersected
   >2. Four pts are colinear
   >     1. No endpts is overlapped
   >         1. one segment "inside" the other segment
   >         2. one segment "outside" the other segment
   >     2. One endpts is overlapped
   >         1. one endpt is "outside" a segment
   >         2. one endpt is "inside" a segment
   >     3. two endpts are overlapped => same segment
   >3. No three pts are colinear
   >    1. If two segment is intersected, one must cross other segment
   >    2. Two segments DO NOT intersect

-}
ptOnSegment::Vertex3 GLfloat ->Vertex3 GLfloat ->Vertex3 GLfloat -> PtSeg
ptOnSegment p0 q0 q1 = if is then OnEndPt else (if d1 + d2 > ds then OutSeg else InSeg)
    where
        is = containPt p0 [q0, q1]
        ds = dist q0 q1
        d1 = dist p0 q0
        d2 = dist p0 q1

{-|
    === Draw primitives with a list of triple '(GLfloat, GLfloat, GLfloat)' such lines, points, lineloop etc

    <http://localhost/html/indexHaskellOpenGLPrimitiveMode.html PrimitiveMode>

    @
    PrimitiveMode
    Lines
    LineStrip
    LineLoop
    TriangleStrip
    TriangleFan
    Quad
    QuadStrip
    Polygon
    Patches
    @

    drawPrimitive Lines green [(0.1, 0.2, 0.0), (0.2, 0.4, 0.0)]
-}
drawPrimitive::PrimitiveMode -> Color3 GLdouble -> [(GLfloat, GLfloat, GLfloat)]->IO()
drawPrimitive m c list = do
    renderPrimitive m $ mapM_(\(x, y, z) -> do
        color c
        vertex $ Vertex3 x y z) list

{-|
    === KEY: Draw primitives with a list of 'Vertex3 GLfloat'

    <http://localhost/html/indexHaskellOpenGLPrimitiveMode.html PrimitiveMode>
    <<http://localhost/image/4Tile.svg 4Title>>

    @
    PrimitiveMode
    Lines
    LineStrip
    LineLoop
    TriangleStrip
    TriangleFan
    Quad
    QuadStrip
    Polygon
    Patches


    let p0 = Vertex3 0.2 0.3 0
    let v0 = Vertex3 0   (-0.6) 0
    let v1 = Vertex3 (-0.6) (-0.6) 0
    let v2 = Vertex3 (-0.5) (-0.1) 0
    let v3 = Vertex3 0.0 0.0 0
    drawPrimitiveVex Polygon green [v0, v1, v2, v3] 

    @

    drawPrimitiveVex Lines green [Vertex3 0.1, 0.2, 0.0, Vertex3 0.2, 0.4, 0.0]
-}    
drawPrimitiveVex::PrimitiveMode -> Color3 GLdouble -> [Vertex3 GLfloat]->IO()
drawPrimitiveVex m c list = do
    renderPrimitive m $ mapM_(\vx -> do
        color c
        vertex $ vx) list

{-|
   KEY: draw triangle from three vertex

   @
   drawTriangleList red [Vertex3 0 0 0, Vertex3 1 0 0, Vertex3 0 1 0]
   @
-}
drawTriangleList :: Color3 GLdouble -> [Vertex3 GLfloat] -> IO()
drawTriangleList c cx = drawPrimitiveVex LineLoop c cx
    
drawTriangleStrip:: Color3 GLdouble -> [Vertex3 GLfloat] -> IO ()
drawTriangleStrip c cx = drawPrimitiveVex TriangleStrip c cx

{-|
    KEY: Draw ONE triangle ONLY from '(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)'

    @
    PrimitiveMode
    Lines
    LineStrip
    LineLoop
    TriangleStrip
    TriangleFan
    Quad
    QuadStrip
    Polygon
    Patches

    let vv0 = Vertex3 0   0.6 0
    let vv1 = Vertex3 0.6 0.6 0
    let vv2 = Vertex3 0.5 0.1 0
    drawTriangleVex blue (vv0, vv1, vv2) 

    @
-}
drawTriangleVex:: Color3 GLdouble -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) -> IO ()
drawTriangleVex co (a, b, c) = drawPrimitiveVex TriangleStrip co [a, b, c]
                           
{-|
    KEY: Draw ONE triangle ONLY, same as 'drawTriangleVex' 

    * Shorter name, 'drawTriangleVex'

    @
    PrimitiveMode
    Lines
    LineStrip
    LineLoop
    TriangleStrip
    TriangleFan
    Quad
    QuadStrip
    Polygon
    Patches
    @
-}
triangle:: Color3 GLdouble -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) -> IO ()
triangle co (a, b, c) = drawTriangleVex co (a, b, c) 
{-| 
    === draw primitive such lines, points, lineloop etc

    <http://localhost/html/indexHaskellOpenGLPrimitiveMode.html PrimitiveMode>

    @
    Lines
    LineStrip
    LineLoop
    TriangleStrip
    TriangleFan
    Quad
    QuadStrip
    Polygon
    Patches
    @

    'AronOpenGL.randomVertex'

    >list <- randomVertex 60
    >drawPrimitive' Lines red list
-} 
drawPrimitive'::PrimitiveMode -> Color3 GLdouble -> [Vertex3 GLfloat]->IO()
drawPrimitive' m c list = do
    renderPrimitive m $ mapM_(\v3 -> do
        color c
        vertex $ v3) list
    
drawPrimitive2::PrimitiveMode -> Color3 GLdouble -> [Vertex3 GLdouble]->IO()
drawPrimitive2 m c list = do
    renderPrimitive m $ mapM_(\v3 -> do
        color c
        vertex $ v3) list
    
drawLines::Color3 GLdouble -> [Vertex3 GLfloat]->IO()
drawLines c list = do
    renderPrimitive Lines $ mapM_(\v3 -> do
        color c
        vertex v3) list
    mapM_ (\(x, y) -> if (mod x 2) == 1 then drawCircleColor' red 0.01 y else return () ) $ zip [1..] list
    mapM_ (\(x, y) -> if (mod x 2) == 0 then drawCircleColor' green 0.015 y else return () ) $ zip [1..] list


{-|
    === Draw Segment with Two End Pts

    @
    v = [Vertex3 0.1 0.1 0.2, Vertex3 0.4 0.2 0.7]
    drawSegmentWithEndPt red v
    
    v = [[Vertex3 0.1 0.1 0.2, Vertex3 0.4 0.2 0.7]]
    mapM_(\x -> drawSegmentWithEndPt red x) vv
    @
-}
drawSegmentWithEndPt::Color3 GLdouble -> [Vertex3 GLfloat]->IO()
drawSegmentWithEndPt c list = do
    renderPrimitive Lines $ mapM_(\v3 -> do
        color c
        vertex v3) list
    mapM_ (\(x, y) -> if (mod x 2) == 1 then drawCircleColor' red 0.01 y else return () ) $ zip [1..] list
    mapM_ (\(x, y) -> if (mod x 2) == 0 then drawCircleColor' green 0.015 y else return () ) $ zip [1..] list

drawSegmentNoEndPt::Color3 GLdouble -> [Vertex3 GLfloat]->IO()
drawSegmentNoEndPt c list = do
    renderPrimitive Lines $ mapM_(\v3 -> do
        color c
        vertex v3) list
    
drawSegmentNoEndPt2::Color3 GLdouble -> [Vertex3 GLdouble]->IO()
drawSegmentNoEndPt2 c list = do
    renderPrimitive Lines $ mapM_(\v3 -> do
        color c
        vertex v3) list
    
{-|
   === draw fat segment

   @
    when True $ do
     let p₀ = Vertex3 0.0 0.0 0.0 :: Vertex3 GLfloat
     let p₁ = Vertex3 0.5 0.5 0.0 :: Vertex3 GLfloat
     let α = 0.005
     drawFatSegment α [p₀, p₁]
   @
-}
drawFatSegmentEndPt::GLfloat -> [Vertex3 GLfloat] -> IO()
drawFatSegmentEndPt c cx = do 
  -- let a0 = Vertex3 0.0 0.0 0.0
  -- let b0 = Vertex3 0.5 0.5 0.0
  let p₀ = head cx
  let p₁ = last cx
  -- drawSegmentls green [p0, p1]
  drawSegmentls red [p₀, p₁]
  let vab = (-:) p₁ p₀  -- vector a0 -> b0
  let vab' = (*:) c (uv vab)  -- normalize a vector
  let perpccwV = perpccw vab'
  let perpcwV  = perpcw  vab'

  let ppccwPt_b0 = (+:) p₁ perpccwV
  let ppcwPt_b0  = (+:) p₁ perpcwV

  let ppccwPt_a0 = (+:) p₀ perpccwV
  let ppcwPt_a0  = (+:) p₀ perpcwV
  drawSegmentls blue  [p₁, ppccwPt_b0]
  drawSegmentls green [p₁, ppcwPt_b0]

  drawSegmentls blue  [p₀, ppccwPt_a0]
  drawSegmentls green [p₀, ppcwPt_a0]

  drawSegmentls blue  [ppccwPt_a0, ppccwPt_b0]
  drawSegmentls green [ppcwPt_a0,  ppcwPt_b0]

{-|
    === Draw a set segment from each pair of points(segment)

    Segment contains two end points, one is begin point, other is end point

    @
    [p0, p1, p2] = p0 -> p1    (p2 is ignored)
    [p0, p1, p2, p3]
    p0 -> p1
    p2 -> p3

    --NO loop
    drawSegments red  let v0 = Vertex3 0.1 (-0.1) 0
                          v1 = Vertex3 0.2 0.1    0
                          v2 = Vertex3 0.3 0.4    0
                          v3 = Vertex3 0.6 0.2    0
                          ls = [v0, v1, v2, v3]
                      in join $ zipWith (\x y -> [x, y]) (init ls) (tail ls)

    -- Loop
    drawSegments red  let v0 = Vertex3 0.1 (-0.1) 0
                          v1 = Vertex3 0.2 0.1    0
                          v2 = Vertex3 0.3 0.4    0
                          v3 = Vertex3 0.6 0.2    0
                          ls = [v0, v1, v2, v3, v0]
                      in join $ zipWith (\x y -> [x, y]) (init ls) (tail ls)
    @
-}
drawSegments::Color3 GLdouble -> [Vertex3 GLfloat] -> IO()
drawSegments c list = do
    renderPrimitive Lines $ mapM_(\v3 -> do
        color c
        vertex v3) list
    mapM_ (\(x, y) -> if (mod x 2) == 1 then drawCircleColor' red 0.01 y else return () ) $ zip [1..] list
    mapM_ (\(x, y) -> if (mod x 2) == 0 then drawCircleColor' green 0.015 y else return () ) $ zip [1..] list

  
{-|
    === Draw one segment from p0 to p1

    NOTE: deprecated
    * Bad name

    @
    let p0 = Vertex3 0.1 0.1 0
    let p1 = Vertex3 0.4 0.4 0
    drawSegmentls red [p0, p1]
    @

    SAME: 'drawSegmentList' 
-}   
drawSegmentls::Color3 GLdouble -> [Vertex3 GLfloat] -> IO()
drawSegmentls c list = do
    renderPrimitive Lines $ mapM_(\v3 -> do
        color c
        vertex v3) list
    mapM_ (\(x, y) -> if (mod x 2) == 1 then drawCircleColor' red 0.01 y else return () ) $ zip [1..] list
    mapM_ (\(x, y) -> if (mod x 2) == 0 then drawCircleColor' green 0.015 y else return () ) $ zip [1..] list

{-|
    === Draw one segment from p0 to p1

    @
    let p0 = Vertex3 0.1 0.1 0
    let p1 = Vertex3 0.4 0.4 0
    drawSegmentList red [p0, p1]
    @

    SAME: 'drawSegment' 
-}   
drawSegmentList::Color3 GLdouble -> [Vertex3 GLfloat] -> IO()
drawSegmentList c list = do
    renderPrimitive Lines $ mapM_(\v3 -> do
        color c
        vertex v3) list
    mapM_ (\(x, y) -> if (mod x 2) == 1 then drawCircleColor' red 0.01 y else return () ) $ zip [1..] list
    mapM_ (\(x, y) -> if (mod x 2) == 0 then drawCircleColor' green 0.015 y else return () ) $ zip [1..] list
  
{-|
    === Draw one segment from p0 to p1

    @
    let p0 = (Vertex3 0.1 0.1 0, Vertex3 0.4 0.4 0)
    let p1 = (Vertex3 0.4 0.4 0, Vertex3 0.4 0.6 0)
    drawSegment red (p0, p1)
    @
-}    
drawSegment::Color3 GLdouble -> (Vertex3 GLfloat, Vertex3 GLfloat)->IO()
drawSegment c (p0, p1) = do
    renderPrimitive Lines $ mapM_(\v3 -> do
        color c
        vertex v3) list
    mapM_ (\(x, y) -> if (mod x 2) == 1 then drawCircleColor' red 0.01 y else return () ) $ zip [1..] list
    mapM_ (\(x, y) -> if (mod x 2) == 0 then drawCircleColor' green 0.015 y else return () ) $ zip [1..] list
        where
            list = [p0, p1]

{-|
    === Draw one segment from p0 to p1

    @
    let p0 = (Vertex3 0.1 0.1 0, Vertex3 0.4 0.4 0)
    let p1 = (Vertex3 0.4 0.4 0, Vertex3 0.4 0.6 0)
    drawSegment red (p0, p1)
    @
-}    
drawSegmentD::Color3 GLdouble -> (Vertex3 GLdouble, Vertex3 GLdouble)->IO()
drawSegmentD c (Vertex3 x0 y0 z0, Vertex3 x1 y1 z1) = do
  drawSegment c (Vertex3 (rf x0) (rf y0) (rf z0), Vertex3 (rf x1) (rf y1) (rf z1))
    
{-|
    === Draw one segment from p0 to p1, simple version of 'drawSegmentD' and 'drawSegment'
 
    @
    let p0 = (0,   0, 0)
    let p1 = (0.2, 0, 0)
    drawSeg red (p0, p1)
    @
-} 
drawSeg::Color3 GLdouble -> (GLdouble, GLdouble, GLdouble) -> (GLdouble, GLdouble, GLdouble) -> IO()
drawSeg c (x0, y0, z0) (x1, y1, z1) = drawSegmentD c (Vertex3 x0 y0 z0, Vertex3 x1 y1 z1)
                                      
drawSegNoEnd::Color3 GLdouble -> (GLdouble, GLdouble, GLdouble) -> (GLdouble, GLdouble, GLdouble) -> IO()
drawSegNoEnd c (x0, y0, z0) (x1, y1, z1) = drawSegmentNoEnd2 c (Vertex3 x0 y0 z0, Vertex3 x1 y1 z1)
                                      
{-| 
    === Draw one segment with no endpt
-} 
drawSegmentNoEnd::Color3 GLdouble -> (Vertex3 GLfloat, Vertex3 GLfloat)->IO()
drawSegmentNoEnd c (p0, p1) = do
    renderPrimitive Lines $ mapM_(\v3 -> do
        color c
        vertex v3) list
        where
            list = [p0, p1]
    
{-| 
    === Draw one segment with no endpt
-} 
drawSegmentNoEnd2::Color3 GLdouble -> (Vertex3 GLdouble, Vertex3 GLdouble)->IO()
drawSegmentNoEnd2 c (p0, p1) = do
    renderPrimitive Lines $ mapM_(\v3 -> do
        color c
        vertex v3) list
        where
            list = [p0, p1]

    
-- | definition:
--   Segment contains two end points, one is begin point, other is end point
--   [p0, p1] = p0 -> p1
--
--data SegEndPt = No      -- no pt, just a segment
--                | End   -- end pt
--                | Beg   -- begin pt
--                | Both  -- begin and end pts
--                | Cen   -- center pt
--                | All   -- all pts: begin, end and ceneter
drawSegment'::SegEndPt -> Color3 GLdouble -> [Vertex3 GLfloat]->IO()
drawSegment' endpt c list = do
    renderPrimitive Lines $ mapM_(\v3 -> do
        color c
        vertex v3) list
    let bList = mapM_ (\(x, y) -> if (mod x 2) == 1 then drawCircleColor' red 0.01 y else return () ) $ zip [1..] list
        eList = mapM_ (\(x, y) -> if (mod x 2) == 0 then drawCircleColor' green 0.015 y else return () ) $ zip [1..] list
        ols = odds  list
        els = evens list
        cls = zipWith(\x y -> pure(/2) <*> (pure(+) <*> x <*> y)) ols els
        cList = mapM_ (drawCircleColor' blue 0.012) cls
        in case endpt of
            Beg -> bList -- begin pt
            End -> eList -- end pt
            Cen -> cList -- center pt
            Both -> do
                bList
                eList
            All -> do
                bList
                eList
                cList
            _   -> return ()

-- | Draw intersection pt from two segments
-- | Two segments need not have an intersection.
--
drawSegmentArg::Vertex3 GLfloat ->
                Vertex3 GLfloat ->
                Vertex3 GLfloat ->
                Vertex3 GLfloat -> IO()
drawSegmentArg p0 p1 q0 q1 = do
            drawLines green [p0, ps]
            if not $ s ∈ [0, 1]
            then if s > 1
                then drawLines blue  [p1, ps] -- (p0 --> p1) --> ps
                else drawLines green [p0, ps] -- ps <-- (p0 --> p1
            else return ()
            if t > 1 || t < 0
            then if t > 1
                then drawLines green [q1, qt] -- (q0 --> q1) --> qt
                else drawLines blue  [q0, qt] -- qt <-- (q0 --> q1)
            else return ()
            where
                tup = fromJust $ intersectLine  p0 p1 q0 q1
                vx  = fst tup
                s   = (head . head) $ snd tup
                t   = (last . last) $ snd tup
                ps  = p0 +: (s *: (p1 -: p0))
                qt  = q0 +: (t *: (q1 -: q0))

{-|
  === draw sphere center at c with radius r
  * β is in x-y plane, it rotates around z-Axis. ϕ is in x-z plane, it rotates around y-Axis
  * \( r*\cos(ϕ) \) is the radius of circle cut through x-y plane


  <http://localhost/image/sphere_coordinate.svg sphere_equation>

  \[
        \begin{equation}
        \begin{aligned}
            f( ϕ, β ) &= r \times \cos(ϕ) * \cos(β) + x₀ \\
            f( ϕ, β ) &= r \times \cos(ϕ) * \sin(β) + y₀ \\
            f( ϕ, β ) &= r \times \sin(ϕ) + z₀
        \end{aligned}
        \end{equation}
  \]

-}
drawSpherePt::Vertex3 GLfloat -> GLfloat -> [Vertex3 GLfloat]
drawSpherePt c@(Vertex3 x0 y0 z0) r = pp
        where
            fx ϕ β = r*cos(ϕ)*cos(β) + x0
            fy ϕ β = r*cos(ϕ)*sin(β) + y0
            fz ϕ β = r*sin(ϕ) + z0
            n = 50
            π = pi
            δ = 2*π/n
            ll= map(\x -> x*δ) [1..n]
            ls= map(\x -> x*δ) [1..n]
            pp=[ Vertex3 (fx ϕ β) (fy ϕ β) (fz ϕ β)  | ϕ <- let d = π/10 in map (*d) [0..10], β <- ls]


{-|
   === KEY: points set for sphere
-}
spherePts::[[Vertex3 GLfloat]]
spherePts = geneParamSurface fx fy fz 10
    where
        n = 40::Int
        δ = (2*pi)/(rf(n-1)) :: Float
        r = 0.4
        br = 0.2
        σ = 1/rf(n-1)

        fx::Int -> Int -> GLfloat
        fx i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in r*cos(α)*cos(β)
        fy::Int -> Int -> GLfloat
        fy i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                     n = 3
                 in r*cos(α)*sin(β)
        
        fz::Int -> Int -> GLfloat
        fz i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in r*sin(α)

drawSphere::IO()
drawSphere = do
    let n = 40::Int
        δ = (2*pi)/(rf(n-1)) :: Float
        r = 0.4
        br = 0.2
        σ = 1/rf(n-1)

        fx::Int -> Int -> GLfloat
        fx i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in r*cos(α)*cos(β)
        fy::Int -> Int -> GLfloat
        fy i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                     n = 3
                 in r*cos(α)*sin(β)
        
        fz::Int -> Int -> GLfloat
        fz i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in r*sin(α)
        -- in drawParamSurfaceN fx fy fz n
        in drawParamSurface fx fy fz

drawParaboloid::IO()
drawParaboloid = do
    let n = 40::Int
        δ = (2*pi)/(rf(n-1)) :: Float
        r = 0.4
        br = 0.2
        σ = 1/rf(n-1)

        fx::Int -> Int -> GLfloat
        fx i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in β*cos(α)
        fy::Int -> Int -> GLfloat
        fy i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                     n = 3
                 in β*sin(α)
        
        fz::Int -> Int -> GLfloat
        fz i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in β*β
        -- in drawParamSurfaceN fx fy fz n
        in drawParamSurface fx fy fz    

{-|
    === KEY: draw conic

     -http://localhost/image/opengl_coinc.png
-}
drawConic::IO()
drawConic = do
  preservingMatrix $ do
    let u = 0.2
    -- rotate (90)$ (Vector3 0 0 1 :: Vector3 GLdouble)
    -- translate (Vector3 u u 0 :: Vector3 GLdouble)
    let n = 40::Int
        δ = (2*pi)/(rf(n-1)) :: Float
        r = 0.04
        br = 0.02
        σ = 1/rf(n-1)
        s = 0.05 -- scale the radius

        fx::Int -> Int -> GLfloat
        fx i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = s*δ*j'
                 in β*cos(α)
        fz::Int -> Int -> GLfloat
        fz i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = s*δ*j'
                 in β*sin(α)
        
        fy::Int -> Int -> GLfloat
        fy i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = s*δ*j'
                 in β
        ss = [[Vertex3 (fx i j)
                       (fy i j)
                       (fz i j) | i <- [1..n]] | j <- [1..n]]    
        in do
            mapM_ (\row -> drawSegmentFromTo red row) ss
            mapM_ (\row -> drawSegmentFromTo blue row) $ tran ss
    
        -- in drawParamSurface fx fy fz

{-|
   IMAGE: http://xfido.com/image/opengl_grid.png
-}
drawGrid::IO()
drawGrid = do
    let n = 20::Int
        δ = (2*pi)/(rf(n-1)) :: Float
        r = 0.4
        br = 0.2
        -- σ = 1/rf(n-1)

        fx::Int -> Int -> GLfloat
        fx i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     -- β  = δ*j'
                 in α*0.1
        fy::Int -> Int -> GLfloat
        fy i j = let i' = rf i
                     j' = rf j
                     -- α  = δ*i'
                     β  = δ*j'
                 in β*0.1
        
        fz::Int -> Int -> GLfloat
        fz i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in 0.2
        -- in drawParamSurfaceN fx fy fz n
        in drawParamSurfaceN_new fx fy fz n

-- | draw plane with three points
-- | no check whether they are co-linear
--
drawPlane::Color3 GLdouble ->
           Vertex3 GLfloat ->
           Vertex3 GLfloat ->
           Vertex3 GLfloat -> IO()
drawPlane c q0 q1 q2 = do
  renderPrimitive Lines $ mapM_ (\x -> do
                                    color c
                                    vertex x) [q0, q1, q0, q2, q1, q2]
{-|
    === Generate a circle 

    > let radius = 0.1
    > let pts = circlePt (Vertex3 0 0 0) radius

    See 'circleN' $n$ segments
-}
circlePt::Vertex3 GLfloat -> Double -> [Vertex3 GLfloat]
circlePt (Vertex3 x0 y0 z0) r =[let alpha = (pi2*n)/num in Vertex3 ((rf r)*sin(alpha) + x0) ((rf r)*cos(alpha) + y0) (0 + z0) | n <- [1..num]]
        where
            num = 4 
            pi2 = 2*pi::Float

{-|
    \(\color{red}{Deprecated} \) Use 'circlePt'

    === Fri Feb 15 11:13:17 2019 

    === Draw xy-plane circle
-}
circle'::Vertex3 GLfloat -> Double -> [Vertex3 GLfloat]
circle' (Vertex3 x0 y0 z0) r =[let alpha = (pi2*n)/num in Vertex3 ((rf r)*sin(alpha) + x0) ((rf r)*cos(alpha) + y0) (0 + z0) | n <- [1..num]]
        where
            num = 4
            pi2 = 2*pi::Float
            
circle2::Vertex3 GLdouble -> Double -> [Vertex3 GLdouble]
circle2 (Vertex3 x0 y0 z0) r =[let alpha = (pi2*n)/num in Vertex3 ((rf r)*sin(alpha) + x0) ((rf r)*cos(alpha) + y0) (0 + z0) | n <- [1..num]]
        where
            num = 4
            pi2 = 2*pi::Double
            
            
{-|
    === Draw xy-plane circle with $n$ segment

    See 'circlePt' at xy-plane
-}
circleN::Vertex3 GLfloat -> Double -> Integer -> [Vertex3 GLfloat]
circleN (Vertex3 x0 y0 z0) r num =[let alpha = (pi2*(rf n))/(rf num) in Vertex3 ((rf r)*sin(alpha) + x0) ((rf r)*cos(alpha) + y0) (0 + z0) | n <- [1..num]]
        where
            pi2 = 2*pi::Float

{-|
    === Draw xy-plane circle with $n$ segment, draw arc, circle arc

    See 'circlePt' at xy-plane

    @
    let cen = Vertex3 0.1 0.1 0
    let radius = 1.0
    let nStep = 10
                     + -> start interval
                     ↓
    let interval = (pi/4, pi/2)
                           ↑
                           + -> end interval

    circleNArc cen radius nStep interval
    @

    * circleNArc center radius n_step arc=(0, pi/2)
    * Arc rotates counter-clockwise if r₁ > r₀
    * Arc rotates clockwise         if r₁ <= r₀

    (x - x0)^2 + (y - y0)^2 = r^2

    x - x0 = r * cos α
    y - y0 = r * sin α

    x = r * cos α + x0
    y = r * sin α + y0
-}
circleNArc::Vertex3 GLdouble -> Double -> Integer -> (GLdouble, GLdouble) -> [Vertex3 GLdouble]
circleNArc (Vertex3 x0 y0 z0) r num (r0, r1) =[let delta = (r1 - r0)/(rf num)
                                                   r' = rf r
                                               in Vertex3 (r' * cos (r0  + (rf n)*delta) + x0)  (r' * sin (r0 + (rf n)*delta) + y0)  z0 | n <- [0..num]]

{-|
    === Draw xz-plane circle
-}
circleXY::Vertex3 GLfloat -> Double -> [Vertex3 GLfloat]
circleXY (Vertex3 x0 y0 z0) r =[let alpha = (pi2*n)/num in Vertex3 ((rf r)*cos(alpha) + x0) ((rf r)*sin(alpha) + y0) (0 + z0) | n <- [1..num]]
        where
            num = 4
            pi2 = 2*pi::Float

{-|
    === Draw xz-plane circle
-}
circleXZ::Vertex3 GLfloat -> Double -> [Vertex3 GLfloat]
circleXZ (Vertex3 x0 y0 z0) r =[let alpha = (pi2*n)/num in Vertex3 ((rf r)*cos(alpha) + x0) (0 + y0) ((rf r)*sin(alpha) + z0) | n <- [1..num]]
        where
            num = 4
            pi2 = 2*pi::Float

{-|
    === Draw yz-plane circle
-}
circleYZ::Vertex3 GLfloat -> Double -> [Vertex3 GLfloat]
circleYZ (Vertex3 x0 y0 z0) r =[let alpha = (pi2*n)/num in Vertex3  (0 + x0) ((rf r)*cos(alpha) + y0) ((rf r)*sin(alpha) + z0) | n <- [1..num]]
        where
            num = 4
            pi2 = 2*pi::Float



{-|
    KEY: draw dot, small circle

    @
    echo dot is a circle
    r = 0.01
    c = red

    drawDot (Vertex3 0.0 0.0 0.0) -- p0
    drawDot (Vertex3 0.4 0.0 0.0) -- p1
    @
-}
drawDot::Vertex3 GLfloat -> IO()
drawDot cen = drawPrimitive' LineLoop c $ circle' cen r
        where
            r = 0.01
            c = red
{-|
    KEY: draw dot, small circle with radius r = 0.1 

    @
    echo dot is a circle
    r = 0.01
    c = red

    drawDot (Vertex3 0.0 0.0 0.0) -- p0
    drawDot (Vertex3 0.4 0.0 0.0) -- p1
    @
-}
drawDotR::Vertex3 GLfloat -> Double -> IO()  -- drawDotR v r
drawDotR cen r = drawPrimitive' LineLoop c $ circle' cen r
        where
            c = red
  
drawDotColor::Vertex3 GLfloat -> Color3 GLdouble -> IO()  -- drawDotR v r
drawDotColor cen c = drawPrimitive' LineLoop c $ circle' cen r
        where
            r = 0.01
  
drawDotRColor::Vertex3 GLfloat -> GLdouble -> Color3 GLdouble -> IO()  -- drawDotRColor v r c
drawDotRColor cen r c = drawPrimitive' LineLoop c $ circle' cen r


-- | Given an Vector3 x y z or Vertex3 x y z
-- | Convert Cartesian Coordinates to Polar Coordinates
-- | beta  x-y plane,
-- | alpha x-z plane
-- |
-- |             y  z
-- |             | /
-- |          ---|/--- x
-- |             /
--
cartToPolar::(Vector3 GLfloat) -> (GLfloat, GLfloat)
cartToPolar (Vector3 x y z) = (atan(y/x), asin(z/rxz))
        where
            rxy = sqrt $ dot3ve (Vector3 x y 0.0) (Vector3 x y 0.0)
            rxz = sqrt $ dot3ve (Vector3 x 0.0 z) (Vector3 x 0.0 z)
            -- | at  = if y < 0 && x < 0 then pi + atan(y/x) else atan(y/x)

{-|
    === right hand coordinates system in OpenGL, x-z plan, rotation matrix

    * rotate matrix on x-z plane

    @ 
    roty α =[[c          os α,          0.0,  sin α]
             ,[           0.0 ,           1.0,  0.0  ]
             ,[negate $ sin α, 0.0,  c   os α]
             ]                       lll
    @
-}
roty α =[[cos α,          0.0,  sin α]
        ,[ 0.0 ,          1.0,  0.0  ]
        ,[negate $ sin α, 0.0,  cos α]
         ]

{-|
    === right hand coordinates system in OpenGL, x-y plane, rotation matrix

    * rotate matrix on x-y plane

    @
    rotz ϕ = [ [cos ϕ,   negate $ sin ϕ, 0.0]
              ,[sin ϕ,            cos ϕ, 0.0]
              ,[0.0  ,              0.0, 1.0]
              ]
    @

-}
rotz ϕ = [ [cos ϕ,          negate $ sin ϕ, 0.0]
         ,[ sin ϕ,          cos ϕ, 0.0]
          ,[ 0.0 ,            0.0, 1.0]
          ]

coordTip:: Color3 GLdouble -> IO()
coordTip c = do
    renderPrimitive Lines $ mapM_(\(x, y, z) -> do
        color c
        vertex (Vertex3 x y z ::Vertex3 GLfloat)
        ) conic

coordTipX::Color3 GLdouble ->  GLdouble -> IO()
coordTipX c u = do
    preservingMatrix $ do
        translate (Vector3 u 0 0 :: Vector3 GLdouble)
        --rotate (90)$ (Vector3 0 0 1 :: Vector3 GLdouble)
        coordTip c
    preservingMatrix $ do
        translate (Vector3 (u/2.0) 0 0 :: Vector3 GLdouble)
        GL.scale (1/scaleFont :: GL.GLdouble) (1/scaleFont) 1

        -- Ref: https://hackage.haskell.org/package/gloss-rendering-1.13.1.1/docs/src/Graphics.Gloss.Internals.Rendering.Picture.html#renderPicture
        -- text looks weird when we have got blend on
        GL.blend $= GL.Disabled        
        preservingMatrix $ GLUT.renderString GLUT.Roman "Hello World"
        GL.blend $= GL.Enabled
    
coordTipY:: Color3 GLdouble ->  GLdouble -> IO()
coordTipY c u = do
    preservingMatrix $ do
        translate (Vector3 0 u 0 :: Vector3 GLdouble)
        rotate (90)$ (Vector3 0 0 1 :: Vector3 GLdouble)
        coordTip c
    preservingMatrix $ do
        translate (Vector3 0 (u/2.0) 0 :: Vector3 GLdouble)
        rotate (90)$ (Vector3 0 0 1 :: Vector3 GLdouble)
        GL.scale (1/scaleFont :: GL.GLdouble) (1/scaleFont) 1
    
        -- Ref: https://hackage.haskell.org/package/gloss-rendering-1.13.1.1/docs/src/Graphics.Gloss.Internals.Rendering.Picture.html#renderPicture
        -- text looks weird when we have got blend on
        -- GLUT.renderString GLUT.Roman "Y"
        GL.blend $= GL.Disabled        
        preservingMatrix $ GLUT.renderString GLUT.Roman "Y"
        GL.blend $= GL.Enabled

coordTipZ::Color3 GLdouble -> GLdouble -> IO()
coordTipZ c u = do
    preservingMatrix $ do
        translate (Vector3 0 0 u :: Vector3 GLdouble)
        rotate (-90)$ (Vector3 0 1 0 :: Vector3 GLdouble)
        coordTip c
    preservingMatrix $ do
        translate (Vector3 0 0 (u/2.0) :: Vector3 GLdouble)
        rotate (-90)$ (Vector3 0 1 0 :: Vector3 GLdouble)
        GL.scale (1/scaleFont :: GL.GLdouble) (1/scaleFont) 1

        -- Ref: https://hackage.haskell.org/package/gloss-rendering-1.13.1.1/docs/src/Graphics.Gloss.Internals.Rendering.Picture.html#renderPicture
        -- text looks weird when we have got blend on
        -- GLUT.renderString GLUT.Roman "Z"
        GL.blend $= GL.Disabled        
        preservingMatrix $ GLUT.renderString GLUT.Roman "Z"
        GL.blend $= GL.Enabled

show3dStr::String -> Color3 GLdouble ->  GLdouble -> IO()
show3dStr str c u = do
    -- preservingMatrix $ do
        -- translate (Vector3 u 0 0 :: Vector3 GLdouble)
        -- rotate (90)$ (Vector3 0 0 1 :: Vector3 GLdouble)
         -- coordTip c
    preservingMatrix $ do
        translate (Vector3 (-u) (-u) 0 :: Vector3 GLdouble)
        -- SEE: Why we need to scale it before redering
        -- https://www.reddit.com/r/haskell/comments/o8qkbv/looking_for_minimum_example_to_render_string_or
        -- NOT Sure where the "4000" come from
        GL.scale (1/4000 :: GL.GLdouble) (1/4000::GL.GLdouble) 1
        color c
        -- Ref: https://hackage.haskell.org/package/gloss-rendering-1.13.1.1/docs/src/Graphics.Gloss.Internals.Rendering.Picture.html#renderPicture
        -- text looks weird when we have got blend on
        GL.blend $= GL.Disabled        
        preservingMatrix $ GLUT.renderString GLUT.Roman str
        GL.blend $= GL.Enabled


{-|
    === Coordinate with tips

    positive dir is the tips dir
-}
renderCoordinates::IO()
renderCoordinates = do
     let u = 1
     coordTipX red   u
     coordTipY green u
     coordTipZ blue  u
     renderPrimitive Lines $ do
         -- x-Axis
         color  (Color3  u    0    0    :: Color3  GLdouble)
         vertex (Vertex3 (-u) 0    0    :: Vertex3 GLdouble)
         color  (Color3  u    0    0    :: Color3  GLdouble)
         vertex (Vertex3 u    0    0    :: Vertex3 GLdouble)
         --     y-Axis                                      
         color  (Color3  0    u    0    :: Color3  GLdouble)
         vertex (Vertex3 0    (-u) 0    :: Vertex3 GLdouble)
         color  (Color3  0    u    0    :: Color3  GLdouble)
         vertex (Vertex3 0    u    0    :: Vertex3 GLdouble)
         --     z-Axis                                      
         color  (Color3  0    0    u    :: Color3  GLdouble)
         vertex (Vertex3 0    0    (-u) :: Vertex3 GLdouble)
         color  (Color3  0    0    u    :: Color3  GLdouble)
         vertex (Vertex3 0    0    u    :: Vertex3 GLdouble)


{-|
    === KEY: Draw circle(r, c) which is ⊥ to ve
    === Given an radius r, center Vertex3 c, and Vector ve

    <http://localhost/image/img101.png Circle_perpendicular_a_Vector>

    @
      -- vector ve perpendicular to the circle(r, c)
      drawCircleVec 0.2 (Vertex3 0.0 0.0 0.0) (Vector3 0.2 0 0)
                     ↑           ↑                       ↑ 
                   radius r    center c             vector ve
      
    @
-}
drawCircleVec::GLfloat -> Vertex3 GLfloat -> Vector3 GLfloat -> IO()
drawCircleVec r c v = do
  mapM_ drawDot $ map (matVx) ls'
  drawLines blue [c, c +: v]
  where
    (ϕ, α) = cartToPolar v
    circ = [[[0.0], [r*(cos β)], [r*(sin β)]] | β <- let π = 3.1415; d = 2*π/100 in map(*d) [1..100]]
    -- | circ = [[[0.5], [0.5], [0.0]]]
    ls   = map(multiMat (rotz $ ϕ)) circ -- rotate x-y plane
    ls'  = map(multiMat (roty $ α)) ls   -- rotate x-z plane
    -- |  v'   = multiMat (rotz $ ϕ) (veMat v)
    -- |  v''  = multiMat (roty $ α) v'
    -- | matVx [[x], [y], [z]] = Vertex3 x y z
    -- | veMat (Vector3 x y z) = [[x], [y], [z]]
    -- | ls   = map(multiMat (rotz $ 0.0)) circ
    ls1  = map(multiMat (rotz $ pi/4)) circ
    ls2  = map(multiMat (rotz $ 2*pi/4)) circ
    ls3  = map(multiMat (rotz $ 3*pi/4)) circ
    ls4  = map(multiMat (rotz $ 4*pi/4)) circ
    ls5  = map(multiMat (rotz $ 5*pi/4)) circ
    ls6  = map(multiMat (rotz $ 6*pi/4)) circ
    ls7  = map(multiMat (rotz $ 7*pi/4)) circ
    ls8  = map(multiMat (rotz $ 8*pi/4)) circ

{-|
    === matrix to Vertex

    @
    matVx [[x], [y], [z]] = Vertex3 x y z
    @
-}
matVx [[x], [y], [z]] = Vertex3 x y z


{-|
    === matrix to Vertex

    @
    matVe [[x], [y], [z]] = Vector3 x y z
    @
-}
matVe [[x], [y], [z]] = Vector3 x y z


{-|
    === Vector to matrix

    @
    veMat (Vector3 x y z) = [[x], [y], [z]]
    @
-}
veMat (Vector3 x y z) = [[x], [y], [z]]

        

{-|
    === KEY: rectangle with leftTop and bottomRight
    @
    let     x0 = -0.5::GLfloat
            y0 = -0.5::GLfloat                                                     
            z0 = 0.0::GLfloat                                                      
            x1 = 0.5::GLfloat                                                      
            y1 = 0.5::GLfloat                                                      
            z1 = 0.0::GLfloat  in drawRect ((Vertex3 x0 y0 z0), (Vertex3 x1 y1 z1))

              Y
         p₀   ↑
         ↓    |    th
         +----|----+
         |    |    |  rv
         |    |    |
    lv   |    /----- → x
         |   /     |
         +--/------+ ← p₁  
           z        

             bh   
    @

    http://localhost/image/opengl_draw rect.png
-}                             
drawRect::(Vertex3 GLfloat, Vertex3 GLfloat) -> IO()
drawRect (p0@(Vertex3 x0 y0 z0), p1@(Vertex3 x1 y1 z1)) = do
        drawSegmentNoEnd red (Vertex3 x0 y0 z0, Vertex3 x1 y0 z0)  --  top horizontal 
        drawSegmentNoEnd red (Vertex3 x0 y0 z0, Vertex3 x0 y1 z0)  --  left vertical  
        drawSegmentNoEnd red (Vertex3 x1 y0 z0, Vertex3 x1 y1 z1)  --  right vertical
        drawSegmentNoEnd red (Vertex3 x0 y1 z0, Vertex3 x1 y1 z1)  --  bottom horizontal
        
{-|
    === KEY: draw rectangle with color (Color3 GLdouble)
-}
drawRectColor::Color3 GLdouble -> (Vertex3 GLfloat, Vertex3 GLfloat) -> IO()
drawRectColor color (p0@(Vertex3 x0 y0 z0), p1@(Vertex3 x1 y1 z1)) = do
        drawSegmentNoEnd color (Vertex3 x0 y0 z0, Vertex3 x1 y0 z0)  --  top horizontal 
        drawSegmentNoEnd color (Vertex3 x0 y0 z0, Vertex3 x0 y1 z0)  --  left vertical  
        drawSegmentNoEnd color (Vertex3 x1 y0 z0, Vertex3 x1 y1 z1)  --  right vertical
        drawSegmentNoEnd color (Vertex3 x0 y1 z0, Vertex3 x1 y1 z1)  --  bottom horizontal

{-|
    === KEY: draw rectangle with color (Color3 GLdouble)
-}
drawRectColor2::Color3 GLdouble -> (Vertex3 GLdouble, Vertex3 GLdouble) -> IO()
drawRectColor2 color (p0@(Vertex3 x0 y0 z0), p1@(Vertex3 x1 y1 z1)) = do
        drawSegmentNoEnd2 color (Vertex3 x0 y0 z0, Vertex3 x1 y0 z0)  --  top horizontal 
        drawSegmentNoEnd2 color (Vertex3 x0 y0 z0, Vertex3 x0 y1 z0)  --  left vertical  
        drawSegmentNoEnd2 color (Vertex3 x1 y0 z0, Vertex3 x1 y1 z1)  --  right vertical
        drawSegmentNoEnd2 color (Vertex3 x0 y1 z0, Vertex3 x1 y1 z1)  --  bottom horizontal
        
        
{-|
    === Draw Rectangle with Width and Height

    @
           w
       ⌜--------⌝
       |        |  
       |        | 
       |   +    | h
       |        |
       |        |
       ⌞--------⌟
    @
-}
drawRect2d::(GLfloat, GLfloat) -> IO()
drawRect2d (w, h) = do
  drawRect (p0, p1)
  where
    x0 = w/2
    y0 = h/2
    p0 = Vertex3 (-x0) y0    0
    p1 = Vertex3 x0    (-y0) 0

{-|
    === Draw on xy-plane quads

    @
      drawQuads [Vertex3 0.1 0.1 0.0, Vertex3 0.2 0.1 0.0, Vertex3 0.2 0.2 0.0, Vertex3 0.1 0.2 0.0]    
    @
-}
drawQuads::[Vertex3 GLfloat] -> IO()
drawQuads cx = drawPrimitive' Quads red $ cx

{-|
    === KEY: draw quads

    @
    drawQuadsColor red [Vertex3 0.0 0.0 0.0, Vertex3 0.2 0.0 0.0, Vertex3 0.0 0.2 0.0, Vertex3 0.0 0.0 0.2]
    @

    http://localhost/image/opengl_drawquads.png
-}
drawQuadsColor:: Color3 GLdouble -> [Vertex3 GLfloat] -> IO()
drawQuadsColor c cx = drawPrimitive' Quads c $ cx 

{-|
    === KEY: xz-plane draw quads
-}
drawQuadsXZColor:: Color3 GLdouble -> [Vertex3 GLfloat] -> IO()
drawQuadsXZColor c cx = do
  preservingMatrix $ do
    rotate (90)$ (Vector3 1 0 0 :: Vector3 GLdouble)
    drawPrimitive' Quads c $ cx 

{-|
    === KEY: yz-plane draw quads
-}
drawQuadsYZColor:: Color3 GLdouble -> [Vertex3 GLfloat] -> IO()
drawQuadsYZColor c cx = do
  preservingMatrix $ do
    rotate (90)$ (Vector3 0 1 0 :: Vector3 GLdouble)
    drawPrimitive' Quads c $ cx 



{-|
    === KEY: fill rectangle

    @
             ↑
             |
        v0   ⟶   v1
                   
        ↑           |
        |    +      ↓  -> y

       v3    <—-    v2
    @
-}
drawRectFill2d::Color3 GLdouble -> (GLfloat, GLfloat) -> IO()
drawRectFill2d c (w, h) = do
  drawQuadsColor c [vx0, vx1, vx2, vx3]
  where
    x0 = w/2
    y0 = h/2
    vx0 = Vertex3 (-x0) y0    0
    vx1 = Vertex3 x0    y0    0
    vx2 = Vertex3 x0    (-y0) 0
    vx3 = Vertex3 (-x0) (-y0) 0


{-|
    === draw circle with center and radius

    >drawCircle cen r = drawPrimitive LineLoop red $ circle cen r
-}
drawCircle'::Vertex3 GLfloat -> Double -> IO()
drawCircle' cen r = drawPrimitive' LineLoop red $ circle' cen r

                    
drawCircle2::Vertex3 GLfloat -> Double -> IO()
drawCircle2 cen r = drawPrimitive' LineLoop red $ circleN cen r 30

{-|
    KEY: draw circle with three points

    @
    let q0 = Vertex3 0 0 0
    let q1 = Vertex3 0.8 0.8 0
    let q2 = Vertex3 1.5 0.5 0
    let pt = threePtCircle q0 q1 q2
    print pt
    let c0 = case pt of
                  Just x  -> x
                  Nothing -> (Vertex3 0 0 0)
    
    -- drawCircle2 c0 $ rf $ distX c0 q1
  
    drawCircleThreePt q0 q1 q2 green

    drawSegmentWithEndPt green [q0, q1]
    drawSegmentWithEndPt blue  [q1, q2]
    @
-}
drawCircleThreePt::Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Color3 GLdouble -> IO()
drawCircleThreePt p0 p1 p2 co = case c0 of
                                 Just c  -> drawCircleColorN c (rf $ distX c p1) co 30
                                 Nothing -> return ()
    where
      c0 = threePtCircle p0 p1 p2

{-|
  SEE: 'drawCircleThreePtListX'
-}
drawCircleThreePtList :: [Vertex3 GLfloat] -> Color3 GLdouble -> IO()
drawCircleThreePtList cx c = if len cx == 3 then drawCircleThreePt p0 p1 p2 c else error "List has to contain three Vertex3 GLfloat."
  where
    p0 = cx !! 0
    p1 = cx !! 1
    p2 = cx !! 2
  
{-|
  SEE: 'drawCircleThreePtList'
-}
drawCircleThreePtListX :: Color3 GLdouble -> [Vertex3 GLfloat] -> IO()
drawCircleThreePtListX c cx = if len cx == 3 then drawCircleThreePt p0 p1 p2 c else error "List has to contain three Vertex3 GLfloat."
  where
    p0 = cx !! 0
    p1 = cx !! 1
    p2 = cx !! 2
  
  
{-|
    KEY: three points fixes a circle

    return: center of a circle
-}
threePtCircle::Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Maybe (Vertex3 GLfloat)
threePtCircle p0 p1 p2 = case ret of
                              Just x  -> Just (fst x)
                              Nothing -> Nothing
    where
    c0 = center p0 p1
    pv = perpcw (p1 -: p0) -- perpendicular to p0 -> p1, counter-clockwise
    x0 = c0 +: pv
    -- line c0 x0
    c1 = center p1 p2
    pu = perpcw (p2 -: p1) -- perpendicular to p1 -> p2, counter-clockwise
    x1 = c1 +: pu
    -- line c1 x1
    ret = intersectLine c0 x0 c1 x1

threePtCircleList :: [Vertex3 GLfloat] -> Maybe (Vertex3 GLfloat)
threePtCircleList cx = if len cx == 3 then threePtCircle p0 p1 p2 else error "Three pts only in the list"
  where
    p0 = cx !! 0
    p1 = cx !! 1
    p2 = cx !! 2
  
{-|
    KEY: Center of two Vertex3 GLfloat
-}
center:: Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat
center (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) = Vertex3 ((x0 + x1) / 2) ((y0 + y1) / 2) ((z0 + z1) / 2)
  
{-|
    === draw circle with center and radius, n steps

    @
    let cen = Vertex3 0.1 0.0 0.0
    let r = 0.5
    let n = 10
    drawCircleColorN cen r n
    @
-}  
drawCircleColorN::Vertex3 GLfloat -> Double -> Color3 GLdouble -> Integer -> IO()
drawCircleColorN cen r co n = drawPrimitive' LineLoop co $ circleN cen r n
                    
  
drawCircleXYZ::Vertex3 GLfloat -> Double -> IO()
drawCircleXYZ cen r = do
                drawPrimitive' LineLoop red $ circleXY cen r
                drawPrimitive' LineLoop red $ circleXZ cen r
                drawPrimitive' LineLoop red $ circleYZ cen r

drawCircleXYZColor::Vertex3 GLfloat -> Double -> Color3 GLdouble -> IO()
drawCircleXYZColor cen r c = do
                drawPrimitive' LineLoop c $ circleXY cen r
--                drawPrimitive' LineLoop c $ circleXZ cen r
--                drawPrimitive' LineLoop c $ circleYZ cen r


{-|
    === draw circle with center , Color3, radius

    >drawCircleColor (Vertex3 0.1 0.2 0.3) red 0.5
-}
drawCircleColor::Vertex3 GLfloat -> Color3 GLdouble -> Double -> IO()
drawCircleColor cen c r = drawPrimitive' LineLoop c $ circle' cen r
                          
drawCircleColor2::Vertex3 GLdouble -> Color3 GLdouble -> Double -> IO()
drawCircleColor2 cen c r = drawPrimitive2 LineLoop c $ circle2 cen r
                          
{-|
    === Similar to drawCircleColor, but it can do more

    * draw two circles with different centers

    >mapM_ (drawCircleColor' red 0.5) [Vertex3 0.1 0.2 0.3, Vertex3 0.2 0.3 04]
-}
drawCircleColor'::Color3 GLdouble ->Double -> Vertex3 GLfloat -> IO()
drawCircleColor' c r cen = drawPrimitive' LineLoop c $ circle' cen r

{-|
    === Conic Parameter Equation
    gx <http://localhost/html/indexThebeautyofTorus.html Conic>
-}
conic::[(GLfloat, GLfloat, GLfloat)]
conic= [ let r' = r - rd*i in (d'*i, r'*sin(δ*k), r'*cos(δ*k)) | i <- [0..m], k <-[1..n]]
        where
            n = 20
            m = 10
            h = 0.1
            r = 0.05
            δ = 2*pi/n
            d' = h/m
            rd = r/m



-- | determinant of two dimension matrix
det2::(Num a)=>[[a]] -> a
det2 [[a, b], [c, d]] = a*d - b*c


--- | Sat Dec 22 20:59:28 2018
--- |
--- | find the inverse of matrix in Rational number
--- | it is not much difference from the Integer code
--- | change division from (div n m) => (n / m)
--- |
inverseR::[[Rational]]->[[Rational]]
inverseR m = if diag == 0 then [[]] else mb'
        where
            id = ident' $ length m
            -- argumented matrix [m] ++ [id]
            argm = zipWith(\x y -> x ++ y) m id
            -- argm =
            -- [[1 2 3 1 0 0]]
            -- [[4 5 6 0 1 0]]
            -- [[7 8 9 0 0 1]]
            mt = upperTri' argm
            -- mt =
            -- [[1, 2, 3 x x x]]
            -- [[   2, 2 x x x]]
            -- [[      1 x x x]]
            --
            -- If diag[onal] == 0 then it is single matrix
            diag = foldl(*) 1 [head x | x <- mt]
            ar = zipWith(\x y -> (replicate x 0) ++ y) [0..] mt
            -- ar =
            -- [[1 2 3 x x x]
            --  [0 2 2 x x x]
            --  [0 0 1 x x x]]
            pm = map(\x -> partList (length ar) x ) ar
            -- pm =
            -- [[[1 2 3] [x x x]]
            --  [[0 1 2] [x x x]]
            --  [[0 0 1] [x x x]]]
            m1 = map(\r -> head r) pm
            m2 = map(\r -> last r) pm
            -- m1 =
            -- [[1 2 3]
            --  [0 1 2]
            --  [0 0 1]]
            -- m2 =
            -- [[x x x]
            --  [x x x]
            --  [x x x]]
            m11= reverse $ map(\x -> reverse x) m1
            -- m11 =
            -- [[3 2 1]
            --  [2 1 0]
            --  [1 0 0]]
            -- [[1 0 0]
            --  [2 1 0]
            --  [3 2 1]]
            m22= reverse $ map(\x -> reverse x) m2
            -- m22 =
            -- [[x x x]
            --  [x x x]
            --  [x x x]]

            m3 = zipWith(\x y -> x ++ y) m11 m22
            m4 = upperTri' m3
            --m4'= map(\r -> map(\x -> divI x   $ toInteger (head r))
            -- Fri Dec 14 16:04:32 2018
            -- remove the division here
            m4'= map(\r -> map(\x -> x / (head r)) r) m4
            -- Not full inverse matrix here
            mm'= zipWith(\x y -> (replicate x 0) ++ y) [0..] m4'
            mm = map(\x -> partList (length mm') x) mm'
            m1'= map(\x -> head x) mm
            m2'= map(\x -> last x) mm
            ma'= map(\x -> reverse x) $ reverse m1'
            mb'= map(\x -> reverse x) $ reverse m2'

{-|
  === Inverse of two dimension matrix
  <http://localhost/pdf/inverse_matrix2.pdf Inverse_matrix_determinant>

    \[
        \begin{equation}
        \begin{aligned}
        A &= \begin{bmatrix}
            a & b \\
            c & d \\
            \end{bmatrix} \\
        A^{ -1} &= \frac{1}{ \det A }
                \begin{bmatrix}
                 d & -b \\
                 -c & a \\
                 \end{bmatrix}
        \end{aligned}
        \end{equation}
    \]
    * inverse should be used in general.

    * Or 'isInver' should be used for large matrix because 'isInver' is implemented
      in <http://localhost/pdf/gram_schmidt.pdf QR_Decompoisition>

    @
    isInver::(Fractional a, Ord a)=> [[a]] -> Bool
    isInver m = if len (filter(< 0.0001) cx) > 0 then False else True
    @

    * Following function is implemented in Gaussian Elimination

    * There is some Integer overflow issue, it only works for small matrix, e.g. 10 by 10

    @
    isInvertible::[[Integer]]->Bool
    @
-}
inv2::(Fractional a)=>[[a]] -> [[a]]
inv2 [[a, b], [c, d]] = (1/det2([[a, b], [c, d]])) *: [[d, (-b)], [(-c), a]]
    where
        -- scalar multiply 2x2 matrix
        (*:)::(Num a)=>a -> [[a]] -> [[a]]
        (*:) a cx = (map . map) (*a) cx
        dt = det2([[a, b], [c, d]])

data SegColinear = Colinear3 -- ^ If three pts are colinear => Colinear3
                   | Colinear4 -- ^ If four pts are colinear => Colinear4
                   | None deriving (Eq, Show)
{-|
    === If four points are colinear then return 'Colinear4'
    === If only three points are colinear then return 'Colinear3'
    === Else return 'None'

    >data SegColinear = Colinear3 -- ^ If three pts are colinear => Colinear3
    >                   | Colinear4 -- ^ If four pts are colinear => Colinear4
    >                   | None deriving (Eq, Show) -- ^ else => None

    The function uses 'isColinear'
-}
fourPtColinear::(Vertex3 GLfloat, Vertex3 GLfloat) -> (Vertex3 GLfloat, Vertex3 GLfloat) -> SegColinear
fourPtColinear  (p0, p1) (q0, q1) = if (is0 && is1) then Colinear4 else
                                            if (is0 || is1) || (is0' || is1') then Colinear3 else None
           where
                is0 = isColinear p0 q0 q1
                is1 = isColinear p1 q0 q1
                is0'= isColinear q0 p0 p1
                is1'= isColinear q1 p0 p1


{-|
    === Find the intersection of two lines, also see 'intersectSeg'
    ==== Assume that two endpoints of each segment are not overlapped.(segment has non-zero length)

    __NOTE__ This function is ONLY for two dimensions

    __Line__ extends infinitly in both direction


    * If both line intersect at one pt => return the pt
        1. Intersection can be on a segments or NOT on a segment 
    * If four points are colinear: return Nothing, see 'fourPtColinear'

    @
    let p0 = Vertex3 0 0 0
    let p1 = Vertex3 1 0 0
    let q0 = Vertex3 1 0 0
    let q1 = Vertex3 2 0 0
    intersectLine p0 p1 q0 q1
    Nothing

    let xp0 = Vertex3 0 0    0
    let xp1 = Vertex3 1 1    0
    let xq0 = Vertex3 1 0    0
    let xq1 = Vertex3 2 (-1) 0
    let xret = intersectLine xp0 xp1 xq0 xq1
    Just (Vertex3 0.5 0.5 0) , [[0.5], [-0.5]]
    @

    * Two dimensions determinant is used here

    * The Intersection of two line is __NOT__ necessary in their segments
    * If two lines are __parallel__ or __overlapped__ then return Nothing
    * Else return the intersection and \( s, t \)

    \[
        \text{Given four pts: \(p_0, p_1\) and \(q_0, q_1\)} \\
        \begin{aligned}
        v_0 &= p_1 - p_0 \quad v_0 \text{ is a vector} \\
        v_1 &= q_1 - q_0 \quad v_1 \text{ is a vector} \\
        A &= \begin{bmatrix}
             v_0 & v_1
             \end{bmatrix} \\
        \det A &= 0  \quad \text{they are linearly dependent }
        \end{aligned}
    \]

    <http://localhost/pdf/check_line_intersection.pdf Check Line Intersection>
    TODO: Fix ERROR on PDF: <http://localhost/pdf/intersectionLine.pdf intersect_line_PDF>

    @
    data Seg a = Seg a a
    fun::Seg GLflat -> Seg GLfloat -> Maybe(Vertex3 GLfloat, [[GLfloat]])
    fun (Seg (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1)) = Nothing
    @
    @
    intersectLine p0 p1 q0 q1
    (Just ((Vertex3 2.0 2.0 0), [[2.0],[2.0]]))= intersectLine
                                                (Vertex3 0 0 0)
                                                (Vertex3 1 1 0)
                                                (Vertex3 2 0 0)
                                                (Vertex3 2 1 0)
    @
-}
intersectLine::Vertex3 GLfloat ->
               Vertex3 GLfloat ->
               Vertex3 GLfloat ->
               Vertex3 GLfloat ->
               Maybe (Vertex3 GLfloat, [[GLfloat]])
intersectLine p0@(Vertex3 x0 y0 z0)
              p1@(Vertex3 x1 y1 z1)
              q0@(Vertex3 a0 b0 c0)
              q1@(Vertex3 a1 b1 c1)
              = if d == 0 || c4 == Colinear4 then Nothing else (Just (pt, st))
              where
--                is0 = isColinear p0 q0 q1
--                is1 = isColinear p1 q0 q1
--
                -- if c4 == Colinear4 then four pts are colinear
                c4 = fourPtColinear (p0, p1) (q0, q1)
                w1   = v2a $ p1 - p0  -- [[1, 2]]
                w2   = v2a $ q1 - q0  -- [[4, 5]]
                -- if d == 0 then two lines are parallel
                d    = det2 $ w1 ++ w2 -- det2 [[1, 2], [4, 5]]
                v01 = p1 -: p0 -- p₁ - p₀  f(t) = p₀ + s(p₁ - p₀)
                u01 = q1 -: q0 -- q₁ - q₀  f(s) = q₀ + t(q₁ - q₀)
                ma = [[ne (x1 - x0), (a1 - a0)],  -- [s]
                      [ne (y1 - y0), (b1 - b0)]]  -- [t]
                ivm= inv2 ma
                v  = [[x0 - a0],   -- p₀ - q₀
                      [y0 - b0]]
                -- solve s and t
                -- [s]
                -- [t]
                st = multiMat ivm v
                -- |  st = ivm *. v
                s  = (head . head) st -- st = [[s],[t]]
                -- t  = (last . last) st
                pt = p0 +: (s *: v01)
                ne = negate
                (*.) [[a, b], [c, d]] [[x], [y]]  = [[a*x + b*y], [c*x + d*y]]

                v2a (Vertex3 x y z) = [[x, y]]

{-| 
    === If two line parallel or four pts colinear => return Nothing
    === Else there is intersection pt, pt maybe on on segment or NOT on segment

    Sun Feb 17 15:32:59 2019 
    This function should replace 'intersectLine'
-} 
intersectLine2::Vertex3 GLfloat ->
                Vertex3 GLfloat ->
                Vertex3 GLfloat ->
                Vertex3 GLfloat ->
                Maybe (Vertex3 GLfloat, (GLfloat, GLfloat))
intersectLine2 p0@(Vertex3 x0 y0 z0)
               p1@(Vertex3 x1 y1 z1)
               q0@(Vertex3 a0 b0 c0)
               q1@(Vertex3 a1 b1 c1) 
                 | d == 0 || c4 == Colinear4  = Nothing 
                 | overLappedMaybe /= Nothing = overLappedMaybe
                 | otherwise                  = Just (pt, (s, t)) 
--                 | otherwise                  = (Just (pt, st)) 
                    where
                        -- if c4 == Colinear4 then four pts are colinear
                        c4 = fourPtColinear (p0, p1) (q0, q1)
                        w1   = v2a $ p1 - p0  -- [[1, 2]]
                        w2   = v2a $ q1 - q0  -- [[4, 5]]
                        -- if d == 0 then two lines are parallel
                        d    = det2 $ w1 ++ w2 -- det2 [[1, 2], [4, 5]]
                        v01 = p1 -: p0 -- p₁ - p₀  f(s) = p₀ + s(p₁ - p₀)
                        u01 = q1 -: q0 -- q₁ - q₀  f(t) = q₀ + t(q₁ - q₀)
                        ma = [[ne (x1 - x0), (a1 - a0)],  -- [s]
                              [ne (y1 - y0), (b1 - b0)]]  -- [t]
                        ivm= inv2 ma
                        v  = [[x0 - a0],   -- p₀ - q₀
                              [y0 - b0]]

                        -- If two segments are overlapped ONE endPt
                        overLappedMaybe = onePtOverlappedSeg (p0, p1) (q0, q1)
                        -- solve s and t
                        -- [s]
                        -- [t]
                        st = multiMat ivm v
                        -- |  st = ivm *. v
                        s  = (head . head) st -- st = [[s],[t]]
                        t  = (last . last) st -- st = [[s],[t]]
                        pt = p0 +: (s *: v01)
                        ne = negate
                        (*.) [[a, b], [c, d]] [[x], [y]]  = [[a*x + b*y], [c*x + d*y]]

                        v2a (Vertex3 x y z) = [[x, y]]


{-| 
    === Given two segments: \( (p_0, p_1), (q_0, q_1) \), find the overlapped endPt

    == Precondition: Four pts are NOT colinear \( \Rightarrow \) __any three pts__ are NOT colinear

    If two segments are overlapped at one endPt, return Maybe(Vertex3 GLfloat, GLfloat s, GLfloat t)
    else return Nothing

    == Four cases
    1. \( p_0 = q_0 \)
    2. \( p_0 = q_1 \)
    3. \( p_1 = q_0 \)
    4. \( p_1 = q_1 \)

    < https://xfido.com/image/onept_overlapped_segment.svg onept_overlapped_segment >

-} 
onePtOverlappedSeg::(Vertex3 GLfloat, Vertex3 GLfloat) -> 
                    (Vertex3 GLfloat, Vertex3 GLfloat) -> 
                    Maybe (Vertex3 GLfloat, (GLfloat, GLfloat))
onePtOverlappedSeg (p0, p1) (q0, q1) | p0 == q0 = Just (p0, (0.0, 0.0)) -- (Vertex3, s, t) 
                                     | p0 == q1 = Just (p0, (0.0, 1.0)) -- (Vertex3, s, t) 
                                     | p1 == q0 = Just (p1, (1.0, 0.0)) -- (Vertex3, s, t) 
                                     | p1 == q1 = Just (p1, (1.0, 1.0)) -- (Vertex3, s, t) 
                                     | otherwise = Nothing

-- | Four Vertex3 in a list
--
intersectLine'::[Vertex3 GLfloat] ->
               Maybe (Vertex3 GLfloat, [[GLfloat]])
intersectLine' [p0@(Vertex3 x0 y0 z0)
               ,p1@(Vertex3 x1 y1 z1)
               ,q0@(Vertex3 a0 b0 c0)
               ,q1@(Vertex3 a1 b1 c1)
               ]
              = if is0 && is1 then Nothing else (Just (pt, st))
              where
                is0 = isColinear p0 q0 q1
                is1 = isColinear p1 q0 q1
                v01 = p1 -: p0 -- p₁ - p₀  f(t) = p₀ + s(p₁ - p₀)
                u01 = q1 -: q0 -- q₁ - q₀  f(s) = q₀ + t(q₁ - q₀)
                ma  = [[ne (x1 - x0), (a1 - a0)],  -- [s]
                      [ne (y1 - y0), (b1 - b0)]]  -- [t]
                ivm = inv2 ma
                v   = [[x0 - a0],   -- p₀ - q₀
                      [y0 - b0]]
                -- solve s and t
                -- [s]
                -- [t]
                st = multiMat ivm v
                -- |  st = ivm *. v
                s  = (head . head) st -- st = [[s],[t]]
                pt = p0 +: (s *: v01)
                ne = negate
                (*.) [[a, b], [c, d]] [[x], [y]]  = [[a*x + b*y], [c*x + d*y]]

-- | TODO: fix, the code only handles two dimension.
-- | Given a point: p0, line: q0 q1
-- | Compute the distance from p0 to line: q0 q1
-- |
pointToLine::Vertex3 GLfloat ->
             Vertex3 GLfloat ->
             Vertex3 GLfloat ->
             GLfloat
pointToLine p0 q0 q1 = if is then 0.0 else di
            where
                is = isColinear p0 q0 q1 -- if three pts are colinear, return zero
                nr = perpcw $ (q1 -: q0) -- normal of q0 -> q1
                p1 = p0 +: nr -- p0 +: t*normal where t = 1
                vx = fst $ fromJust $ intersectLine p0 p1 q0 q1  -- compute the insection of two lines

                -- square root can not be represented in Rational number in general,
                -- Convert: Vertex3 Rational => Vertex3 GLfloat
                vx'= fmap (realToFrac) vx
                di = dist p0 vx'


{-|
    === Find the intersection of two segments, 'intersectLine' or 'intersectSegNoEndPt'
    ==== Assume that two endpoints of each segment are not overlapped.(segment has non-zero length)

    * If two EndPts from different segments are over overlapped => then Maybe(Vertex3 endpt)

    __NOTE__ This function is ONLY for two dimensions

    __NOTE__ endpoins of different segments may be coincide

    @
    intersectSeg (p0, p1) (q0, q1)
    Nothing
    
    intersectSeg (p0, p1) (q0, q1)
    Vertex3 x y z

    v0 = Vertex3 0 0 0
    v1 = Vertex3 1 1 0
    u0 = Vertex3 1 1 0
    u1 = Vertex3 1 0 0
    intersectSeg (v0, v1) (v1, u1)
    Just (Vertex3 1.0 1.0 0.0)
    @

    The function is based on 'intersectLine'
    * If two segments are __parallel__ or __overlapped__ then return Nothing

    TODO: add test cases
-}
intersectSeg::(Vertex3 GLfloat, Vertex3 GLfloat)->(Vertex3 GLfloat, Vertex3 GLfloat)->Maybe (Vertex3 GLfloat)
intersectSeg (p0, p1) (q0, q1) = case is of
                                      Just x -> if (s ∈ [0.0, 1.0]) && (t ∈ [0.0, 1.0]) then (Just (fst x)) else Nothing
                                        where s = (head . head) $ snd x
                                              t = (last . last) $ snd x
                                      _      -> Nothing
               where
                is = intersectLine p0 p1 q0 q1

{-|
    === intersection excluding two EndPts, 'intersectLine' or 'intersectSeg'

    Sun Feb 17 19:24:22 2019

    There are some issues in endpts overlapped

    Deprecated, Should use 'intersectSegNoEndPt2'

    If four pts are colinear \( \Rightarrow \) Nothing

-}
intersectSegNoEndPt::(Vertex3 GLfloat, Vertex3 GLfloat)->(Vertex3 GLfloat, Vertex3 GLfloat)->Maybe (Vertex3 GLfloat)
intersectSegNoEndPt (p0, p1) (q0, q1) = case is of
                                      Just x -> if s /= 0.0 && s /= 1.0 && t /= 0.0 && t /= 1.0 && (s ∈ [0.0, 1.0]) && (t ∈ [0.0, 1.0]) then (Just (fst x)) else Nothing
                                        where s = (head . head) $ snd x
                                              t = (last . last) $ snd x
                                      _      -> Nothing
               where
                is = intersectLine p0 p1 q0 q1

{-| 
    === intersection excluding two EndPts, 'intersectLine' or 'intersectSeg'
    Sun Feb 17 19:26:05 2019 

    Fixed some bugs in 'intersectSegNoEndPt'

    @
    v0 = Vertex3 0 0 0
    v1 = Vertex3 1 1 0
    u0 = Vertex3 1 1 0
    u1 = Vertex3 1 0 0
    intersectSegNoEndPt2 v0 v1 v1 u1
    Nothing
    @

    SEE picture
    < http://xfido.com/image/endpoint_intersection.svg  end_point_intersection >
-} 
intersectSegNoEndPt2::(Vertex3 GLfloat, Vertex3 GLfloat)->(Vertex3 GLfloat, Vertex3 GLfloat)->Maybe (Vertex3 GLfloat)
intersectSegNoEndPt2 (p0, p1) (q0, q1) | overLapped /= Nothing = Nothing
                                       | is /= Nothing = if s ∈ [0.0, 1.0] && (t ∈ [0.0, 1.0]) then mj else Nothing
                                       | otherwise = Nothing
                                          where 
                                                is = intersectLine2 p0 p1 q0 q1
                                                s  = (fst . snd) $ fromJust is 
                                                t  = (snd . snd) $ fromJust is  
                                                mj = Just $ fst $ fromJust is 
                                                overLapped = onePtOverlappedSeg (p0, p1) (q0, q1)

                                       
{-|
    === If four pts are colinear return False 
    === If the intersection is within \( s \in [0.0, 1.0], t \in [0.0, 1.0] \) return True
    === Else return False
-}
isIntersectedSeg::(Vertex3 GLfloat, Vertex3 GLfloat)->(Vertex3 GLfloat, Vertex3 GLfloat)->Bool
isIntersectedSeg (p0, p1) (q0, q1) = case is of
                                      Just x -> (s ∈ [0.0, 1.0]) && (t ∈ [0.0, 1.0])
                                        where s = (head . head) $ snd x
                                              t = (last . last) $ snd x
                                      _      -> False
               where
                is = intersectLine p0 p1 q0 q1

{-|
    === intersect line, return Rational
-}
intersectLineR::Vertex3 GLfloat ->
                Vertex3 GLfloat ->
                Vertex3 GLfloat ->
                Vertex3 GLfloat ->
                Maybe (Vertex3 Rational, [[Rational]])
intersectLineR p0@(Vertex3 x0 y0 z0)
               p1@(Vertex3 x1 y1 z1)
               q0@(Vertex3 a0 b0 c0)
               q1@(Vertex3 a1 b1 c1)
               = if is0 && is1 then Nothing else (Just (pt, st))
               where
                is0 = isColinear p0 q0 q1
                is1 = isColinear p1 q0 q1
                v01 = fmap (toRational) $ p1 -: p0 -- p₁ - p₀  f(t) = p₀ + s(p₁ - p₀)
                u01 = q1 -: q0 -- q₁ - q₀  f(s) = q₀ + t(q₁ - q₀)
                ma = [[ne (x1 - x0), (a1 - a0)],  -- [s]
                      [ne (y1 - y0), (b1 - b0)]]  -- [t]
                ma'= toR ma
                ivm= invR ma'
                v'  = [[x0 - a0],   -- p₀ - q₀
                      [y0 - b0]]
                v = toR v'
                -- solve s and t
                -- [s]
                -- [t]
                st = ivm *. v
                s  = (head . head) st -- st = [[s],[t]]
                p0' = fmap (toRational) p0
                pt = p0' +> (s **: v01)
                ne = negate
                invR::[[Rational]] -> [[Rational]]
                invR [[a, b], [c, d]] = (1/detR([[a, b], [c, d]])) #: [[d, (-b)], [(-c), a]]

                (*.)::[[Rational]]->[[Rational]] -> [[Rational]]
                (*.) [[a, b], [c, d]] [[x], [y]]  = [[a*x + b*y], [c*x + d*y]]

                detR::[[Rational]] -> Rational
                detR [[a, b], [c, d]] = a*d - b*c
                (#:)::Rational->[[Rational]] -> [[Rational]]
                (#:) r cx = (fmap . fmap) (*r) cx

                (**:)::Rational -> Vector3 Rational -> Vector3 Rational
                (**:) r (Vector3 x y z) = Vector3 (r*x) (r*y) (r*z)

                toR m = (fmap . fmap) (toRational) m

                (+>)::Vertex3 Rational -> Vector3 Rational -> Vertex3 Rational
                (+>) (Vertex3 a b c) (Vector3 x y z) = Vertex3 (a + x) (b + y) (c + z)

{-|
 === Three points rotation order can be determinated by the \( \color{red}{\text{Right Hand Rule}} \)
 <http://localhost/html/indexConvexHullAlgorithm.html#rotate_dir Three_Points_Direction>

  If given three points in following order:

  p₀ = (1, 0)

  p₁ = (0, 0)

  p₂ = (0, 1)

  then vectors are computed in following:

  v10 = p₀ - p₁

  v12 = p₂ - p₁

 matrix can be formed as following:

 m = [v10, v12]

 \( \vec{v_{01}} \times \vec{v_{02}} \) in Right Hand Rule

 \[
  m = \begin{bmatrix}
      1 & 0 \\
      0 & 1 \\
      \end{bmatrix} \\
  \det m = 1 > 0 \\
 \]

 If the three points are collinear:

 \( \det M = 0 \)

 If the order of three points in clockwise order:

 \( \det M > 0 \)

 If the order of three points in counter clockwise order:

 \( \det M < 0 \)

-}
threePtDeterminant::(Fractional a)=>(Vertex3 a) ->(Vertex3 a)->(Vertex3 a)->a
threePtDeterminant p0 p1 p2 = det m
  where
    v10 = v2m $ p0 -: p1
    v12 = v2m $ p2 -: p1
    v2m (Vector3 x y z) = [x, y]
    m = [v10, v12]

{-|
 === Three points in Counter ClockWise order
 \(  \det M > 0 \)
-}
threePtCCW::(Fractional a, Ord a)=>(Vertex3 a)->(Vertex3 a)->(Vertex3 a)->Bool
threePtCCW p0 p1 p2 = threePtDeterminant p0 p1 p2 < 0

{-|
 === Three points in ClockWise order
 \( \det M < 0 \)
-}
threePtCW::(Fractional a, Ord a)=>(Vertex3 a)->(Vertex3 a)->(Vertex3 a)->Bool
threePtCW p0 p1 p2 = threePtDeterminant p0 p1 p2 > 0

{-|
    === Three points are collinear

\[
    \begin{aligned}
    p_0 &= (x_0, y_0) \\
    p_1 &= (x_1, y_1) \\
    p_2 &= (x_2, y_2) \\
    u   &= p_0 - p_1 \\
    u   &= (x_0 - x_1, y_0 - y_1) \\
    v   &= p_2 - p_1 \\
    v   &= (x_2 - x_1, y_2 - y_1) \\
    M   &= \begin{bmatrix}
           u & v
           \end{bmatrix} \\
    &\mbox{If three pts are colinear, then } \\
    \det M &= 0 \\
    \end{aligned}
\]
-}
threePtCollinear::(Fractional a, Ord a)=>(Vertex3 a)->(Vertex3 a)->(Vertex3 a)->Bool
threePtCollinear p0 p1 p2 = threePtDeterminant p0 p1 p2 == 0.0

{-|
    Given a point \( p_0 \), a line \(q_0, q_2 \)
    === Check whether a point \(p_0\) is on the left size of a line \( q_0, q_2 \), it is based on 'threePtDeterminant' with additional condtion: \(y_2 > y_0 \) or \( y_2 < y_0 \) since

    __NOTE__ y2 \(\neq\) y0 where given (Vertex3 x0 y0 z0) and (Vertex3 x2 y2 z2)

    ==== Assume \( p_0, p_2 \) are not __coincide__

    * If pt on the line, return False
    * If pt on the right side of the line, return False
    * Else return True

    \[
        \begin{aligned}
        p_1 &= (x_1, y_1) \\
        p_0 &= (x_0, y_0) \\
        p_2 &= (x_2, y_2) \\
        u &= \overrightarrow{p1 p0}  \\
        v &= \overrightarrow{p1 p2}  \\
        \end{aligned}
    \]

    if \( y_2 > y_0 \)
    form a determinant
    \[
        \begin{aligned}
         \det \begin{vmatrix}
                u & v
                \end{vmatrix} > 0
        \end{aligned}
    \]
    if \( y_2 < y_0 \)
    form a determinant
    \[
        \begin{aligned}
         \det \begin{vmatrix}
                u & v
                \end{vmatrix} < 0
        \end{aligned}
    \]

    If \( y_2 = y_0 \), compare \( x_0, x_2 \)

    === Check whether a point is below a line \(p_0, p_2\)
    if \( x_2 < x_0 \)
    \[
        \begin{aligned}
         \det \begin{vmatrix}
                u & v
                \end{vmatrix} > 0
        \end{aligned}
    \]

    if \( x_2 > x_0 \)
    \[
        \begin{aligned}
         \det \begin{vmatrix}
                u & v
                \end{vmatrix} < 0
        \end{aligned}
    \]


-}
ptOnLeftLine::(Fractional a, Ord a)=>(Vertex3 a) ->(Vertex3 a)->(Vertex3 a) -> Bool
ptOnLeftLine p0@(Vertex3 x0 y0 z0)
             p1@(Vertex3 x1 y1 z1)
             p2@(Vertex3 x2 y2 z2) = if y2 > y0 then (de > 0) == True
                                        else if y2 < y0 then (de > 0) == False else (de > 0) == False
             where
                de = threePtDeterminant p0 p1 p2

--ptOnUpLine::(Fractional a, Ord a)=>(Vertex3 a) ->(Vertex3 a)->(Vertex3 a) -> Bool
--

{-|
  === KEY: Check whether a point \(p_0\) is inside a triangle \( \triangle ABC \), point inside triangle

  * is a point inside a triangle
  * __NOTE__ the order of three pts: \(A, B, C\) does't matter. e.g CCW or CW
  * \(p_0\) can NOT be the same point as \(A, B, C\), if \(p_0\) is overlapped pts \(A, B, C\), then return (False, 0.0)
  * If \(p_0\) is collinear with AB, BC, or AC, then \(p_0\) is considered inside the \( \triangle ABC \)

  \(p_0\) is the point that is tested
  three points \(A, B, C\) forms a triangle

  * if point p0 inside the triangle, return true

  * The sum of three angles are in degree.

  @
  let p0 = Vertex3 0.1 0.1 0
  let a  = Vertex3 1 0 0
  let b  = Vertex3 0 0 0
  let c  = Vertex3 0 1 0
  
  ptInsideTri p0 (a, b, c)
  (True, 360.0)
  @

  TODO: how to check whether a pt is inside a n-polygon 'ptInsidePolygon'
  <http://localhost/html/indexConvexHullAlgorithm.html#npolygon N-Polygon>

-}
ptInsideTri::Vertex3 GLfloat -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) -> (Bool, GLfloat)
ptInsideTri p0 (a, b, c) = (notSame && is, notSame ? rad $ 0.0)
                           where
                             notSame = not $ p0 `elem` [a, b, c]
                             rad= (cosVex3 b p0 a) + (cosVex3 c p0 b) + (cosVex3 c p0 a)
                             is = abs(rad - 2*pi) < 0.001
  

{-|
    === KEY: same as 'ptInsideTri' is a point inside a triangle

    * Better name
-}
isPtInsideTri::Vertex3 GLfloat -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) -> (Bool, GLfloat)
isPtInsideTri p0 (a, b, c) = ptInsideTri p0 (a, b, c)

{-|
    === KEY: almost same as 'ptInsideTri' is a point inside a triangle

    * Pass a list of vertex (three)
    * Better name
-}  
isPtInsideTriList::Vertex3 GLfloat -> [Vertex3 GLfloat] -> (Bool, GLfloat)
isPtInsideTriList p0 [a, b, c] = ptInsideTri p0 (a, b, c)
  

{-|
Given a point p0 and three pts: q0, q1, q2,

Compute the intersection of line is perpendicular to the plane and passes point p0

If three pts (q0, q1, q2) are colinear, return Nothing

otherwise, return Just (Vertex3 GLfloat)
-}
perpPlane::(Vertex3 GLfloat) ->
           (Vertex3 GLfloat) ->
           (Vertex3 GLfloat) ->
           (Vertex3 GLfloat) ->
           Maybe (Vertex3 Rational)
perpPlane p0@(Vertex3 e0 e1 e2)
          q0@(Vertex3 m0 m1 m2)
          q1@(Vertex3 k0 k1 k2)
          q2@(Vertex3 d0 d1 d2)
          = if is then Nothing else Just pt
        where
            is = isColinear q0 q1 q2
            nr = fromJust $ normal3 q0 q1 q2
            v1 = q1 -: q0
            v2 = q2 -: q0
            qp = p0 -: q0
            vqp = (map . map) (toRational) $ veMat qp
            mat = zipWith3(\x y z -> x ++ y ++ z) (veMat v1) (veMat v2) (veMat $ neg nr)
            mat' = (map . map) (toRational) mat
            ima = inverseR mat'
            sth = multiMatR ima vqp -- sth =  [[s], [t], [h]]
            h  = (last . last) sth
            pt  = p0' `ad` (h `mu` nr') where
                                        p0' = toR p0 -- toRational
                                        mu x y = fmap(*x) y  -- t*v
                                        nr' = toR nr  -- toRational
                                        -- pt = p0 + v in affine space:)
                                        ad (Vertex3 x y z) (Vector3 a b c) = Vertex3 (x + a) (y + b) (z + c)

            -- | local functions
            veMat (Vector3 x y z) = [[x], [y], [z]]
            matVx [[x], [y], [z]] = (Vertex3 x y z)
            neg x = fmap (negate) x
            toR x = fmap (toRational) x

-- | Line intersects Plane
-- | Given Line: p0 p1 and Plane: q0 q1 q2
-- | Compute the intersection of line and plane
-- |
--  p1 = p0 +: h(p1 -: p0)
lineIntersectPlane::Vertex3 GLfloat ->
                    Vector3 GLfloat ->
                    Vertex3 GLfloat ->
                    Vertex3 GLfloat ->
                    Vertex3 GLfloat ->
                    Maybe (Vertex3 Rational)
lineIntersectPlane p0 ve q0 q1 q2 = if is then Nothing else Just pt
                where
                    is = isColinear q0 q1 q2
                    v1 = q1 -: q0 -- q0 -> q1
                    v2 = q2 -: q0 -- q0 -> q2
                    nr = ve
                    qp = p0 -: q0
                    vqp = (map . map) (toRational) $ veMat qp
                    mat = zipWith3(\x y z -> x ++ y ++ z) (veMat v1) (veMat v2) (veMat $ neg nr)
                    mat' = (map . map) (toRational) mat
                    ima = inverseR mat'
                    sth = multiMatR ima vqp -- sth =  [[s], [t], [h]]
                    h  = (last . last) sth
                    pt  = p0' `ad` (h `mu` nr') where
                                                p0' = toR p0 -- toRational
                                                mu x y = fmap(*x) y  -- t*v
                                                nr' = toR nr  -- toRational
                                                -- pt = p0 + v in affine space:)
                                                ad (Vertex3 x y z) (Vector3 a b c) = Vertex3 (x + a) (y + b) (z + c)

                    -- | local functions
                    veMat (Vector3 x y z) = [[x], [y], [z]]
                    matVx [[x], [y], [z]] = (Vertex3 x y z)
                    neg x = fmap (negate) x
                    toR x = fmap (toRational) x


{-|
 === Slow Algorithm 1 \( \color{red}{ \mathcal{O}(n^2)} \)
 1. find the a point \( p_0 \) which has the maximum y-axis value
 2. find the a point \( p' \) from \( p_0 \) so that the rest of all the points on one side of the segment \( \overline{p_0 p'} \)
 3. continua with the rest of points

 <http://localhost/html/indexConvexHullAlgorithm.html Slow_Algorithm>

 >p0 p1 p2 p3
 >(p0, p1) (p0, p2) (p0, p3)
 >(p1, p2) (p1, p3)
 >(p2, p3)

 === Naive Algorithm to compute the convex hull in \( \color{red}{\mathcal{O}(nh)} \) where \( \color{red}{n} \) is the number of points on the plane, \( \color{red}{h} \) is the number of points on the convex hull

 If all the points are around the convex hull, then the runtime will be \( \color{red}{\mathcal{O}(n^2)} \)

 @
 convexHull n pts
 n is the total of vertices
 pts contains all the vertexes

 p0 = Vertex3 0 0 0
 p1 = Vertex3 1 0 0
 p2 = Vertex3 0 1 0
 p3 = Vertex3 0.1 0.1 0
 pts= [p0, p1, p2, p3]
 n  = 4 = len pts
 pts = [p0, p1, p2, p3]
 exp= [[p2, p1],
       [p1, p0],
       [p0, p2]]
 @

 * __Algorithm__
 <http://localhost/html/indexConvexHullAlgorithm.html#convexhull ConvexHull>
 * Find a __top__ vertex: B(Vertex3 x y z) with maximum y-Axis value (TODO: What if there are more than one pts on the top?)
 * Construct a new vertex: A(Vertex3 x+1 y z) from __top__(Vertex3 x y z)
 * Compute \( \cos ∠ABC \) from three points: \(A, B, C\)
 * If there are only two points in pts for the list of Vertex3 e.g. p0 p1
 * then segments: \(\overline{AB} \) and \(\overline{BC} \) are drawn
 <http://localhost/html/indexConvexHullAlgorithm.html#three_pts_angle Three_pts_angle>

 n = length pts \(\Rightarrow\) n vertices \(\Rightarrow\) n edges. if there is two vertices, then \((p_0, p_1)\) and \((p_1, p_0)\)

 vec(top x1) and vec(top x2)

 \(\vec{v_1}\) = top -> x1 = x1 -: top

 \(\vec{v_2}\) = top -> x2 = x2 -: top

 * TODO: Handle the case where the three points are colinear.

 * TODO: If there are more than one point that have maximum y-axis, then choose the left most point
 <http://localhost/html/indexConvexHullAlgorithm.html#top_point Top_Point>

 \( \color{red}{TODO} \): use eliminate points technic to improve the algorithm
 <http://localhost/html/indexConvexHullAlgorithm.html#eliminate_points Eliminate_Points>

 It seems that the algorithm is still \( \color{red}{ \mathcal{O}(nh)} \)
 Given n vertices on a plane. There are \(h\) points on the convex hull.
 Assume \( \frac{1}{h} n \) vertices may be eliminated when the next point is found on the convex hull.
 The total number of steps are:
 \[
 \begin{aligned}
    s &= n + \frac{h-1}{h} n + \frac{h-2}{h} n + \dots + \frac{1}{h} n \\
    s &= n h (1 + 2 + 3 + \dots + h) \\
    s &= n h \frac{(1 + h)h}{2} \\
    s &= n \frac{1+ h}{2} \\
      &= \mathcal{O}(nh) \\
 \end{aligned}
 \]



 When the algo walks around the convex hull in counter clokcwise order.

 segment can be drawn from top point to previous point

 All the points can be removed if they are on the left side of the segment

 \( \color{red}{TODO} \) use other algo: check whether a point is on the left or right side of a segment

 The algo can be check with the determinant of two vectors 'threePtDeterminant'
 <http://localhost/html/indexConvexHullAlgorithm.html#rotate_dir Three_Points_Direction>

  Wednesday, 22 March 2023 17:34 PDT
  NOTE: DO NOT USE IT
-}
convexHull::Int-> [Vertex3 GLfloat] -> [[Vertex3 GLfloat]]
convexHull n pts = map(\(a, b) -> [a, b]) $ convexHull2 n pts

{-| 
    === Same as 'convexHull', except that it return [(Vertex3 GLfloat, Vertex3 GLfloat)]

    Wednesday, 22 March 2023 17:34 PDT
    NOTE: DO NOT USE
-} 
convexHull2::Int-> [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)]
convexHull2 n pts = convexHull' n topx top pts
    where
        -- Find the vertex that has maximum y value.
        ptsSort = qqsort cmp pts where cmp (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = y1 > y2

        -- Find the largest angle
        -- Angle: p0_p1_p2, vec: (p1, p2) and vec: (p1, p0)
        vexSort = mergeSortC cmp $ map(\p2 -> (cosVex3 p0 p1 p2, p1, p2)) cx  -- p1 to all the rest of pts
                        where
                            p1 = head ptsSort
                            p0 = addx1 p1 where addx1 (Vertex3 x y z) = Vertex3 (x+1) y z
                            cx = tail ptsSort
                            cmp c1 c2  = t1 c1 < t1 c2

        top = t2 $ head' vexSort
        topx = addx1 top where addx1 (Vertex3 x y z) = Vertex3 (x + 1) y z
        {--
                      (x,y)    (x+1,y)
                       top - - topx
                      /
                     p0


                     topx
                     /
                    top
                     \
                      p1
        --}
        convexHull' n topx top ls = if (n > 0) && top /= p0 then (convexHull' (n - 1) top p0 cx) ++ [(p0, top)] else [(p0, top)]
                            where
                                -- Use merge sort here for y andthen x
                                -- Choose the left most vertex
                                -- Find the biggest dot product of vec(top p1) vec(top p2) => the smallest angle
                                cx = filter(\x -> x /= top) ls -- remove top pt
                                cmp c1 c2  = t1 c1 < t1 c2 -- the largest angle for the smallest cos(angle)
                                sortAngle = qqsort cmp $ map(\p -> (cosVex3 p top topx, top, p)) $ cx
                                p0 = t3 $ head sortAngle


{-| 
    === Connect all pts inside convex hull without crossing intersection

    <http://localhost/html/indexConvexHullAlgorithm.html#convexhull_all_segments All_Segments_ConvexHull>

    @
    let vexList = [Vertex3 0.1 0.1 0.1, Vertex3 0.4 0.3 0.6, Vertex3 0.6 0.8 0.2]
    mapM_(\vx -> do 
                drawSegment green vx 
                threadDelay 500
                ) $ convexHullAllSeg vexList



    Wednesday, 29 June 2022 02:22 PDT
    BUG:

        cx = [
                Vertex3 0.1   0.1  0
               ,Vertex3 0.2   0.6  0
               ,Vertex3 0.88  0.9  0
               ,Vertex3 0.25  0.34 0
               ,Vertex3 0.12  0.8  0
               ,Vertex3 1.3   0.12 0
              ]

        let lsx = convexHullAllSeg cx

        OUTPUT: lsx
       (Vertex3 0.1 0.1 0.0,Vertex3 0.2 0.6 0.0)
       (Vertex3 0.1 0.1 0.0,Vertex3 0.25 0.34 0.0)
       (Vertex3 0.12 0.8 0.0,Vertex3 0.1 0.1 0.0)
       (Vertex3 0.12 0.8 0.0,Vertex3 0.2 0.6 0.0)
       (Vertex3 0.12 0.8 0.0,Vertex3 0.88 0.9 0.0)
       (Vertex3 0.12 0.8 0.0,Vertex3 1.3 0.12 0.0)

       (Vertex3 0.12 0.8 0.0,Vertex3 1.3 0.12 0.0)
       (Vertex3 0.12 0.8 0.0,Vertex3 1.3 0.12 0.0)

       (Vertex3 0.2 0.6 0.0,Vertex3 0.25 0.34 0.0)

       (Vertex3 0.88 0.9 0.0,Vertex3 0.88 0.9 0.0)  <- Same Vertex

       (Vertex3 1.3 0.12 0.0,Vertex3 0.1 0.1 0.0)
       (Vertex3 1.3 0.12 0.0,Vertex3 0.12 0.8 0.0)
       (Vertex3 1.3 0.12 0.0,Vertex3 0.12 0.8 0.0)
       (Vertex3 1.3 0.12 0.0,Vertex3 0.12 0.8 0.0)
       (Vertex3 1.3 0.12 0.0,Vertex3 0.2 0.6 0.0)
       (Vertex3 1.3 0.12 0.0,Vertex3 0.25 0.34 0.0)

       (Vertex3 1.3 0.12 0.0,Vertex3 0.88 0.9 0.0)  <- Duplicated segments
       (Vertex3 1.3 0.12 0.0,Vertex3 0.88 0.9 0.0)  <- 
    @

    DO NOT USE 'convexHull2'
-} 
convexHullAllSeg::[Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)]
convexHullAllSeg cx = segList hullList diffList
    where
        -- Generate a list of vertexes defining a convex hull from a given set of vertexes
        -- hullList = convexHull2 le cx where le = len cx 
        hullList = convexHull3 cx
        -- All the pts are NOT on the convex hull
        -- (Data.List.\\) [1,2,3] [1,2] => [3]
        diffList = (L.\\) cx $ map fst hullList 

        -- Connect all pts inside convexhull to all the pts on the boundary of convexhull, no intersection
        -- URL: http://localhost/image/allsegment.svg
        segList::[(Vertex3 GLfloat, Vertex3 GLfloat)] -> [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)]
        segList sl []     = sl
        segList sl (p:cx) = segList ((nonCrossSegmentNoEndPt sl p) ++ sl) cx

{-|
   Given a Triangle mesh with all segments and a vertex, return a list of triangles
-}
vertexWithAllTri :: [(Vertex3 GLfloat, Vertex3 GLfloat)] -> Vertex3 GLfloat -> [[Vertex3 GLfloat]]
vertexWithAllTri cx vx = map (\seg -> vx:seg) segInTriangle
  where
    allChildren = unique $ qqsort cmpVex $ map fromJust $ filter (Nothing /=) $ map(\seg -> vexOnSeg vx seg ? Just (otherEnd vx seg) $ Nothing ) sortSeg
    vexOnSeg v [x, y] = v == x || v == y
    otherEnd v [x, y] = v == x ? y $ x
  
    cartesianPair = sortVex $ combin 2 allChildren
  
    sortSeg = sortVex $ map (\(v1, v2) -> [v1, v2]) cx
    originSeg = S.fromList sortSeg
    validSeg = filter (\x -> x /= [] ) $ map (\x -> S.member x originSeg ? x $ []) cartesianPair
    
    segInTriangle = filter (\seg -> let or ls = foldr (\a b -> a || b) False ls
                                    in not $ or $ map (\pt -> fst $ isPtInsideTriList pt (vx:seg) ) allChildren) validSeg

{-|
   === KEY: find all triangles from a set of vertexes and segments

   Assume any three pts forms a triangle without intersection.
-}
meshAllTriangle :: [(Vertex3 GLfloat, Vertex3 GLfloat)] -> [Vertex3 GLfloat] -> [[Vertex3 GLfloat]]
meshAllTriangle cx cv = map head $ L.groupBy cmpL $ qqsort cmpL $ sortVex $ join $ map (\v -> vertexWithAllTri cx v) cv
  where
    cmpL [a0, b0, c0] [a1, b1, c1] = cmpV a0 a1 && cmpV b0 b1 && cmpV c0 c1
    cmpV (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) = x0 == x1 && y0 == y1 && z0 == z1

{-|
   === KEY: triangulation, triangulate

   * Given a set of vertexes and a list of segments
   * Triangulate the mesh and generate a set of segments.

   * Very slow version, 100 vertexes, it takes __12.647__ seconds to triangulate the mesh.

  <http://localhost/image/triangulationxx.png Triangulation>
-}
drawTriangulation :: [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)] -> (GLfloat, GLfloat)-> IO()
drawTriangulation vex seg (sx, sy) = do
      let segM = map (\s -> (True, s)) seg
      let goodSeg = triangulateSimple vex vex segM
      
      let collectionSeg = collectSegment goodSeg
      
      let validTri = meshAllTriangle collectionSeg vex
      mapM_ (\se -> drawSegmentList        yellow $ map (\v -> shiftXY (sx, sy) v) se) $ map tupTols2 collectionSeg
      -- mapM_ (\vx -> drawCircleThreePtListX blue vx) $ map (\seg -> map (\v -> shiftXY (sx, sy) v) seg) validTri
  
{-|
   === KEY: triangulation, triangulate

   * Given a set of vertexes and a list of segments
   * Triangulate the mesh and generate a set of segments.

   * Very slow version, 100 vertexes, it takes __12.647__ seconds to triangulate the mesh.

  <http://localhost/image/triangulationxx.png Triangulation>
-}
drawTriangulationWithCircle :: [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)] -> (GLfloat, GLfloat)-> IO()
drawTriangulationWithCircle vex seg (sx, sy) = do
      let segM = map (\s -> (True, s)) seg
      let goodSeg = triangulateSimple vex vex segM
      
      let collectionSeg = collectSegment goodSeg
      
      let validTri = meshAllTriangle collectionSeg vex
      mapM_ (\se -> drawSegmentList        yellow $ map (\v -> shiftXY (sx, sy) v) se) $ map tupTols2 collectionSeg
      mapM_ (\vx -> drawCircleThreePtListX blue vx) $ map (\seg -> map (\v -> shiftXY (sx, sy) v) seg) validTri
    

collectSegment :: [(Bool, (Vertex3 GLfloat, Vertex3 GLfloat))] -> [(Vertex3 GLfloat, Vertex3 GLfloat)]
collectSegment segM = if len group == 2 then map fromJust $ filter (/= Nothing) $ map (\v -> S.member v set ? Nothing $ Just v) good
                                        else good
  where
    group = L.groupBy (\(a, b) (a', b') -> a == a') $ qqsort (\(a, b) _ -> a) segM
    
    bad   = map (sortSeg . snd) $ last group
    good  = map (sortSeg . snd) $ head group
    set = S.fromList bad
  
    sortSeg s = lsToTup2 $ sortVexL $ tupTols2 s


triangulateSimple :: [Vertex3 GLfloat] -> [Vertex3 GLfloat] -> [(Bool, (Vertex3 GLfloat, Vertex3 GLfloat))] -> [(Bool, (Vertex3 GLfloat, Vertex3 GLfloat))]
triangulateSimple vex [] segM = segM
triangulateSimple vex (v:vx) segM = case mayTup of
                                         Just tup -> let rmSeg = t1 tup
                                                         addSeg = t2 tup
                                                     in [(False, rmSeg),(True, addSeg)] ++ triangulateSimple vex vex ( [(False, rmSeg),(True, addSeg)] ++ segM)
                                         Nothing -> triangulateSimple vex vx segM
  where
    seg = collectSegment segM
    trix = meshAllTriangle seg vex
      -- let tup = flipEdge c (map lsToTup3 trix) seg
    mayTup = flipEdge v (map lsToTup3 trix) seg
  
  
lsToTup2 :: [Vertex3 GLfloat] -> (Vertex3 GLfloat, Vertex3 GLfloat)
lsToTup2 cx = if len cx == 2 then let v1 = cx !! 0
                                      v2 = cx !! 1
                             in (v1, v2)
                             else error "ERROR: List of two only"

lsToTup3 :: [Vertex3 GLfloat] -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)
lsToTup3 cx = if len cx == 3 then let v1 = cx !! 0
                                      v2 = cx !! 1
                                      v3 = cx !! 2
                             in (v1, v2, v3)
                             else error "ERROR: List of three only"
tupTols3 :: (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) -> [Vertex3 GLfloat]
tupTols3 (x, y, z) = [x, y, z]
  
tupTols2 :: (Vertex3 GLfloat, Vertex3 GLfloat) -> [Vertex3 GLfloat]
tupTols2 (a, b) = [a, b]

{-|
   === KEY:

   1. Given a vertex and a list of triangles.
   2. If the vertex is inside the inscribe circle in the triangle.
   3. Remove a segment and add a segment, add two triangles
-}
flipEdge :: Vertex3 GLfloat -> [(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)] -> [(Vertex3 GLfloat, Vertex3 GLfloat)] -> Maybe ((Vertex3 GLfloat, Vertex3 GLfloat), (Vertex3 GLfloat, Vertex3 GLfloat), [(Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)])
flipEdge v [] ccx = Nothing
flipEdge v (triangle@(x, y, z):tri) ccx = if
                                          | x == v || y == v || z == v -> flipEdge v tri ccx
                                          | not $ isInside -> flipEdge v tri ccx
                                          | interSeg (x, v) (y, z) /= Nothing && lnx == 1 -> Just ((y, z), (x, v), [(v, x, z),(v, x, y)])
                                          | interSeg (y, v) (x, z) /= Nothing && lny == 1 -> Just ((x, z), (y, v), [(v, y, z),(v, x, y)])
                                          | interSeg (z, v) (x, y) /= Nothing && lnz == 1 -> Just ((x, y), (z, v), [(v, y, z),(v, x, z)])
                                          | otherwise   -> flipEdge v tri ccx
          where
            lnx = len $ filter (/= Nothing) $ intersectSegNoEndPtList (x, v) ccx
            lny = len $ filter (/= Nothing) $ intersectSegNoEndPtList (y, v) ccx
            lnz = len $ filter (/= Nothing) $ intersectSegNoEndPtList (z, v) ccx
            interSeg = intersectSegNoEndPt2
            isInside = fst $ isPtInsideInscribeCircle v triangle
  
isPtOnInscribeCircle :: Vertex3 GLfloat -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) -> (Bool, GLfloat)
isPtOnInscribeCircle v (a, b, c) = case mcen of
                                       Just cen -> let k = distX cen v - distX cen a in (abs(k) < epsilon_, k)
                                       Nothing -> error $ "ERROR: Can not form a cirlce with three pts=>" ++ show (a, b, c)
  where
   mcen = threePtCircle a b c
  
isPtInsideInscribeCircle :: Vertex3 GLfloat -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) -> (Bool, GLfloat)
isPtInsideInscribeCircle v (a, b, c) = if not bo && k < 0 then (True, k) else (False, k)
  where
   tup = isPtOnInscribeCircle v (a, b, c)
   bo = fst tup
   k  = snd tup
  
intersectSegNoEndPt3::[Vertex3 GLfloat] ->[Vertex3 GLfloat] ->Maybe (Vertex3 GLfloat)
intersectSegNoEndPt3 [p0, p1] [q0, q1] = intersectSegNoEndPt2 (p0, p1) (q0, q1)

cmpVex::Vertex3 GLfloat -> Vertex3 GLfloat -> Bool
cmpVex (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) = x0 /= x1 ? x0 < x1 $ (y0 /= y1 ? y0 < y1 $ (z0 < z1))


sortVex::[[Vertex3 GLfloat]] -> [[Vertex3 GLfloat]]
sortVex cx = map (\lv -> qqsort (\a b -> cmpVex a b) lv) cx

sortVexL :: [Vertex3 GLfloat] -> [Vertex3 GLfloat]
sortVexL cx = qqsort (\a b -> cmpVex a b) cx  
  
{-|
  Given a segment, a list of segments
  if a intersection is found, then return the vertex of intersection and the segment
-}
intersectSegNoEndPtList ::(Vertex3 GLfloat, Vertex3 GLfloat) -> [(Vertex3 GLfloat, Vertex3 GLfloat)]-> [Maybe (Vertex3 GLfloat, (Vertex3 GLfloat, Vertex3 GLfloat))]
intersectSegNoEndPtList (v1, v2) cx = map (\seg -> case intersectSegNoEndPt2 (v1, v2) seg of
                                                        Just v -> Just (v, seg)
                                                        Nothing -> Nothing
                                          ) cx

  

{-|
  === KEY: convexhull, convex hull

  Thursday, 23 March 2023 15:41 PDT
  TODO: How to handle the boundary pts are colinear, DONE

  <http://localhost/image/convexhullgood.png covexhull_best>
-}
convexHull3 :: [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)]
convexHull3 cx = convexHullX top topx top' rest
  where
    -- TODO: Why x < x' does not work
    -- cmpPt v@(Vertex3 x y z) v'@(Vertex3 x' y' z') = y /= y' ? y > y' $ (x /= x' ? x < x' $ (error $ "ERROR: cmpPt, same pts = " ++ show v ++ " " ++ show v'))
    cmpPt v@(Vertex3 x y z) v'@(Vertex3 x' y' z') = y /= y' ? y > y' $ (x /= x' ? x > x' $ (error $ "ERROR: cmpPt, same pts = " ++ show v ++ " " ++ show v'))
    sortY = qqsort cmpPt
  
    top = head $ sortY cx
    topx = shiftX top 10.0
  
    rest = filter (/= top) cx
    top' = top
    convexHullX :: Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)]
    convexHullX top topx top' [] =  [(top, top')]
    convexHullX top topx top' rest =  if pi == top' then [(pi, top)] else (convexHullX pi top top' rest') ++ [(pi, top)]
            where
              --                                largest angle                    smallest dist
              cmp c1 c2 = (t1 c1 /= t1 c2) ? (t1 c1 > t1 c2) $ (t2 c1 /= t2 c2 ? t2 c1 < t2 c2 $ (error $ "ERROR: cmp, same pts = " ++ show c1 ++ " " ++ show c2))
              -- cmp c1 c2 = (t1 c1 > t1 c2)
              sortAngle = qqsort cmp $ map (\p -> (cosVex3 p top topx, sqdist p top, p)) $ (top /= top' ? (rest ++ [top']) $ rest)
              -- sortAngle = qqsort cmp $ map (\p -> (cosVex3 p top topx, sqdist p top, p)) rest
              pi = t3 $ head sortAngle
              rest' = filter (/= pi) rest
{-|
   === KEY: convexhull, loop all the outer boundary to inner boundary

   <http://localhost/image/convexhullloop.png convexhullloop>
-}
convexHullLoop :: [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)]
convexHullLoop cx = convexHullX top topx top' rest
  where
    -- TODO: Why x < x' does not work
    -- cmpPt v@(Vertex3 x y z) v'@(Vertex3 x' y' z') = y /= y' ? y > y' $ (x /= x' ? x < x' $ (error $ "ERROR: cmpPt, same pts = " ++ show v ++ " " ++ show v'))
    cmpPt v@(Vertex3 x y z) v'@(Vertex3 x' y' z') = y /= y' ? y > y' $ (x /= x' ? x > x' $ (error $ "ERROR: cmpPt, same pts = " ++ show v ++ " " ++ show v'))
    sortY = qqsort cmpPt
  
    top = head $ sortY cx
    topx = shiftX top 10.0
  
    rest = filter (/= top) cx
    top' = top
    convexHullX :: Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> [Vertex3 GLfloat] -> [(Vertex3 GLfloat, Vertex3 GLfloat)]
    convexHullX top topx top' [] =  [] -- [(top, top')] -- include the last segment for a loop
    convexHullX top topx top' rest =  if pi == top' then [(pi, top)] else (convexHullX pi top top' rest') ++ [(pi, top)]
            where
              --                                largest angle                    smallest dist
              cmp c1 c2 = (t1 c1 /= t1 c2) ? (t1 c1 > t1 c2) $ (t2 c1 /= t2 c2 ? t2 c1 < t2 c2 $ (error $ "ERROR: cmp, same pts = " ++ show c1 ++ " " ++ show c2))
              -- cmp c1 c2 = (t1 c1 > t1 c2)
              sortAngle = qqsort cmp $ map (\p -> (cosVex3 p top topx, sqdist p top, p)) rest
              -- sortAngle = qqsort cmp $ map (\p -> (cosVex3 p top topx, sqdist p top, p)) rest
              pi = t3 $ head sortAngle
              rest' = map t3 $ tail sortAngle
  

-- shift alone on X-axis
shiftX (Vertex3 x y z) a = Vertex3 (x + a) y z
shiftXList ls a = map (\v -> shiftX v a) ls
  
-- shift alone on Y-axis
shiftY (Vertex3 x y z) a = Vertex3 x (y + a) z
shiftYList ls a = map (\v -> shiftY v a) ls

shiftXY (x', y') (Vertex3 x y z) = Vertex3 (x + x') (y + y') z
  
shiftXYList t ls = map (\v -> shiftXY t v) ls
  
shiftTriX :: (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) -> GLfloat -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)
shiftTriX (a, b, c) k = (shiftX a k, shiftX b k, shiftX c k)

shiftTriY :: (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) -> GLfloat -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)
shiftTriY (a, b, c) k = (shiftY a k, shiftY b k, shiftY c k) 


{-|
    === Check whether a pt is inside a polygon (assume the polygon is convex)
    === This is essentially a Covex Hull problem => use Convex Hull Algorithm

    TODO: Add test cases, never test it yet

    @
    let p  = Vertex3 0.2 0.3 0
    let v0 = Vertex3 0   0.6 0
    let v1 = Vertex3 0.6 0.6 0
    let v2 = Vertex3 0.0 0.0 0
    ptInsidePolygon p [v0, v1, v2]  -- return True 
    @
-}
ptInsidePolygon::Vertex3 GLfloat -> [Vertex3 GLfloat] -> Bool
ptInsidePolygon p0 cx = not $ containPt p0 cxx
    where
        cx' = cx ++ [p0]
        sz = len cx'
        cxx = map(\x -> head x) $ convexHull sz cx'

{-|
    === Given a \(p_0\): Vertex3 and [Vertex3], check whether the list contain p0
-}
containPt::Vertex3 GLfloat -> [Vertex3 GLfloat] -> Bool
containPt p0 cx = S.member p0 s
    where
        s = S.fromList cx
{-|
    === Projection from \(u\) onto \(v\) in Vector3
    <http://localhost/pdf/projectionlatex.pdf projection>
-}
projv::(Vector3 GLfloat)->(Vector3 GLfloat)->(Vector3 GLfloat)
projv u v = w'
  where
    u' = veMat u
    v' = veMat v
    w  = projn u' v'
    w' = matVe w

{-|
   === Compute the norm of a vector, length of a vector

   \(v = (x, y, z)\)

   \(|v| = \sqrt{ x^2 + y^2 + z^2} \)
-}
nr::Vector3 GLfloat -> GLfloat
nr v = sqrt $ dot3ve v v
       
nr2::Vector3 GLdouble -> GLdouble
nr2 v = sqrt $ dot3ve v v
       
{-|
  === Normalize a vector

  e.g \( \|\vec{v}\| = 1 \)
-}
uv::(Vector3 GLfloat)->(Vector3 GLfloat)
uv v = fmap (/n) v
    where
        n = nr v
{-|
    === Rodrigue formula

    \(u\) rotates around \(v\) in angle \(\phi\) in right hand rule

    rejection = \(u - p\)

    gx <http://localhost/pdf/rotate_arbitrary_axis.pdf Rotation>
-}
rod u v θ = tv
  where
    p = projv u v
    w = u ⊗ v -- right hand rule
    re= u - p
    u'= ((cos θ) *: re) + (((sin θ) * (nr re)) *: (uv w))
    tv= p + u'

{-|
    === Compute an angle from three points: \(a, b, c \) with dot product

    >let a = Vertex3 1 0 0
    >let b = Vertex3 0 0 0
    >let c = Vertex3 1 0 0
    >cosVex3 a b c
    >0.0

    \( \vec{ba} = a - b \)

    \( \vec{bc} = c - b \)

    \[
        \begin{equation}
        \begin{aligned}
            \vec{ba} \circ \vec{bc} &= | \vec{ba} | | \vec{bc} | \cos{\angle ABC}  \\
            \cos{\angle ABC} &= \frac{ \vec{bc} \circ \vec{bc} }{| \vec{ba} | | \vec{bc}|} \\
        \end{aligned}
        \end{equation}
    \]

-}
cosVex3::Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> GLfloat
cosVex3 p0 p1 p2 = acos $ d/(n1*n2)
                    where
                        v10 = p0 -: p1
                        v12 = p2 -: p1
                        d = dot3ve v10 v12
                        n1 = nr v10
                        n2 = nr v12
{-|
    === Compute an angle from three points: \(a, b, c \) with dot product

    * The angle is defined by angle $\angle ABC$ in $\bigtriangleup ABC$ from point $a$ to $c$ in counter-clockwise

    >let a = Vertex3 1 0 0
    >let b = Vertex3 0 0 0
    >let c = Vertex3 1 0 0
    >angleThreePts a b c
    >0.0

    @
      interval of angle is [0, π]
    @
-}
angleThreePts::Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> GLfloat
angleThreePts p0 p1 p2 = acos $ d/(n1*n2)
                    where
                        v10 = p0 -: p1  -- vector p1 -> p0
                        v12 = p2 -: p1  -- vector p1 -> p2
                        d = dot3ve v10 v12
                        n1 = nr v10
                        n2 = nr v12


{-|
    === Find all the segments DO NOT Cross some new segments
    Current algorithm is brute force

    __NOTE__ use __Convex Hull algo__ might be faster

    __NOTE__ EndPts are excluded, please see 'intersectSegNoEndPt'

    * Given a list of segments and a point

    * _SEE_ pictures to better understand it
    <http://localhost/html/indexConvexHullAlgorithm.html#non_cross_segment non_cross_segment>

    <http://localhost/html/indexConvexHullAlgorithm.html#non_cross_segment_2 non_cross_segment_2>

    Given Segments \( [(B, E), (E, A), (A, D), (D, C)] \) pt: \(F\)

    return => \( [(A, F), (F, D), (F, C)] \)

    @
    let p0 = Vertex3 0 0 0
        p1 = Vertex3 0.5 0 0
        p2 = Vertex3 0 0.5 0
        q0 = Vertex3 1 1 0
        ls = [(p0, p1), (p0, p2)]
        exp= sort [(p0, q0), (p1, q0), (p2, q0)]
        in exp == (sort $ nonCrossSegmentNoEndPt ls q0)

    let p0 = Vertex3 0 0 0
        p1 = Vertex3 0.5 0 0
        p2 = Vertex3 0 0.5 0
        q0 = Vertex3 1 1 0
        ls = [(p0, p1), (p1, p2)]
        exp= sort [(p1, q0), (p2, q0)]
        in exp == (sort $ nonCrossSegmentNoEndPt ls q0)
    @

    remove all the segments with same vertex from old segments list
    then check all the new segments with the rest of old segments list

-}
nonCrossSegmentNoEndPt::[(Vertex3 GLfloat, Vertex3 GLfloat)] ->
                        Vertex3 GLfloat ->
                        [(Vertex3 GLfloat, Vertex3 GLfloat)]
nonCrossSegmentNoEndPt cx p = S.toList $ delSeg (S.fromList newSeg) badSeg
        where
            -- Cartesian product
            -- checkInter oldSeg newSeg = join $ map(\x -> map(\y -> (intersectSegNoEndPt x y) /= Nothing) oldSeg) newSeg
            badSeg = map(\x -> snd x) $ filter(\x -> fst x == True) $ checkInter oldSeg newSeg

            delSeg::Set (Vertex3 GLfloat, Vertex3 GLfloat) ->
                    [(Vertex3 GLfloat, Vertex3 GLfloat)] ->
                    Set (Vertex3 GLfloat, Vertex3 GLfloat)
            delSeg s []     = s
            delSeg s (b:bx) = delSeg (g s b) bx
                where
                    g s b = delete b s


            frm::[(Vertex3 GLfloat, Vertex3 GLfloat)] ->
                 [(Vertex3 GLfloat, Vertex3 GLfloat)] ->
                 [(Vertex3 GLfloat, Vertex3 GLfloat)]
            frm cx [] = cx
            frm cx (b:bx) = frm (g cx b) bx
                where
                    g cx b = filter(\x -> x /= b) cx

            checkInter oldSeg newSeg = join $ map(\nx -> map(\y -> ((intersectSegNoEndPt2 nx y) /= Nothing, nx)) oldSeg) newSeg
            -- Did not improve performance 
--            checkInter oldSeg newSeg = join $ map(\nx -> 
--                                map(\y -> ((intersectSegNoEndPt2 nx y) /= Nothing, nx)) (rmAdjacentSeg (fst nx) oldSeg)) newSeg
            oldSeg = cx         -- [(p0, p1)]
            newSeg = mkSeg cx p -- [(p0, p), (p1, p)]
            mkSeg cx p = map(\x -> (x, p)) $ fv cx -- [(p0, p1)], p => [(p0, p), (p1, p)]
            fv cx = unique $ join $ map(\x -> [fst x, snd x]) cx -- [(p0, p1), (p0, p2), (p1, p2)] => [p0, p1, p2]

{-| 
    === Remove all adjacent edges or adjacent segments from a list of segments/edges
-} 
rmAdjacentSeg::Vertex3 GLfloat -> 
               [(Vertex3 GLfloat, Vertex3 GLfloat)] -> 
               [(Vertex3 GLfloat, Vertex3 GLfloat)]
rmAdjacentSeg p0 cx = filter(\s -> fst s /= p0 && snd s /= p0) cx


{-| 
    === Affine Combination on three points
    == Draw all the points inside a triangle p0 p1 p2

    The number of steps is \( n = 10 \)

    Extend it to polygon

    <http://localhost/image/affine_triangle.png Affine_Triangle>
-} 
affineTri::Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> [Vertex3 GLfloat]
affineTri p0 p1 p2 = unique $ join $ map(\h -> map(\t -> if t + h <= 1 then mu (1 - h - t) p0 + (mu h p1) + (mu t p2) else p0 ) tt ) hh 
    where
        mu x v = (*x) <$> v

        hh::[Float]
        hh = map(\x ->x*del) [0..n]
            where
                del = 1/n;
                n = 10;
        tt = hh



{-| 
    === Draw all segments from Vertex3 a To Vertex3 b
-} 
drawSegmentFromTo::Color3 GLdouble -> [Vertex3 GLfloat] -> IO()
drawSegmentFromTo c cx = do
                            let n = length cx
                            let pair = join $ zipWith(\x y -> [x, y]) (init cx) (tail cx)
                            mapM_ (\x -> drawCircleColor x red 0.002) pair 
                            drawPrimitive' Lines c pair 
                            let one = head pair
                            let las = last pair
                            drawCircleColor one green 0.005 
                            drawCircleColor las blue 0.014
                            
{-| 
    === Draw all segments from Vertex3 a To Vertex3 b

    Deprecated
    Use 'drawSegmentFromToD'
-} 
drawSegmentFromTo2::Color3 GLdouble -> [Vertex3 GLdouble] -> IO()
drawSegmentFromTo2 c cx = do
                            let n = length cx
                            let pair = join $ zipWith(\x y -> [x, y]) (init cx) (tail cx)
                            mapM_ (\x -> drawCircleColor2 x red 0.002) pair 
                            drawPrimitive2 Lines c pair 
                            let one = head pair
                            let las = last pair
                            drawCircleColor2 one green 0.005 
                            drawCircleColor2 las blue 0.014 
{-| 
    === Draw all segments from Vertex3 GLdouble To Vertex3 GLdouble
-} 
drawSegmentFromToD::Color3 GLdouble -> [Vertex3 GLdouble] -> IO()
drawSegmentFromToD c cx = do
                            let n = length cx
                            let pair = join $ zipWith(\x y -> [x, y]) (init cx) (tail cx)
                            mapM_ (\x -> drawCircleColor2 x red 0.002) pair 
                            drawPrimitive2 Lines c pair 
                            let one = head pair
                            let las = last pair
                            drawCircleColor2 one green 0.005 
                            drawCircleColor2 las blue 0.014 
  
                            
{-| 
    === 2d grid on x-y-plane, z=0
    == draw grid

    >mapM_ (\row -> drawSegmentFromTo red row ) grid 
    >mapM_ (\row -> drawSegmentFromTo red row ) $ tran grid 
-} 
grid::[[Vertex3 GLfloat]]
grid =[[ let c = (C a b) in Vertex3 (re c) (im c) 0 | a <- aa] | b <- bb]
        where 
            n  = 10 
            fa = 1/(1.5*n)
            aa = map(\x -> fa * x) [-n..n]
            bb = map(\x -> fa * x) [-n..n]

grid2::(GLfloat -> GLfloat -> GLfloat) -> [[Vertex3 GLfloat]]
grid2 f =[[ Vertex3 x y (f x y) | x <- aa] | y <- bb]
        where 
            n  = 10 
            fa = 1/(1.5*n)
            aa = map(\x -> fa * x) [-n..n]
            bb = map(\x -> fa * x) [-n..n]

{-| 
    === Draw parameter equation.
    x = 2*u    \u -> 2*u        u [1..10]
    y = 3*v    \v -> 3*v        v [3..20]
    z = u + v  \(u, v) -> u + v
-} 
grid3::(GLfloat -> GLfloat -> GLfloat) -> (GLfloat -> GLfloat) -> (GLfloat -> GLfloat) -> [[Vertex3 GLfloat]]
grid3 f u v =[[ Vertex3 x y (f x y) | x <- aa] | y <- bb]
        where 
            n  = 20 
            fa = 1/n
            aa = map(\x -> u x) $ map(*fa) [-n..n]
            bb = map(\x -> v x) $ map(*fa) [-n..n]

drawParamSurf::(GLfloat -> GLfloat -> GLfloat) -> (GLfloat -> GLfloat) -> (GLfloat -> GLfloat) -> IO()
drawParamSurf f u v = do
                mapM_ (\row -> drawSegmentFromTo red row ) $ grid3 f u v
                mapM_ (\row -> drawSegmentFromTo blue row ) $ tran $ grid3 f u v


{-|

  @
  torus2::[(GLfloat, GLfloat, GLfloat)]
  torus2= [ ( fx i k, 
             fy i k, 
             fz i k ) | i <- [1..n], k <-[1..n]]
          where 
              del = rf(2*pi/(n-1))
              n = 100 
              r = 0.2
              br = 0.3

              fx = \i k -> (br + r**cos(del*i))*cos(del*j)
              fy = \i k -> sin(rf del*i)
              fz = \i k -> (br + r*cos(rf del*i))*sin(rf del*j)

    i = [1..n], j = [1..n]
    x = outer + inner × cos(δ × i) × cos(δ × j)
    y = sin(δ × i)
    z = outer + inner × cos(δ × i) × sin(δ × j)
  @

  Torus equation
  http://localhost/html/indexThebeautyofTorus.html
-}
torus2::[[Vertex3 GLfloat]]
torus2 =[[Vertex3 (fx i j) 
                  (fy i j) 
                  (fz i j) | i <- [1..n]] | j <-[1..n]]
        where 
            δ = rf(2*pi/(n-1))
            n = 40 
            r = 0.1
            br = 0.2

            fx = \i j -> (br + r*cos(δ*i))*cos(δ*j)
            fy = \i j -> sin(rf δ*i)
            fz = \i j -> (br + r*cos(rf δ*i))*sin(rf δ*j)


drawTorus2::IO()
drawTorus2 = do
  mapM_ (\row -> drawSegmentFromTo red row ) torus2
  mapM_ (\row -> drawSegmentFromTo blue row ) $ tran torus2


type Fx = Int -> Int -> GLfloat
type Fy = Int -> Int -> GLfloat
type Fz = Int -> Int -> GLfloat

drawParamSurface::Fx -> Fy -> Fz -> IO ()
drawParamSurface fx fy fz = do
  mapM_ (\row -> drawSegmentFromTo red row) ss
  mapM_ (\row -> drawSegmentFromTo blue row) $ tran ss
  where
    n = 40
    ss = [[Vertex3 (fx i j)
                   (fy i j)
                   (fz i j) | i <- [(-n)..n]] | j <- [(-n)..n]]

{-|
    === draw parametric surface

    @
    See $sp/PlotGeometry

    -- draw s sphere
    let n = 40::Int
        δ = (2*pi)/(rf(n-1)) :: Float
        r = 0.4
        br = 0.2
        σ = 1/rf(n-1)

        fx::Int -> Int -> GLfloat
        fx i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in r*cos(α)*cos(β)
        fy::Int -> Int -> GLfloat
        fy i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                     n = 3
                 in r*cos(α)*sin(β)
        
        fz::Int -> Int -> GLfloat
        fz i j = let i' = rf i
                     j' = rf j
                     α  = δ*i'
                     β  = δ*j'
                 in r*sin(α)
        in drawParamSurfaceN fx fy fz n
    @
-}
drawParamSurfaceN::Fx -> Fy -> Fz -> Int -> IO ()
drawParamSurfaceN fx fy fz n = do
  preservingMatrix $ do
        let u = 0.2
        translate (Vector3 u u 0 :: Vector3 GLdouble)
        mapM_ (\row -> drawSegmentFromTo red row) ss
        mapM_ (\row -> drawSegmentFromTo blue row) $ tran ss
      where
        ss = [[Vertex3 (fx i j)
                       (fy i j)
                       (fz i j) | i <- [1..n]] | j <- [1..n]]    
{-|

   'drawParamSurfaceN'
   [1..n]

   'drawParamSurfaceN_new'
   [(-n)..n]
-}
drawParamSurfaceN_new::Fx -> Fy -> Fz -> Int -> IO ()
drawParamSurfaceN_new fx fy fz n = do
  preservingMatrix $ do
        -- let u = 0.2
        let u = 0.0
        translate (Vector3 u u 0 :: Vector3 GLdouble)
        mapM_ (\row -> drawSegmentFromTo red row) ss
        mapM_ (\row -> drawSegmentFromTo blue row) $ tran ss
      where
        ss = [[Vertex3 (fx i j)
                       (fy i j)
                       (fz i j) | i <- [(-n)..n]] | j <- [(-n)..n]]    

{-|
    === KEY: generate parametric surface points
    @
    type Fx = Int -> Int -> GLfloat
    type Fy = Int -> Int -> GLfloat
    type Fz = Int -> Int -> GLfloat
    @
-}
geneParamSurface::Fx -> Fy -> Fz -> Int -> [[Vertex3 GLfloat]]
geneParamSurface fx fy fz n = [[Vertex3 (fx i j)
                                        (fy i j)
                                        (fz i j) | i <- [1..n]] | j <- [1..n]]

{-|
    === KEY: draw surface from list of [Vertex3 GLfloat]
-}
drawSurfaceFromList::[[Vertex3 GLfloat]] -> IO()
drawSurfaceFromList cx = do
  mapM_ (\row -> drawSegmentFromTo red row) cx
  mapM_ (\row -> drawSegmentFromTo blue row) $ tran cx

{-| 
    === grid 2d with ratio: 

    >r = 1  => 1/(r*n) => 1/n
    >r = 2  => 1/(2*n) 
-} 
grid2Ratio::(GLfloat -> GLfloat -> GLfloat) -> GLfloat -> [[Vertex3 GLfloat]]
grid2Ratio f r =[[ Vertex3 x y (f x y) | x <- aa] | y <- bb]
        where 
            n  = 10 
            fa = 1/(r*n)
            aa = map(\x -> fa * x) [-n..n]
            bb = map(\x -> fa * x) [-n..n]

{-| 
    === KEY: list to 'Vertex3', list to Vertex3

    >[1, 2, 3] => (1, 2, 3)
-} 
list3ToVertex3::(Show a)=>[a] -> Vertex3 a  -- [1, 2, 3] => (1, 2, 3)
list3ToVertex3 [a, b, c] = Vertex3 a b c
list3ToVertex3 cx         = error $ "Invalid List" ++ (show cx)
                           
{-| 
    === 'Vertex3' to list

    >(1, 2, 3) => [1, 2, 3]
-} 
vertex3ToList::Vertex3 a -> [a]  -- (1, 2, 3) => [1, 2, 3]
vertex3ToList (Vertex3 a b c) = [a, b, c]

{-|
    === KEY: read file to load geometry(ies)

    1. "--" will be ignored
    2. Emtpy line will be ignored

    Support following geometries so far

    @
      point
      0.1 0.1 0
      0.2 0.2 0
      0.3 0.3 0
      endpoint

      segment
      0.1 0.1 0.1
      0.2 0.2 0.2
      0.3 0.3 0.3
      0.4 0.4 0.4
      endsegment

      triangle
      0.1 0.1 0.1
      0.2 0.2 0.2
      0.3 0.3 0.3
      0.4 0.4 0.4
      0.5 5.5 0.5
      0.6 0.6 0.6
      endtriangle
    @
-}
readGLScript::FilePath -> IO [String]
readGLScript fp = fExist fp >>= \x -> if x then readFileList fp >>= \ls -> return $ filter(\x -> let f = not $ hasPrefix "--" x
                                                                                                     g = (len . trim) x > 0
                                                                                                 in  f && g) ls
                                      else return []
{-|
    === string to vector3
-}
strToVector3::String -> Vector3 GLdouble
strToVector3 str = vec
  where
    s = splitSPC str
    vec = if ρ s == 3 then let a = read (s ! 0)::GLdouble
                               b = read (s ! 1)::GLdouble
                               c = read (s ! 2)::GLdouble
                               (!) = (!!)
                           in Vector3 a b c
          else error $ "Error2: String should contain three GLfloat. str=" + str + " s=" + (show s)
    (+) = (++)                         

{-|
    === KEY: str to triple, string to triple

    @
    String to ('GLfloat', 'GLfloat', 'GLfloat')
    @
-}
strToTriple::String -> (GLfloat, GLfloat, GLfloat)
strToTriple str = tri
  where
    s = splitSPC str
    tri = if (ρ s) == 3 then let a = read (s ! 0)::GLfloat
                                 b = read (s ! 1)::GLfloat
                                 c = read (s ! 2)::GLfloat
                                 (!) = (!!)
                             in (a, b, c)
          else error $ "Error1: String should contain three GLfloat. str=" + str + " s=" + (show s)
    (+) = (++)
    
{-|
    === KEY: 
-}
vertex3ToTriple::Vertex3 GLfloat -> (GLfloat, GLfloat, GLfloat)
vertex3ToTriple (Vertex3 a b c) = (a, b, c)
                                  
{-|
    === KEY: 
-}
tripleToVertex3::(GLfloat, GLfloat, GLfloat) -> Vertex3 GLfloat
tripleToVertex3 (a, b, c) = Vertex3 a b c
                            
{-|
    === KEY: 
-}    
strToVertex3::String -> Vertex3 GLfloat
strToVertex3 str = vex
  where
    s = splitSPC str
    vex = if ρ s == 3 then let a = read (s ! 0)::GLfloat
                               b = read (s ! 1)::GLfloat
                               c = read (s ! 2)::GLfloat
                               (!) = (!!)
                           in Vertex3 a b c
          else error $ "Error2: String should contain three GLfloat. str=" + str + " s=" + (show s)
    (+) = (++)
    
{-|
    === KEY: 
-}    
strToVertex3'::String -> Vertex3 GLdouble
strToVertex3' str = vex
  where
    s = splitSPC str
    vex = if ρ s == 3 then let a = read (s ! 0)::GLdouble
                               b = read (s ! 1)::GLdouble
                               c = read (s ! 2)::GLdouble
                               (!) = (!!)
                           in Vertex3 a b c
          else error $ "Error2: String should contain three GLfloat. str=" + str + " s=" + (show s)
    (+) = (++)
    
{-|
-}
takeVertex3::[String] -> [Vertex3 GLdouble]
takeVertex3 [] = []
takeVertex3 cx = xs
  where
    beg = "vertex3"
    end = "endvertex3"
    ss = filter(\x -> (len . trim) x > 0) $ takeBetweenExc beg end cx
    xs = map(\x -> strToVertex3' x ) $ map trim ss

  
{-|
   === KEY: Draw complex function
   @
                    + Complex function, id => ([-1, 1], [-1, 1]) Grid
                    |
    drawComplex (\c -> c * c) 0.8
                               |
                               + -> Scale image, 1.0 => x = [-1, -1 + 0.1 .. 1]
                                                        y = [-1, -1 + 0.1 .. 1]
    
   @
-}
drawComplex :: (Complex GLfloat -> Complex GLfloat) -> GLfloat -> IO()
drawComplex f sc = do
      when False $ do
        mapM_ (\x -> drawConnect x) lx
        mapM_ (\x -> drawConnect x) $ tran lx

      when True $ do
        mapM_ (\x -> drawConnect x) lxc
        mapM_ (\x -> drawConnect x) $ tran lxc
  where
      cTov c = let x = realPart c; y = imagPart c in Vertex3 x y 0
      vToc (Vertex3 x y z) = x :+ y
      vs = map (\x -> sc*x) [-1, -1 + 0.1 .. 1]
      lx = out(\x y -> Vertex3 x y 0) vs vs
      lxc = (map . map) (\x -> let c = vToc x in cTov $ f c) lx
      vv = head lx

      drawConnect vv = mapM_ (\x -> drawSegment green x) $ let lt = init vv; lv = tail vv in zip lt lv
