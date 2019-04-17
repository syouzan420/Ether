import Data.IORef
import Graphics.UI.GLUT
import Data.List

m :: GLfloat 
m = 1

r :: GLfloat 
r = 1

il :: GLfloat 
il = 20 

k :: GLfloat 
k = 1 

t :: GLfloat 
t = 0.1

data Ball = Ball { bc :: Int, br :: Int,
                   bx :: GLfloat, by :: GLfloat,
                   vx :: GLfloat, vy :: GLfloat,
                   ax :: GLfloat, ay :: GLfloat} deriving Show

data Spring = Spring { sx :: GLfloat, sy :: GLfloat,
                       sp :: GLfloat, sq :: GLfloat,
                       l :: GLfloat } deriving Show

type Ether = ([[Ball]],[[Spring]],[[Spring]])

firstBall :: Ball
firstBall = Ball {bc=0,br=0,bx=0,by=0,vx=0,vy=0,ax=0,ay=0}

firstSpringX :: Spring
firstSpringX = Spring {sx=r,sy=0,sp=r+il,sq=0,l=il}

firstSpringY :: Spring
firstSpringY = Spring {sx=0,sy=r,sp=0,sq=r+il,l=il}

initBall :: Int -> Ball -> [Ball]
initBall 0 _ = []
initBall n b
  = b:initBall (n-1) b{bc=bc b+1,bx=bx b+r*2+il}

initBallXY :: Int -> Int -> Ball -> [[Ball]]
initBallXY _ 0 _ = []
initBallXY nc nr b
  = (initBall nc b):initBallXY nc (nr-1) b{br=br b+1,by=by b+r*2+il}

initSpringX :: Int -> Spring -> [Spring]
initSpringX 0 _ = []
initSpringX n p
  = p:initSpringX (n-1) p{sx=sx p+il+r*2,sp=sx p+il*2+r*2}

initSpringXS :: Int -> Int -> Spring -> [[Spring]]
initSpringXS 0 _ _ = []
initSpringXS _ 0 _ = []
initSpringXS nc nr p
  = (initSpringX nc p):initSpringXS nc (nr-1) p{sy=sy p+il+r*2,sq=sy p+il+r*2}

initSpringY :: Int -> Spring -> [Spring]
initSpringY 0 _ = []
initSpringY n p
  = p:initSpringY (n-1) p{sy=sy p+il+r*2,sq=sy p+il*2+r*2}

initSpringYS :: Int -> Int -> Spring -> [[Spring]]
initSpringYS _ 0 _ = []
initSpringYS 0 _ _ = []
initSpringYS nc nr p
  = (initSpringY nr p):initSpringYS (nc-1) nr p{sx=sx p+il+r*2,sp=sx p+il+r*2}

makeList :: [String] -> [(Int,Int,GLfloat,GLfloat)]
makeList [] = []
makeList (a:[]) = []
makeList (a:b:c:d:ds)
  = (read a::Int,read b::Int,read c::GLfloat,read d::GLfloat):makeList ds 

setBall :: [(Int,Int,GLfloat,GLfloat)] -> [Ball] -> [Ball]
setBall _ [] = []
setBall lst (b:bs)
  = (foldl (\acc (co,ro,rx,ry) -> if (bc b==co)&&(br b==ro) then b{bx=bx b+rx,by=by b+ry} else acc) b lst):(setBall lst bs)

setBallXY :: [(Int,Int,GLfloat,GLfloat)] -> [[Ball]] -> [[Ball]]
setBallXY _ [] = []
setBallXY lst (b:bs)
  = (setBall lst b):setBallXY lst bs

setAccelX :: Bool -> ([Ball],[Spring]) -> [Ball]
setAccelX True (b:[],p:[])
  = b{ax=(-k)*(l p-il)/m*(sp p-(sx p))/(l p),ay=(-k)*(l p-il)/m*(sq p-(sy p))/(l p)}:[]
setAccelX False ((b:bs),pa@(p:ps))
  = b{ax=k*(l p-il)/m*(sp p-(sx p))/(l p),ay=k*(l p-il)/m*(sq p-(sy p))/(l p)}:setAccelX True (bs,pa) 
setAccelX True ((b:bs),(p:q:qs))
  = b{ax=(-k)*(l p-il)/m*(sp p-(sx p))/(l p) + k*(l q-il)/m*(sp q-(sx q))/(l q),
      ay=(-k)*(l p-il)/m*(sq p-(sy p))/(l p) + k*(l q-il)/m*(sq q-(sy q))/(l q)}:setAccelX True (bs,(q:qs))

setAccelY :: Bool -> ([Ball],[Spring]) -> [Ball]
setAccelY True (b:[],p:[])
  = b{ax=ax b+(-k)*(l p-il)/m*(sp p-(sx p))/(l p),ay=ay b+(-k)*(l p-il)/m*(sq p-(sy p))/(l p)}:[]
setAccelY False ((b:bs),pa@(p:ps))
  = b{ax=ax b+k*(l p-il)/m*(sp p-(sx p))/(l p),ay=ay b+k*(l p-il)/m*(sq p-(sy p))/(l p)}:setAccelY True (bs,pa) 
setAccelY True ((b:bs),(p:q:qs))
  = b{ax=ax b+(-k)*(l p-il)/m*(sp p-(sx p))/(l p) + k*(l q-il)/m*(sp q-(sx q))/(l q),
      ay=ay b+(-k)*(l p-il)/m*(sq p-(sy p))/(l p) + k*(l q-il)/m*(sq q-(sy q))/(l q)}:setAccelY True (bs,(q:qs))

setAccelY2 :: Bool -> ([Ball],[Spring]) -> [Ball]
setAccelY2 True (b:[],p:[])
  = b{ax=(-k)*(l p-il)/m*(sp p-(sx p))/(l p),ay=(-k)*(l p-il)/m*(sq p-(sy p))/(l p)}:[]
setAccelY2 False ((b:bs),pa@(p:ps))
  = b{ax=k*(l p-il)/m*(sp p-(sx p))/(l p),ay=k*(l p-il)/m*(sq p-(sy p))/(l p)}:setAccelY2 True (bs,pa) 
setAccelY2 True ((b:bs),(p:q:qs))
  = b{ax=(-k)*(l p-il)/m*(sp p-(sx p))/(l p) + k*(l q-il)/m*(sp q-(sx q))/(l q),
      ay=(-k)*(l p-il)/m*(sq p-(sy p))/(l p) + k*(l q-il)/m*(sq q-(sy q))/(l q)}:setAccelY2 True (bs,(q:qs))

setAccelXS :: [([Ball],[Spring])] -> [[Ball]]
setAccelXS [] = []
setAccelXS (x:xs) = (setAccelX False x):setAccelXS xs

setAccelYS :: [([Ball],[Spring])] -> [[Ball]]
setAccelYS [] = []
setAccelYS (x:xs) = (setAccelY False x):setAccelYS xs

setAccelYS2 :: [([Ball],[Spring])] -> [[Ball]]
setAccelYS2 [] = []
setAccelYS2 (x:xs) = (setAccelY2 False x):setAccelYS2 xs

setVelocity :: [Ball] -> [Ball]
setVelocity [] = []
setVelocity (b:bs)
  = b{vx=vx b+(ax b)*t,vy=vy b+(ay b)*t}:(setVelocity bs) 

setVelocityS :: [[Ball]] -> [[Ball]]
setVelocityS [] = []
setVelocityS (b:bs)
  = (setVelocity b):setVelocityS bs

setPosition :: [Ball] -> [Ball]
setPosition (b:[]) = [b{bx=bx b+(vx b)*t,by=by b+(vy b)*t}]
setPosition (b:c:cs)
  = b{bx=bx b+(vx b)*t,by=by b+(vy b)*t}:(setPosition (c:cs))

setPositionS :: [[Ball]] -> [[Ball]]
setPositionS [] = []
setPositionS (b:bs)
  = (setPosition b):setPositionS bs

setPositionX :: [Ball] -> [Ball]
setPositionX (b:[]) = [b{bx=bx b+(vx b)*t}]
setPositionX (b:c:cs)
  = b{bx=bx b+(vx b)*t}:(setPositionX (c:cs))

setPositionXS :: [[Ball]] -> [[Ball]]
setPositionXS [] = []
setPositionXS (b:bs)
  = (setPositionX b):setPositionXS bs

setPositionY :: [Ball] -> [Ball]
setPositionY (b:[]) = [b{by=by b+(vy b)*t}]
setPositionY (b:c:cs)
  = b{by=by b+(vy b)*t}:(setPositionY (c:cs))

setPositionYS :: [[Ball]] -> [[Ball]]
setPositionYS [] = []
setPositionYS (b:bs)
  = (setPositionY b):setPositionYS bs

setSpringX :: ([Ball],[Spring]) -> [Spring]
setSpringX (_,[]) = []
setSpringX ((b:c:cs),(s:ss))
  = s{sx=bx b+r,sy=by b,sp=bx c-r,sq=by c}:setSpringX (c:cs,ss)

setSpringXS :: [([Ball],[Spring])] -> [[Spring]]
setSpringXS [] = []
setSpringXS (x:xs)
  = (setSpringX x):setSpringXS xs

setSpringY :: ([Ball],[Spring]) -> [Spring]
setSpringY (_,[]) = []
setSpringY ((b:c:cs),(s:ss))
  = s{sx=bx b,sy=by b+r,sp=bx c,sq=by c-r}:setSpringY (c:cs,ss)

setSpringYS :: [([Ball],[Spring])] -> [[Spring]]
setSpringYS [] = []
setSpringYS (x:xs)
  = (setSpringY x):setSpringYS xs

setLength :: [Spring] -> [Spring]
setLength [] = []
setLength (p:ps)
  = p{l=sqrt ((sp p-(sx p))^2+(sq p-(sy p))^2)}:setLength ps

setLengthS :: [[Spring]] -> [[Spring]]
setLengthS [] = []
setLengthS (x:xs)
  = (setLength x):setLengthS xs

newEther :: Ether -> Ether 
newEther (bl,spx,[])
  = let newbl = setPositionS$setVelocityS$setAccelXS (zip bl spx) in
                (newbl,setLengthS$setSpringXS (zip newbl spx),[])
newEther (bl,[],spy)
  = let newbl = setPositionS$setVelocityS$transpose$setAccelYS2 (zip (transpose bl) spy) in
                (newbl,[],setLengthS$setSpringYS (zip (transpose newbl) spy))
newEther (bl,spx,spy) 
  = let newbl = setPositionS$setVelocityS$transpose$setAccelYS (zip (transpose (setAccelXS (zip bl spx))) spy) in
                (newbl,setLengthS$setSpringXS (zip newbl spx),setLengthS$setSpringYS (zip (transpose newbl) spy))

main :: IO () 
main = do
  (_progName, args) <- getArgsAndInitialize
  (cn,rn) <- if (args==[]) 
                   then return (3::GLsizei,3::GLsizei)
                   else return (read$head args ::GLsizei,
                                read$head$tail args ::GLsizei)
  ti <- if (length args)==3
           then return ((read$last args) :: Timeout)
           else return (1 :: Timeout)
  tx2 <- if (length args)>3
           then return (unwords$drop 3 args)
           else return "0 0 10 0"
  let tx = show cn++" "++(show rn) 
  let dispX = 80+24*cn
  let dispY = 70+24*rn
  --let tx2 = "0 0 10 0"
  initialDisplayMode $= [RGBAMode,DoubleBuffered]
  initialWindowSize $= Size dispX dispY 
  _window <- createWindow "ETHER"
  ether <- newIORef (etherInit tx tx2)
--  idleCallback $= Just (idle ether)
  displayCallback $= display ether dispX dispY
  reshapeCallback $= Just reshape
  addTimerCallback ti $ timerProc (idle ether) ti
  mainLoop

etherInit :: String -> String -> Ether 
etherInit tx tx2 = 
  let cn = read$head$words tx :: Int
      rn = read$head$tail$words tx :: Int
      lst = makeList (words tx2) 
  in let bls = initBallXY cn rn firstBall
         spx = initSpringXS (cn-1) rn firstSpringX
         spy = initSpringYS cn (rn-1) firstSpringY
     in let ibl = setBallXY lst bls
            ispx = setLengthS$setSpringXS (zip ibl spx)
            ispy = setLengthS$setSpringYS (zip (transpose ibl) spy)
        in (ibl,ispx,ispy)
  
idle :: IORef Ether -> IdleCallback
idle ether = do
  eth <- get ether
  let eth' = newEther eth
  ether $= eth'
  postRedisplay Nothing

display :: IORef Ether -> GLsizei -> GLsizei -> DisplayCallback
display ether dispX dispY = do
  clearColor $= Color4 0 0 0 0
  clear [ColorBuffer]
  loadIdentity
  ortho2D 0.0 (fromIntegral dispX) 0.0 (fromIntegral dispY) 
  (bl,spx,spy) <- get ether
  currentColor $= Color4 1 0.8 0.5 0
  mapM_ (mapM_ (\b -> preservingMatrix $ do
                  translate (Vector3 (bx b+50) (by b+50) 0)
                  renderPrimitive Polygon $ mapM_ vertex [
                    Vertex3 (4*cos h) (4*sin h) 0 :: Vertex3 GLfloat | h <- [0,0.1..2*pi]
                    ])) bl
  swapBuffers

timerProc :: IO () -> Timeout -> TimerCallback
timerProc act ti = do
  act
  addTimerCallback ti $ timerProc act ti

reshape :: ReshapeCallback
reshape (Size w h) = do
  viewport $= (Position 0 0 , (Size w h))
  matrixMode $= Projection
  loadIdentity
  matrixMode $= Modelview 0
