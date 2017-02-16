module Main where

import Data.Text.Lazy (Text, pack, unpack)
import Data.Graph.Inductive (Gr, mkGraph)
import Graphics.Gloss
import Graphics.Gloss.Juicy
import WriteRunDot
import Control.Monad (unless, when)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO
import Data.List
import Data.List.Split
import Control.Monad
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Color 
import Graphics.Gloss.Data.Picture
import Control.Concurrent (threadDelay)
import qualified Graphics.UI.GLUT as GLUT
import Data.GraphViz (
  GraphvizParams,
  GlobalAttributes(
    GraphAttrs,
    NodeAttrs
    ),
  X11Color(Transparent, White),
  nonClusteredParams,
  globalAttributes,
  fmtNode,
  fmtEdge,
  graphToDot
  )
import Data.GraphViz.Printing (toDot, renderDot)
import Data.GraphViz.Attributes.Complete (
  DPoint(DVal),
  Attribute(
    Margin,
    Pad,
    Center,
    BgColor,
    FontSize,
    Shape,
    Label,
    ViewPort,
    RankDir,
    Style,
    FillColor
    ),
  Shape(Circle, BoxShape),
  Label(StrLabel),
  ViewPort(VP),
  RankDir(FromLeft),
  StyleName(Filled),
  StyleItem(SItem),
  toWColor,
  wVal,
  hVal,
  zVal,
  focus
  )


-- tiny utility functions, in the same spirit as 'maybe' or 'either'
-- makes the code a wee bit cleaner
bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x
    

-- Callback operations --

errorCallback :: G.ErrorCallback
errorCallback err description = hPutStrLn stderr description


keyCallback :: G.KeyCallback
keyCallback window key scancode action mods
  | key == G.Key'Escape && action == G.KeyState'Pressed = G.setWindowShouldClose window True
  | key == G.Key'Space && action == G.KeyState'Pressed = printkey "space"
  | otherwise = G.setWindowShouldClose window False
  where printkey x = putStrLn x


charCallback :: G.CharCallback
charCallback window codepoint = do
  putChar codepoint
  putStr "\n"


cursorPosCallback :: G.CursorPosCallback
cursorPosCallback window xpos ypos
  | xpos > ypos = printkey xpos
  | otherwise = printkey ypos
  where printkey x = print x


mouseButtonCallback :: G.MouseButtonCallback
mouseButtonCallback window button action mods
  | button == G.MouseButton'1 && action == G.MouseButtonState'Pressed = putStrLn "down"
  | button == G.MouseButton'1 && action == G.MouseButtonState'Released = putStrLn "up"
  | otherwise = putStrLn "other"


-- gnome classes, graphviz functionality --
-- not working --

gnomes :: Gr Text Text
gnomes = mkGraph [(1, pack "Hello"), (3, pack "World")] [(1, 3, pack "?")]

gnomeParams :: GraphvizParams n Text Text () Text
gnomeParams = nonClusteredParams {
  globalAttributes = ga,
  fmtNode = fn,
  fmtEdge = fe
  }
  where
    ga = [
      GraphAttrs [
         RankDir FromLeft,
         BgColor [toWColor Transparent]
         ],
      NodeAttrs [
        Shape BoxShape,
        FillColor [toWColor White],
        Style [SItem Filled []]
        ]
      ]

    fn (n,l) = [(Label . StrLabel) l]
    fe (f,t,l) = [(Label . StrLabel) l]


wid :: Int 
wid = 640


hei :: Int
hei = 480


main :: IO ()
main = do 
  readfile "graphtest.dot.plain"
  --initialisations
  GLUT.exit
  (_,_) <- GLUT.getArgsAndInitialize
  G.setErrorCallback (Just errorCallback)
  successfulInit <- G.init
  glossState <- initState

  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
      mw <- G.createWindow wid hei "Simple example, haskell style" Nothing Nothing
      maybe' mw (G.terminate >> exitFailure) $ \window -> do
          G.makeContextCurrent mw
          --track activity
          G.setKeyCallback window (Just keyCallback)
          G.setCharCallback window (Just charCallback)
          G.setCursorPosCallback window (Just cursorPosCallback)
          G.setMouseButtonCallback window (Just mouseButtonCallback)
          --loop main functionality
          mainLoop window glossState
          G.destroyWindow window
          G.terminate
          exitSuccess

  --graphviz stuff
  --not working
  putStr $ unpack $ renderDot $ toDot $ graphToDot gnomeParams gnomes
  doDots [ ("test.png" , graphToDot gnomeParams gnomes) ]

--read the original .plain file and extract relivant information
readfile :: FilePath -> IO ()
readfile x = do
  handle <- openFile x ReadMode
  contents <- hGetContents handle
  let dotplain = lines contents
  store dotplain
  hClose handle 

--iterate through list and save data
store :: [String] -> IO ()
store [] = putStrLn "done storing nodes"
store [x] = putStrLn "done storing nodes"
store (x:xs) = do
  save x
  store xs

--test for what information the line is giving and write to file
save :: String -> IO ()
save x = do
  let line = splitOn " " x
  if "graph" `elem` line 
    then initialiseFile (line!!2) (line!!3)
    else if "node" `elem` line 
      then addNode (line!!1) (line!!2) (line!!3)
      else if "edge" `elem` line
        then addConnection (line!!1) (line!!2)
        else putStrLn "done"


initialiseFile :: [Char] -> [Char] -> IO ()
initialiseFile a b = do
  writeFile "pos.txt" (a ++ " " ++ b ++ "\n")


getAB :: IO [[Char]]
getAB = do
  contents <- readFileStrict "pos.txt"
  let dotplain = lines contents
  let ab = splitOn " " (head dotplain)
  return ab
  

addNode :: [Char] -> String -> String -> IO ()
addNode name c d = do
  ab <- getAB
  let a = (read (ab!!0)) :: Float
  let b = (read (ab!!1)) :: Float
  let cint = (read c) :: Float
  let dint = (read d) :: Float
  let widfloat = fromIntegral wid :: Float
  let heifloat = fromIntegral hei :: Float
  let xpos = ((cint/a)*widfloat)
  let ypos = (((b-dint)/b)*heifloat)
  appendFile "pos.txt" (name ++ " " ++ show xpos ++ " " ++ show ypos ++ "\n")


addConnection :: [Char] -> [Char] -> IO ()
addConnection name1 name2 = appendFile "pos.txt" (name1 ++ " " ++ name2 ++ "\n")


displayPic :: Picture -> IO ()
displayPic p@(Bitmap width height _ _) = do
    glossState <- initState
    displayPicture (width, height) black glossState 1.0 $
      Pictures [p]
displayPic _ = error "only the Bitmap constructor should be used here"


mainLoop :: G.Window -> State-> IO ()
mainLoop w glossState = unless' (G.windowShouldClose w) $ do
    (width, height) <- G.getFramebufferSize w
    let ratio = fromIntegral wid / fromIntegral hei
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
    --GL.clear [GL.ColorBuffer]
    
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GL.ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
    GL.matrixMode GL.$= GL.Modelview 0
    
    GL.loadIdentity



    --displayPicture (width, height) black glossState 1.0 $
    --  Pictures
    --            [ Color violet $ translate (-300) 100 $ polygon [((-10), 10), ((-10), 70), (20, 20), (20, 30)]
    --            , Color red $ translate (-200) 100 $ line [(-30, -30), (-40, 30), (30, 40), (50, -20)]
    --            , Color (makeColor 0 128 255 1) $ translate (-100) 100 $ lineLoop [(-30, -30), (-40, 30), (30, 40), (50, -20)]
    --            , Color red $ translate 0 100 $ circle 30
    --            , Color green $ translate 100 100 $ thickCircle 30 10
    --            , Color yellow $ translate 200 100 $ circleSolid 30
    --            , Color chartreuse $ translate (-200) (-100) $ thickArc 0 180 30 30
    --            , Color (dark magenta) $ translate (-100) (-100) $ arcSolid 0 90 30
    --            , Color (bright magenta) $ translate 0 (-100) $ scale 0.2 0.2 $ text "Boo!"
    --            , Color (dim cyan) $ translate 100 (-100) $ rotate 30 $ rectangleWire 20 50
    --            , Color (light cyan) $ translate 200 (-100) $ rotate 60 $ rectangleSolid 20 50 ]



    loadJuicyPNG "/home/alexandra/interactive-visualisation/graphtest.dot.png" >>= maybe (putStrLn "Couldn't load file") displayPic 

    
    G.swapBuffers w
    G.pollEvents
    mainLoop w glossState

hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h = hGetContents h >>= \s -> length s `seq` return s

readFileStrict :: FilePath -> IO String
readFileStrict name =  openFile name ReadMode >>= hGetContentsStrict
