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
import Data.Char
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


--cursorPosCallback :: G.CursorPosCallback
--cursorPosCallback window xpos ypos = do
--  result <- isNode xpos ypos
--  pressed <- (G.getMouseButton window G.MouseButton'1)
--  let underPressure = (pressed == G.MouseButtonState'Pressed)
--  when (result /= []) $ print (result)
--  when (underPressure) $ print "Pressed"
--  when (result /= [] && underPressure) $ print "PRESSED"


mouseButtonCallback :: G.MouseButtonCallback
mouseButtonCallback window button action mods = do
  (xpos, ypos) <- G.getCursorPos window 
  result <- isNode xpos ypos
  (connectionsRes, name) <- connections result
  --when (button == G.MouseButton'1 && action == G.MouseButtonState'Pressed && result /= []) $ print result
  when (button == G.MouseButton'1 && action == G.MouseButtonState'Pressed && result /= [] && connectionsRes /= 0) $ print ("Number of connections on node " ++ name ++ " is " ++ show connectionsRes)


--calculate the number of conections a node has
connections :: [String] -> IO (Int, String)
connections [_, name, _, _] = do
  contents <- readFileStrict "pos.txt"
  let dotplain = lines contents
  return ((countConnections 0 name dotplain), name)
connections [] = do
  return (0, "")


countConnections :: Int -> String -> [String] -> Int
countConnections acc name [] = 0
countConnections acc name (x:[])
  | name  `elem` (splitOn " " x) = acc   --not +1 cause of the name in the original node line
  | otherwise = (acc-1)   --to account for the original node line
countConnections acc name (x:xs)
  | name `elem` (splitOn " " x) = countConnections (acc+1) name xs
  | otherwise = countConnections acc name xs


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
  --putStrLn "Insert filename, where filename.dot , filename.dot.plain and filename.dot.png exists: "
  --filenameRead <- getLine
  let filenameRead = "graphtest"
  let filenamePlain = filenameRead ++ ".dot.plain"
  let filenamePNG = filenameRead ++ ".dot.png"

  --initialisations
  readfile filenamePlain
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
          --G.setCursorPosCallback window (Just cursorPosCallback)
          G.setMouseButtonCallback window (Just mouseButtonCallback)
          --loop main functionality
          mainLoop window glossState filenamePNG
          G.destroyWindow window
          G.terminate
          exitSuccess

  --graphviz stuff
  --not working
  putStr $ unpack $ renderDot $ toDot $ graphToDot gnomeParams gnomes
  doDots [ ("test.png" , graphToDot gnomeParams gnomes) ]


mainLoop :: G.Window -> State -> String -> IO ()
mainLoop w glossState filenamePNG = unless' (G.windowShouldClose w) $ do
    (width, height) <- G.getFramebufferSize w
    let ratio = fromIntegral wid / fromIntegral hei
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
    --GL.clear [GL.ColorBuffer]
    
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    GL.ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
    GL.matrixMode GL.$= GL.Modelview 0
    
    GL.loadIdentity

    loadJuicyPNG filenamePNG >>= maybe (putStrLn "Couldn't load file") displayPic 

    G.swapBuffers w
    G.pollEvents
    mainLoop w glossState filenamePNG


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
store [_] = putStrLn "done storing nodes"
store (x:xs) = do
  save x
  store xs


--test for what information the line is giving and write to file
save :: String -> IO ()
save x = do
  let allLines = splitOn " " x
  if "graph" `elem` allLines 
    then initialiseFile (allLines!!2) (allLines!!3)
    else if "node" `elem` allLines 
      then addNode (allLines!!1) (allLines!!2) (allLines!!3)
      else if "edge" `elem` allLines
        then addConnection (allLines!!1) (allLines!!2)
        else putStrLn "done"


--input the original size of the image from the .plain file--
initialiseFile :: [Char] -> [Char] -> IO ()
initialiseFile a b = do
  writeFile "pos.txt" (a ++ " " ++ b ++ "\n")


--add a node information to the pos.txt file--
addNode :: [Char] -> String -> String -> IO ()
addNode name c d = do
  ab <- getAB
  let a = if (checkNumber (ab!!0))
            then (read (ab!!0)) :: Float
            else 
              0
  let b = if (checkNumber (ab!!1))
            then (read (ab!!1)) :: Float
            else 
              0
  let cint = if (checkNumber c) 
              then (read c) :: Float
              else 
                0
  let dint = if (checkNumber d)
              then (read d) :: Float
              else
              0
  let widfloat = fromIntegral wid :: Float
  let heifloat = fromIntegral hei :: Float
  let xpos = ((cint/a)*widfloat)
  let ypos = (((b-dint)/b)*heifloat)
  appendFile "pos.txt" ("node " ++ name ++ " " ++ show xpos ++ " " ++ show ypos ++ "\n")


--check for if it is a double or int--
checkNumber :: [Char] -> Bool 
checkNumber [] = True 
checkNumber (x:xs) = (isDigit x || x == '.') && checkNumber xs 


--retrieve the original size of the image from the .plain file now stored in the pop.txt file--
getAB :: IO [[Char]]
getAB = do
  contents <- readFileStrict "pos.txt"
  let dotplain = lines contents
  let ab = splitOn " " (head dotplain)
  return ab


--add edges to the pos.txt file--
addConnection :: [Char] -> [Char] -> IO ()
addConnection name1 name2 = appendFile "pos.txt" (name1 ++ " " ++ name2 ++ "\n")


--display the original image in the GLFW window--
displayPic :: Picture -> IO ()
displayPic p@(Bitmap width height _ _) = do
    glossState <- initState
    displayPicture (width, height) black glossState 1.0 $
      Pictures [p]
displayPic _ = error "only the Bitmap constructor should be used here"


--check if the cursor position is sitting on a node--
isNode :: Double -> Double -> IO [String]
isNode xpos ypos = do
  contents <- readFileStrict "pos.txt"
  let dotplain = lines contents
  let splice = map (splitOn " ") dotplain
  let nodes = map extractNode splice
  let nodesBool = (map (inRange xpos ypos)) nodes
  let index = elemIndex True nodesBool 
  if index == Nothing
    then return []
    else return (nodes !!(extractMaybe index))


extractMaybe :: Maybe t -> t
extractMaybe (Just x) = x


--isolate the node data--
extractNode :: [String] -> [String]
extractNode content
  | "node" `elem` content = content
  | otherwise = [""]


--check if cursor position is within curtain range to node--
inRange :: Double -> Double -> [String] -> Bool
inRange xpos ypos [_, _, x, y]
  | abs(xint - xpos) < 20 && abs(yint - ypos) < 20 = True
  | otherwise = False
  where
    xint = if (checkNumber x) 
            then read x :: Double
            else 
              0
    yint = if (checkNumber y) 
            then read y :: Double
            else
              0
inRange _ _ [_] = False
inRange _ _ [] = False
inRange _ _ [_, _] = False
inRange _ _ [_, _, _] = False
inRange _ _ (_:_:_:_:_:_) = False


--strict file reading
hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h = hGetContents h >>= \s -> length s `seq` return s


readFileStrict :: FilePath -> IO String
readFileStrict name =  openFile name ReadMode >>= hGetContentsStrict
