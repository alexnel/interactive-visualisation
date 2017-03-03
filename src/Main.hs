module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Control.Monad (unless, when)
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.Environment
import System.IO
import Data.Char
import Data.List
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as C 
import Data.IORef
import System.IO.Unsafe 
import qualified Data.Map as Map
import Graphics.Gloss.Rendering
import Control.Concurrent (threadDelay)
import Control.Concurrent
import qualified Graphics.UI.GLUT as GLUT
import System.Process


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

--errorCallback :: G.ErrorCallback
--errorCallback err description = hPutStrLn stderr description


keyCallback :: G.KeyCallback
keyCallback window key _ action _
  | key == G.Key'Escape && action == G.KeyState'Pressed = G.setWindowShouldClose window True
  | key == G.Key'Space && action == G.KeyState'Pressed = putStrLn "space"
  | otherwise = G.setWindowShouldClose window False


charCallback :: G.CharCallback
charCallback _ codepoint = do
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
mouseButtonCallback window button action _ = do
  when (button == G.MouseButton'1 && action == G.MouseButtonState'Pressed) $ do
    (xpos, ypos) <- G.getCursorPos window 
    (nameFind,_) <- isNode xpos ypos
    --(connectionsRes, name) <- connections nameFind
    --when (button == G.MouseButton'1 && action == G.MouseButtonState'Pressed && result /= []) $ print result
    when (nameFind /= B.empty) $ putStrLn ("You clicked node " ++ (C.unpack nameFind))


----calculate the number of conections a node has
--connections :: String -> IO (Int, String)
--connections name = do
--  --contents <- readFileStrict "pos.txt"
--  --let dotplain = lines contents
--  --return ((countConnections 0 name dotplain), name)
--  if name == ""
--    then return (0, "")
--    else do
--      dotplain <- readIORef edges
--      let dotList = Map.toList dotplain
--      return ((countConnections 0 name dotList), name)


--countConnections :: Int -> String -> [(String,String)] -> Int
--countConnections acc name [] = 0
--countConnections acc name (x:[])
--  | name == a || name == b = (acc+1)
--  | otherwise = acc
--  where (a,b) = x
--countConnections acc name (x:xs)
--  | name == a || name == b = countConnections (acc+1) name xs
--  | otherwise = countConnections acc name xs
--  where (a,b) = x



scalingFactor :: Float
scalingFactor = 100.0


ab :: IORef (Map.Map Float Float)
ab = unsafePerformIO $ newIORef Map.empty


nodes :: IORef (Map.Map B.ByteString (Float,Float))
nodes = unsafePerformIO $ newIORef Map.empty


--edges :: IORef (Map.Map B.ByteString B.ByteString)
--edges = unsafePerformIO $ newIORef Map.empty



main :: IO ()
main = do 

  args <- getArgs
  let filenameRead = args!!1
  let format = args!!0
  let filenamePlain = filenameRead ++ ".plain"
  let filenamePNG = filenameRead ++ ".png"
  let command1 = format ++ " -Tplain -O " ++ filenameRead
  let command2 = format ++ " -Tpng -O " ++ filenameRead


  --generate image from dot file
  _ <- createProcess (shell command1)
  _ <- createProcess (shell command2)
  threadDelay 40000






  --initialisations
  readfile filenamePlain
  GLUT.exit
  (_,_) <- GLUT.getArgsAndInitialize
  --G.setErrorCallback (Just errorCallback)
  successfulInit <- G.init
  glossState <- initState

  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
      [(a,b)] <- getAB
      mw <- G.createWindow (round (a*scalingFactor)) (round (b*scalingFactor)) "Node finder" Nothing Nothing
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




mainLoop :: G.Window -> State -> String -> IO ()
mainLoop w glossState filenamePNG = unless' (G.windowShouldClose w) $ do

    
    _ <- forkIO $ G.pollEvents

    loadJuicyPNG filenamePNG >>= maybe (putStrLn "Couldn't load file") displayPic 

    G.swapBuffers w
    
    threadDelay 20000
    mainLoop w glossState filenamePNG


--read the original .plain file and extract relivant information
readfile :: FilePath -> IO ()
readfile x = do
  contents <- openFile x ReadMode >>= B.hGetContents
  let dotplain = C.splitWith (=='\n') contents
  store dotplain


--iterate through list and save data
store :: [B.ByteString] -> IO ()
store [] = putStrLn "done storing nodes"
store [_] = putStrLn "done storing nodes"
store (x:xs) = do
  save x
  store xs


--test for what information the line is giving and write to file
save :: B.ByteString -> IO ()
save x
  | (C.pack "graph") `elem` allLines = initialiseFile (allLines!!2) (allLines!!3)
  | (C.pack "node") `elem` allLines = addNode (allLines!!1) (allLines!!2) (allLines!!3)
--  | (C.pack "edge") `elem` allLines = addConnection (allLines!!1) (allLines!!2)
  | otherwise = putStrLn "ignore edges"
  where allLines = C.split ' ' x


--input the original size of the image from the .plain file--
initialiseFile :: B.ByteString -> B.ByteString -> IO ()
initialiseFile a b = do
  let aFloat = if (checkNumber (C.unpack a))
          then (read (C.unpack a)) :: Float
          else 
            0
  let bFloat = if (checkNumber (C.unpack b)) 
            then (read (C.unpack b)) :: Float
            else 
              0
  modifyIORef' ab (\m -> (Map.insert aFloat bFloat m))
  --writeFile "pos.txt" (a ++ " " ++ b ++ "\n")


--add a node information to the pos.txt file--
addNode :: B.ByteString -> B.ByteString -> B.ByteString -> IO ()
addNode name c d = do
  [(_,b)] <- getAB

  let cint = if (checkNumber (C.unpack c)) 
              then (read (C.unpack c)) :: Float
              else 
                0
  let dint = if (checkNumber (C.unpack d))
              then (read (C.unpack d)) :: Float
              else
                0
  let xpos = (cint*scalingFactor)
  let ypos = ((b-dint)*scalingFactor)
  modifyIORef' nodes (\m -> (Map.insert name (xpos,ypos) m))
  --appendFile "pos.txt" ("node " ++ name ++ " " ++ show xpos ++ " " ++ show ypos ++ "\n")

--check for if it is a double or int--
checkNumber :: String -> Bool 
checkNumber [] = True 
checkNumber (x:xs) = (isDigit x || x == '.') && checkNumber xs 


--retrieve the original size of the image from the .plain file now stored in the pop.txt file--
getAB :: IO [(Float,Float)]
getAB = do
  abVals <- readIORef ab
  let abList = Map.toList abVals
  return abList


----add edges to the pos.txt file--
--addConnection :: B.ByteString -> B.ByteString -> IO ()
--addConnection name1 name2 = do
--  modifyIORef' edges (\m -> (Map.insert name1 name2 m))
--  --appendFile "pos.txt" (name1 ++ " " ++ name2 ++ "\n")


--display the original image in the GLFW window--
displayPic :: Picture -> IO ()
displayPic p@(Bitmap width height _ _) = do
    glossState <- initState
    displayPicture (width, height) black glossState 1.0 $
      Pictures [p]
displayPic _ = error "only the Bitmap constructor should be used here"


--check if the cursor position is sitting on a node--
isNode :: Double -> Double -> IO (B.ByteString,(Float,Float))
isNode xpos ypos = do
  --contents <- readFileStrict "pos.txt"
  --let dotplain = lines contents
  --let splice = map (splitOn " ") dotplain
  --let nodes = map extractNode splice
  --let nodesBool = (map (inRange xpos ypos)) nodes
  --let index = elemIndex True nodesBool 
  --if index == Nothing
  --  then return []
  --  else return (nodes !!(extractMaybe index))
  dotplain <- readIORef nodes
  let dotList = Map.toList dotplain
  let nodesBool = (map (inRange xpos ypos)) dotList

  let index = elemIndex True nodesBool

  if index == Nothing
    then return (B.empty,(0.0,0.0))
    else return (dotList!!(extractMaybe index))


extractMaybe :: Maybe t -> t
extractMaybe (Just x) = x


--check if cursor position is within curtain range to node--
inRange :: Double -> Double -> (B.ByteString,(Float,Float)) -> Bool
inRange xpos ypos (_,(x,y))
  | abs(xint - xpos) < 20 && abs(yint-ypos) < 20 = True
  | otherwise = False
  where
    xint = realToFrac x :: Double
    yint = realToFrac y :: Double


----strict file reading
--hGetContentsStrict :: Handle -> IO B.ByteString
--hGetContentsStrict h = hGetContents h >>= \s -> B.length s `seq` return s


--readFileStrict :: FilePath -> IO B.ByteString
--readFileStrict name =  openFile name ReadMode >>= hGetContentsStrict
