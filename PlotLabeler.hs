module Main where

import Data.List
import Debug.Trace
import Graphics.Gloss.Game
import System.IO.Unsafe

data Label = Nominal | Anomaly deriving (Eq)

instance Show Label where
  show Nominal = "0"
  show Anomaly = "1"

data State = State {
  window :: (Float, Float), -- size of the window
  selection :: Maybe (Float, Float, Label), -- selection around mouse
  info :: [((Float, Float), (Float, Label))] -- data
} deriving (Show, Eq)

parseLine :: String -> (String, Float)
parseLine s = (date, vals)
  where
    date = takeWhile (/= ',') s
    vals = read . tail . dropWhile (/= ',') $ s

addUpDates :: [(String, Float)] -> [(String, Float)]
addUpDates =
  map squash
  . groupBy (\a b -> fst a == fst b)
  . sortBy (\a b -> fst a `compare` fst b)
  where
    squash s = (fst $ head s, sum $ map snd s)

myShow :: (String, Float) -> String
myShow (k, v) = k ++ "," ++ show v

windowWidth = 1000 :: Int
windowHeight = 500 :: Int

main :: IO ()
main = do
  contents <- lines <$> readFile "mydata.csv"
  let startingState = newState . map snd . addUpDates . map parseLine $ contents
  play display backgroundColor fps startingState render eventHandler [update]
  where
    display = InWindow "Plot Labeller" (windowWidth, windowHeight) (0, 0)
    backgroundColor = white
    fps = 30

range = 15 -- labelling range of the mouse when pressed

newState :: [Float] -> State
newState =
  State (fromIntegral windowWidth, fromIntegral windowHeight) Nothing
  . map (\i -> ((0,0), (i, Nominal)))

render :: State -> Picture
render (State (width, height) sr ls) =
  translate (-width/2) (-height/2)
  $ pictures
  $ (line coords : map drawPoint coordsAndLabels ++ [selection])
  where
    drawPoint (x, y, l) = translate x y $ typeColor l $ circleSolid 3.0
    coordsAndLabels = map (\((x, y), (_, l)) -> (x, y, l)) ls
    coords = map fst ls
    selection = maybe
      blank
      (\(x, y, l) -> translate x y $ typeColor l $ circle range)
      sr
    typeColor Nominal = color blue
    typeColor Anomaly = color red

zipWithIndex :: [a] -> [(Float, a)]
zipWithIndex = zip (map fromIntegral [0 ..])

mapTo :: Float -> Float -> Float -> Float -> Float -> Float
mapTo min1 max1 min2 max2 v =
  min2 + (max2 - min2) * ((v - min1) / (max1 - min1))

eventHandler :: Event -> State -> State
eventHandler (EventResize (nx, ny)) s =
  s {window = (fromIntegral nx, fromIntegral ny)}
eventHandler (EventKey (Char 's') Down _ _) s =
  unsafePerformIO $ do
    writeFile "output.csv" $ infoToOutput s
    return $ trace "Data Saved" s
eventHandler (EventKey (MouseButton b) Down _ (mx, my)) s = s {selection = sel}
  where
    (w, h) = window s

    buttonToLabel LeftButton = Anomaly
    buttonToLabel _ = Nominal

    sel = Just (mx + w/2, my + h/2, buttonToLabel b)
eventHandler (EventMotion (x, y)) s = s {selection = sel}
  where
    (w, h) = window s
    sel = fmap (\(_, _, l) -> (x + w/2, y + h/2, l)) $ selection s
eventHandler (EventKey _ Up _ p) s = s {selection = Nothing}

update :: Float -> State -> State
update _ s@(State (width, height) selection dat) =
  s {info = updated}
  where
    updated = map rule myData
    rule (p, (v, l)) = maybe (p, (v, l)) idk selection
      where
        idk (x, y, sl) =
          if distance (x, y) p < range
          then (p, (v, sl))
          else (p, (v, l))

    myData = zip coords $ map snd dat
    coords = map remap $ zipWithIndex values
    values = map (fst . snd) dat

    remap (x, y) = (remapX x, remapY y)

    remapY y = mapTo minValue maxValue 0 height y
    minValue = discount 0.05 $ minimum values
    maxValue = addcount 0.05 $ maximum values

    remapX x = mapTo 0 endIndex 0 width x
    endIndex = fromIntegral $ length values

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

infoToOutput :: State -> String
infoToOutput = unlines . map (\(v, l) -> show l) . map snd . info

addcount :: Float -> Float -> Float
addcount p a = a + a * p

discount :: Float -> Float -> Float
discount p a = a - a * p
