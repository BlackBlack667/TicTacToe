module Window (window, fps, backgroundColor) where
import Graphics.Gloss.Data.Color
import Graphics.Gloss

fps :: Int
fps = 60

backgroundColor :: Color
backgroundColor = black

window :: String -> (Int, Int) -> Display
window programName (x,y) = InWindow programName (x, y) (0, 0)
