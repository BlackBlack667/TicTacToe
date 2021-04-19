module Main where
import Data.Either
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.Environment
import Control.Concurrent
import Data.ByteString as B
import Codec.BMP
import qualified Data.List as L

----Bad representation for changing colors. Needs redesign. Used by bmpChangeColor function.
--recolor :: (Float, Float, Float) -> BMP -> BMP
--recolor (rc, gc, bc) bmp@BMP {bmpRawImageData = raw} =
--    bmp {bmpRawImageData = B.pack $ process $ B.unpack raw} where
--    process (b:g:r:a:xs) = (mul b bc):(mul g gc):(mul r rc):a:process xs
--    process xs = xs
--   mul c cc = round $ cc * fromIntegral c


-- Screen variables

screenWidth :: Int
screenWidth = 800
screenHeight :: Int
screenHeight = 640
backgroundColor :: Color
backgroundColor = black
fps :: Int
fps = 60

-- Indexes for BMPs

squarePicture :: Int
squarePicture = 1
xPicture :: Int
xPicture = 2
oPicture :: Int
oPicture = 3
colorPlayer1 :: Int
colorPlayer1 = 4
colorPlayer2 :: Int
colorPlayer2 = 5

-- World types
data Cell = Cell Player | None deriving (Eq, Show)
data State = Running | GameOver Player deriving (Eq, Show)
data Player = Player1 | Player2 deriving (Eq, Show)
data Game = Game {
    gameBMP :: [Picture]
  , gameState :: State
  , gamePlayer :: Player
  , gameAnimate :: Picture
  , gameBoard :: Board
  , gameMadeTurn :: Bool
  , gameN :: Int
  , gameProduct :: Float
  , gameTileBound :: Int
  } 
type Board = [(Int,Cell)]

-- Window operations

window :: String -> (Int, Int) -> Display
window programName (x,y) = InWindow programName (x, y) (0, 0)

-- Initial world - Beginning of game.

initialWorld:: [Picture] -> Int -> Game
initialWorld pictures n = Game {
    gameBMP = pictures
  , gameState = Running
  , gamePlayer = Player1
  , gameAnimate = Blank
  , gameBoard = L.zip [1..(gameN (initialWorld [Blank] n))*(gameN (initialWorld [Blank] n))] (L.take ((gameN (initialWorld [Blank] n))*(gameN (initialWorld [Blank] n))) (repeat None))
  , gameMadeTurn = False
  , gameN = n
  , gameProduct = (fromIntegral 3) / (fromIntegral n)
  , gameTileBound = 200 -- Normal bound for 3x3 Board
  }

-- Lists operations

filterPicture       :: [(Int, Cell)] -> Player -> Int -> [Int]
player1CellsOfBoard :: Board -> [Picture] -> Int -> Int -> State -> Picture
player2CellsOfBoard :: Board -> [Picture] -> Int -> Int -> State -> Picture
netOnBoard          :: Board -> [Picture] -> Int -> Int -> Picture
setWinnerNetOnBoard :: Picture -> Int -> Int -> Picture

filterPicture xs player n = [n | (n,x) <- xs, x == Cell player]

findPictures player listOfPictures n bound state = (\x -> Translate (fromIntegral ((x-1) `mod` n) * bound) (fromIntegral ((x-1) `div` n)*(-bound)) picturePlayerList)
 where picturePlayerList = pictures ([listOfPictures!!(if player == Player1 then 2 else 3)] <> (if state == Running then [listOfPictures!!(if player == Player1 then 4 else 5)] else [Blank])   )
player1CellsOfBoard board listOfPictures n bound state = pictures $ L.map finding filtering
 where filtering = filterPicture board Player1 n
       finding = findPictures Player1 listOfPictures n (fromIntegral bound) state
player2CellsOfBoard board listOfPictures n bound state = pictures $ L.map finding filtering
 where filtering = filterPicture board Player2 n
       finding = findPictures Player2 listOfPictures n (fromIntegral bound) state
netOnBoard board listOfPictures n bound = pictures $ (L.map (\z -> Translate (xParam z) (yParam z) (listOfPictures!!squarePicture)) [1..n*n])
 where xParam z = fromIntegral $ (fromIntegral ((z-1)`mod`n)) * bound
       yParam z = fromIntegral $ (fromIntegral $ floor (fromIntegral ( (z+(n-1)) `div` (fromIntegral n)) )-(fromIntegral 1))*(-bound)
setWinnerNetOnBoard bmp n bound = pictures $ L.map (\z -> Translate (xParam z) (yParam z) bmp ) [1..n*n]
 where xParam z = fromIntegral $ fromIntegral ((z-1)`mod`n)*bound
       yParam z = fromIntegral $ floor $ fromIntegral ((( fromIntegral ( fromIntegral z + (n-1) ) `div` n )-(fromIntegral 1) ) * (-bound))

-- Render operations

translateWholeWorldPicture2 (x,y) n = Translate ((fromIntegral n)*(-100)) ((fromIntegral n)*100)
buildWholePicture :: [Picture] -> Int -> Picture
buildWholePicture bmps n = Scale ((fromIntegral 3) / (fromIntegral n)) ((fromIntegral 3) / (fromIntegral n)) (pictures bmps)
translateWholeWorldPicture (x,y) = Translate 100 (-100)

isGameOver :: Game -> Bool
isGameOver world = checkLists
 where checkLists = checkX diagonalTable || checkO diagonalTable || checkX secondDiagonalTable || checkO secondDiagonalTable || checkMultipleX columnTable || checkMultipleO columnTable || checkMultipleX rowTable || checkMultipleO rowTable
       diagonalTable = [a | a <- [1, n+2..n*n]]
       secondDiagonalTable = [a | a <- [n, n+n-1..n*n], not (a == n*n)]
       columnTable = [a | b <- [1..n] ,a <- [b, b+n..n*n]]
       rowTable  = [a | b <- [1, 1+n..n*n], a <- [b..b+n-1]]
       checkX = isFull world (Cell Player1)
       checkO = isFull world (Cell Player2)
       checkMultipleX = isFulls world (Cell Player1)
       checkMultipleO = isFulls world (Cell Player2)
       n = gameN world

renderGameOver :: Game -> Picture
renderGameOver world = setWinnerNetOnBoard (if winner == GameOver Player1 then bmp!!5 else bmp!!4) n bound
 where board = gameBoard world
       bmp = gameBMP world
       winner = gameState world
       n = gameN world
       bound = gameTileBound world

renderRunning :: Game -> [Picture]
renderRunning world = [
   (netOnBoard board bmps n bound)
 , (player1CellsOfBoard board bmps n bound state)
 , (player2CellsOfBoard board bmps n bound state) ]
 where board = gameBoard world
       bmps = gameBMP world
       n = gameN world
       bound = gameTileBound world
       state = gameState world

render :: (Int, Int) -> Game -> Picture
render (x,y) world = case gameState world of
 Running -> buildWholePicture (L.map (\z -> translateWholeWorldPicture2 (x,y) (gameN world) $ (translate1 z)) (renderRunning world) ) (gameN world)
  where translate1 z = translateWholeWorldPicture (x,y) z
 GameOver Player1 -> (buildWholePicture [translateWholeWorldPicture2 (x,y) (gameN world) (translateWholeWorldPicture (x,y) $ pictures $ (renderRunning world) <> [renderGameOver world] )] (gameN world))
 GameOver Player2 -> (buildWholePicture [translateWholeWorldPicture2 (x,y) (gameN world) (translateWholeWorldPicture (x,y) $ pictures $ (renderRunning world) <> [renderGameOver world] )] (gameN world))

-- Event operations (Mouse)

changePlayer :: Game -> Game    -- TODO: see line 175
changePlayer world = case gamePlayer world of
 Player1 -> if (gameMadeTurn world) == True then  world {gamePlayer = Player2, gameMadeTurn = False} else world
 Player2 -> if (gameMadeTurn world) == True then  world {gamePlayer = Player1, gameMadeTurn = False} else world

mousePosToInt :: (Float, Float) -> (Int, Int)
mousePosToInt (x, y) = (floor x+300, floor y+300)

oneTileCheck :: (Int,Int) -> Board -> Int -> Int -> Int -> Bool
oneTileCheck (x,y) board z bound n = (x >= bound*(((z-1)`mod`n))) && (x <= bound+bound*((z-1)`mod`n)) && ( y-floor (600 * (fromIntegral n/3)) <= (-bound)*((z-1)`div`n)) && (  y-floor (600 * (fromIntegral n/3)) >= (-bound)-bound*((z-1)`div`n)) && (((snd (board!!(z-1)) ) == None))

listTileCheck :: (Int, Int) -> Board -> Int -> Int -> Float -> Bool
listTileCheck (x,y) board n bound scale = or $ (L.map (oneTileCheck (x, y) board (bound) n) [1 .. n*n])

oneTileFind :: (Int,Int) -> Board -> Int -> Int -> Int
oneTileFind (x,y) board n bound = L.sum $ L.map (\z -> if oneTileCheck (floor (fromIntegral x/(3/(fromIntegral n))),floor (fromIntegral y/(3/(fromIntegral n)))) board z bound n  then z else 0) [1..n*n]

transferChange :: Player -> (Int, Cell) -> (Int, Cell)
transferChange z (x, y) = (x, (Cell z))

isRightCordinate :: (Int, Int) -> Board -> Int -> Int -> Int -> Bool  -- broken function for elimaniting bug. TODO: repair it
isRightCordinate (x,y) board bound z n = if (x >= bound*(((z-1)`mod`n))) && (x <= bound+bound*((z-1)`mod`n)) && (y-600 <= (-bound)*((z-1)`div`n)) && (y-600 >= (-bound)-bound*((z-1)`div`n)) then True else False

isOpposite :: (Int, Int) -> Game -> Int -> Bool -- TODO: see line 175
isOpposite (x,y) world 0 = False
isOpposite (x,y) world n = if conditions then True else (isOpposite (x,y) world (n-1))
 where board = gameBoard world
       bound = gameTileBound world
       player = gamePlayer world
       conditions = (isRightCordinate (floor (fromIntegral x/(3/(fromIntegral n))),floor (fromIntegral y/(3/(fromIntegral n)))) board (floor(fromIntegral bound * ((fromIntegral 3)/(fromIntegral n)))) n (gameN world)) && (snd (board!!(n-1)) /= None)
       

playerTurn :: Game -> (Int, Int) -> Game
playerTurn world (x,y) = if isBasicRequirements then (world {gameBoard = gameBoardAdd, gameMadeTurn = True}) else world
 where gameBoardAdd = L.map (\(a,b) -> if a == tilefind then transferChange player (a,b) else (a,b)) board
       isBasicRequirements = (x >= 0 && x <= 600 && y >= 0 && y <= 600) -- && (not $ isOpposite (x,y) world (n*n)) -- not usable for now -- && listTileCheck (x,y) board n bound (gameProduct world)
       tilefind = oneTileFind (x,y) board n bound
       player = gamePlayer world
       board = gameBoard world
       n = gameN world
       bound = gameTileBound world

-- Logic
isAny :: Game -> Cell -> [Int] -> Bool
isAny world cell which = or $ L.map (\z -> if (snd (board!!(z-1)) == cell) then True else False) which
 where board = gameBoard world
isFulls :: Game -> Cell -> [Int]  -> Bool
isFulls world cell which = or $ L.map (isFull world cell) (L.init (L.map (\z -> L.take n $ L.drop z which) [0, n .. n*n]))
 where n = gameN world
isFull :: Game -> Cell -> [Int] -> Bool
isFull world cell which = and $ L.map (\z -> if (snd (board!!(z-1)) == cell) then True else False) which
 where board = gameBoard world
       n = gameN world

logic (EventKey (MouseButton LeftButton) Up _ mousePos) world = case gameState world of
 GameOver _ -> initialWorld (gameBMP world) (gameN world)
 Running -> (if isGameOver world then world {gameState = GameOver (gamePlayer world)} else continueRender)
  where continueRender = if True then changePlayer (playerTurn world $ mousePosToInt mousePos) else undefined
logic (EventKey (Char 'r') Up _ mousePos) world = initialWorld (gameBMP world) (gameN world)
logic (EventKey (SpecialKey KeyUp) Up _ mousePos) world = initialWorld (gameBMP world) ((gameN world)+1)
logic (EventKey (SpecialKey KeyDown) Up _ mousePos) world = if (gameN world) == 2 then world else initialWorld (gameBMP world) ((gameN world)-1)
logic _ world = if isGameOver world then world {gameState = GameOver (gamePlayer world)} else world

-- Animation - not implemented yet

animation' = const id

-- Operating on BMPs , scaling pictures in upcoming feature

bmpChangeColor :: [BMP] -> [BMP]
bmpChangeColor bmps = bmps

bmpConvertToPicture :: [BMP] -> IO [Picture]
bmpConvertToPicture bmps = return $ L.map (\x -> bitmapOfBMP x) bmps

-- IO Action

main :: IO ()
main = do 
       Right x1 <- readBMP "nothing.bmp"
       Right x2 <- readBMP "squarepicture.bmp"
       Right x3 <- readBMP "xpicture.bmp"
       Right x4 <- readBMP "opicture.bmp"
       Right x5 <- readBMP "colorplayer1.bmp"
       Right x6 <- readBMP "colorplayer2.bmp"
       (x,y) <- getScreenSize
       xs <- bmpConvertToPicture (bmpChangeColor $ x1:x2:x3:x4:x5:x6:[])
       n <- pure 3
       play (window "TicTacToe" (x,y)) backgroundColor fps (initialWorld xs n) (render (x,y)) logic animation'
