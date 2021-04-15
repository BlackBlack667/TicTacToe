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

-- Board variable

n :: Int
n = 3

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
  } 
type Board = [(Int,Cell)]

-- Window operation

window :: String -> (Int, Int) -> Display
window programName (x,y) = InWindow programName (x, y) (0, 0)

-- Initial world - in the beginning of game.

initialWorld:: [Picture] -> Game
initialWorld pictures = Game {
    gameBMP = pictures
  , gameState = Running
  , gamePlayer = Player1
  , gameAnimate = Blank
  , gameBoard = L.zip [1..n*n] (L.take (n*n) (repeat None))
  , gameMadeTurn = False
  }

-- Lists operations

filterPicture       :: [(Int, Cell)] -> Player -> [Int]
player1CellsOfBoard :: Board -> [Picture] -> Picture
player2CellsOfBoard :: Board -> [Picture] -> Picture
netOnBoard          :: Board -> [Picture] ->  Picture
setWinnerNetOnBoard :: Picture -> Picture

filterPicture xs player = [n | (n,x) <- xs, x == Cell player]

findPictures player listOfPictures = (\x -> Translate (fromIntegral ((x-1) `mod` 3) * 200) (fromIntegral ((x-1) `div` 3)*(-200)) picturePlayerList)
 where picturePlayerList = pictures ([listOfPictures!!(if player == Player1 then 2 else 3)] <> [listOfPictures!!(if player == Player1 then 4 else 5)])
player1CellsOfBoard board listOfPictures = pictures $ L.map finding filtering
 where filtering = filterPicture board Player1
       finding = findPictures Player1 listOfPictures
player2CellsOfBoard board listOfPictures = pictures $ L.map finding filtering
 where filtering = filterPicture board Player2
       finding = findPictures Player2 listOfPictures
netOnBoard board listOfPictures = pictures $ L.map (\x -> Translate (fromIntegral ((x-1)`mod`3)*200)  ((fromIntegral $ floor ( ( fromIntegral x+2 ) / 3 )-(fromIntegral 1 ) ) * (-200)) (listOfPictures!!squarePicture) ) [1..n*n]
setWinnerNetOnBoard bmp = pictures $ L.map (\x -> Translate (fromIntegral ((x-1)`mod`3)*200)  ((fromIntegral $ floor ( ( fromIntegral x+2 ) / 3 )-(fromIntegral 1 ) ) * (-200)) bmp ) [1..n*n]

-- Render operations

buildWholePicture :: [Picture] -> Picture
buildWholePicture bmps = pictures bmps

translateWholeWorldPicture (x,y) = Translate (-200) (200)

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

renderGameOver :: Game -> Picture
renderGameOver world = setWinnerNetOnBoard (if winner == GameOver Player1 then bmp!!5 else bmp!!4)
 where board = gameBoard world
       bmp = gameBMP world
       winner = gameState world

renderRunning :: Game -> [Picture]
renderRunning world = [
   (netOnBoard board bmps)
 , (player1CellsOfBoard board bmps)
 , (player2CellsOfBoard board bmps) ]
 where board = gameBoard world
       bmps = gameBMP world

render :: (Int, Int) -> Game -> Picture
render (x,y) world = case gameState world of
 Running -> translateWholeWorldPicture (x,y) (buildWholePicture $ renderRunning world)
 GameOver Player1 -> translateWholeWorldPicture (x,y) (renderGameOver world)
 GameOver Player2 -> translateWholeWorldPicture (x,y) (renderGameOver world)

-- Event operations (Mouse)

changePlayer :: Game -> Game
changePlayer world = case gamePlayer world of
 Player1 -> if (gameMadeTurn world) == True then  world {gamePlayer = Player2, gameMadeTurn = False} else world
 Player2 -> if (gameMadeTurn world) == True then  world {gamePlayer = Player1, gameMadeTurn = False} else world

mousePosToInt :: (Float, Float) -> (Int, Int)
mousePosToInt (x, y) = (floor x+300, floor y+300)

oneTileCheck :: (Int,Int) -> Board -> Int -> Bool
oneTileCheck (x,y) board z = (x >= 200*(((z-1)`mod`3))) && (x <= 200+200*((z-1)`mod`3)) && (y-600 <= (-200)*((z-1)`div`3)) && (y - 600 >= -200-200*((z-1)`div`3)) && (((snd (board!!(z-1)) ) == None))

listTileCheck :: (Int, Int) -> Board -> Bool
listTileCheck mousePos board = or $ (L.map (oneTileCheck mousePos board) [1 .. n*n])

oneTileFind :: (Int,Int) -> Board -> Int
oneTileFind (x,y) board = L.sum $ L.map (\z -> if oneTileCheck (x,y) board z then z else 0) [1..n*n]

transferChange :: Player -> (Int, Cell) -> (Int, Cell)
transferChange z (x, y) = (x, (Cell z))

playerTurn :: Game -> (Int, Int) -> Game
playerTurn world (x,y) = if isBasicRequirements then (world {gameBoard = gameBoardAdd, gameMadeTurn = True}) else world
 where gameBoardAdd = L.map (\(a,b) -> if a == tilefind then transferChange player (a,b) else (a,b)) board
       isBasicRequirements = (x >= 0 && x <= 600 && y >= 0 && y <= 600) && listTileCheck (x,y) board
       tilefind = oneTileFind (x,y) board
       player = gamePlayer world
       board = gameBoard world

-- Logic
isAny :: Game -> Cell -> [Int] -> Bool
isAny world cell which = or $ L.map (\z -> if (snd (board!!(z-1)) == cell) then True else False) which
 where board = gameBoard world
isFulls :: Game -> Cell -> [Int] -> Bool
isFulls world cell which = or $ L.map (isFull world cell) (L.init (L.map (\z -> L.take n $ L.drop z which) [0, n .. n*n]))
isFull :: Game -> Cell -> [Int] -> Bool
isFull world cell which = and $ L.map (\z -> if (snd (board!!(z-1)) == cell) then True else False) which
 where board = gameBoard world

logic (EventKey (MouseButton LeftButton) Up _ mousePos) world = case gameState world of
 GameOver _ -> initialWorld (gameBMP world)
 Running -> (if isGameOver world then world {gameState = GameOver (gamePlayer world)} else continueRender)
  where continueRender = changePlayer (playerTurn world $ mousePosToInt mousePos) 
logic (EventKey (Char 'r') Up _ mousePos) world = initialWorld (gameBMP world)
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
       Right x1 <- readBMP "hero.bmp"
       Right x2 <- readBMP "squarepicture.bmp"
       Right x3 <- readBMP "xpicture.bmp"
       Right x4 <- readBMP "opicture.bmp"
       Right x5 <- readBMP "colorplayer1.bmp"
       Right x6 <- readBMP "colorplayer2.bmp"
       (x,y) <- getScreenSize
       xs <- bmpConvertToPicture (bmpChangeColor $ x1:x2:x3:x4:x5:x6:[])
       play (window "name" (x,y)) backgroundColor fps (initialWorld xs) (render (x,y)) logic animation'
