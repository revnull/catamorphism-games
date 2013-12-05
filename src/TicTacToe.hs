
import qualified Data.Map as M
import Control.Monad
import Data.Maybe
import Data.List
import Game

newtype Board = Board (M.Map Int Player)
  deriving (Eq, Ord)

initialBoard = Board M.empty

moves :: Player -> Board -> [Board]
moves player (Board board) = do
    i <- [1..9]
    guard $ not $ M.member i board
    return $ Board $ M.insert i player board

winner :: Player -> Board -> Maybe Player
winner _ (Board b) = findWinner $ M.partition (== X) b
  where findWinner (xs, os) = maybeWinner X xs `mplus` maybeWinner O os
        maybeWinner w m = listToMaybe $ do
            seq <- filter ((3==) . length) $ subsequences $ M.keys m
            guard $ sum seq == 15
            return w

tictactoe = (moves, winner)

magic :: [Int]
magic = [4,9,2,3,5,7,8,1,6]

spaceToMagic :: Int -> Int
spaceToMagic x = fromJust $ M.lookup x spaceMagic
  where spaceMagic = M.fromList $ zip [1..] magic

magicToSpace :: Int -> Int
magicToSpace x = fromJust $ M.lookup x magicSpace
 where magicSpace = M.fromList $ zip magic [1..]

magicBoard :: Board -> Board
magicBoard (Board b) = Board $ M.mapKeys spaceToMagic b

unmagicBoard :: Board -> Board
unmagicBoard (Board b) = Board $ M.mapKeys magicToSpace b

magicWrap :: (Board -> Board) -> Board -> Board
magicWrap fn = unmagicBoard . fn . magicBoard

instance Show Board where
    show board = intercalate "-+-+-\n" $ do
        row <- [[7,8,9],[4,5,6],[1,2,3]]
        return $ intercalate "|" (fmap space row) ++ "\n"
      where space i = maybe (show i) show $ M.lookup i b
            (Board b) = unmagicBoard board

playerIO :: Player -> Board -> IO Board
playerIO player board = do
    let (Board b) = unmagicBoard board
    putStrLn $ "You are " ++ show player
    putStrLn $ show board
    i <- prompt validMove "Enter a move: " "Invalid move number"
    return $ magicBoard $ Board $ M.insert i player b
  where (Board b) = unmagicBoard board
        validMove x = x > 0 && x <= 10 && (not $ M.member x b)

main = playGame tictactoe initialBoard playerIO 

