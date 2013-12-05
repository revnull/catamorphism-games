
import Game
import Data.Maybe
import Control.Monad
import qualified Data.Map as M

newtype NimBoard = NimBoard (M.Map Int Int)
  deriving (Eq, Ord)

nimWinner :: Player -> NimBoard -> Maybe Player
nimWinner player (NimBoard board) = do
    guard $ all (== 0) $ M.elems board
    return $ player

nimMoves :: Player -> NimBoard -> [NimBoard]
nimMoves _ (NimBoard board) = do
    (heap, size) <- M.toList board
    i <- [0..size - 1]
    return $ NimBoard $ M.insert heap i board

nim = (nimMoves, nimWinner)

initialBoard :: NimBoard
initialBoard = NimBoard $ M.fromList [(1,3),(2,5),(3,7)]

instance Show NimBoard where
    show (NimBoard board) = do
        (heap, size) <- M.toAscList board
        show heap ++ ": " ++ concat ["*" | _ <- [1..size]] ++ "\n"

getPlayerMove :: Player -> NimBoard -> IO NimBoard
getPlayerMove p n@(NimBoard b) = do
    putStrLn $ "You are " ++ show p
    putStrLn $ show n
    h <- prompt (isJust . heapSize) "Enter a heap: " "Invalid heap number"
    let (Just size) = heapSize h
    i <- prompt (inRange size) "Enter a count: " "Invalid count"
    return $ NimBoard $ M.insert h (size - i) b
  where heapSize h = do
            size <- M.lookup h b
            guard $ size > 0
            return size
        inRange size x = x > 0 && x <= size

main = playGame nim initialBoard getPlayerMove

