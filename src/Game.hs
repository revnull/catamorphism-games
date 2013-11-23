
module Game (Player(..), other, Game, GameTree, buildGameTree,
             xtoCount, minimax, playGame, prompt) where

import Data.Maybe
import Control.Monad

newtype Mu f = InF { outF :: f (Mu f) }

ana :: Functor f => (a -> f a) -> a -> Mu f
ana f = InF . fmap (ana f) . f

cata :: Functor f => (f a -> a) -> Mu f -> a
cata f = f . fmap (cata f) . outF

data Player = X | O
  deriving (Read, Show, Eq, Ord)

other :: Player -> Player
other X = O
other O = X

type Game s = (Player -> s -> [s], Player -> s -> Maybe Player)

data GameTreeF s a =
    Turn Player s [a]
  | Win Player s
  | Tie s

type GameTree s = Mu (GameTreeF s)

instance Functor (GameTreeF s) where
  fmap f (Turn p s subs) = Turn p s (fmap f subs)
  fmap f (Tie s) = Tie s
  fmap f (Win p s) = Win p s

percolateUp :: (a -> Bool) -> [a] -> [a]
percolateUp pred = percolateUp' id
  where percolateUp' fn [] = fn []
        percolateUp' fn (x:xs) = 
            if pred x
                then x:percolateUp' fn xs
                else percolateUp' (fn . (x:)) xs

buildGameTree :: Game s -> Player -> s -> GameTree s
buildGameTree (moves, winner) player board = ana tree (player, board)
  where tree (p, b) =
            case (winner p b, moves p b) of
                (Just x, _) -> Win x b
                (_, []) -> Tie b
                (_, plays) ->
                    let subPlays = [(other p, play) | play <- plays]
                        ordered = percolateUp (isJust . uncurry winner) subPlays
                    in Turn p b ordered

xtoCount :: GameTree s -> (Int, Int, Int)
xtoCount = cata counts
  where addAll (a,b,c) (x,y,z) = (a+x,b+y,c+z)
        counts (Win X _) = (1, 0, 0)
        counts (Win O _) = (0, 0, 1)
        counts (Tie _) = (0, 1, 0)
        counts (Turn _ _ subs) = foldr addAll (0, 0, 0) subs

data Outcome = Defeat | Boredom | Victory
  deriving (Read, Show, Eq, Ord)

minimax :: Game s -> Player -> s -> s
minimax game player = outcome . cata mm . buildGameTree game player
  where outcome (_, s) = s !! 1
        mm (Win p s)
            | player == p = (Victory, [s])
            | otherwise = (Defeat, [s])
        mm (Tie s) = (Boredom, [s])
        mm (Turn p s subs) =
            let fn = if p == player then maxGame else minGame
                (oc, ms) = fn subs
            in (oc, s:ms)
        maxGame (x:xs) = extreme Victory (>) x xs
        minGame (x:xs) = extreme Defeat (<) x xs
        extreme _ _ t1 [] = t1
        extreme bound fn t1@(x, _) (t2@(y,_):ts)
            | x == bound = t1
            | fn y x = extreme bound fn t2 ts
            | otherwise = extreme bound fn t1 ts

done :: Show s => Game s -> Player -> s -> IO Bool
done (moves, winner) player board =
  case (winner player board, moves player board) of
        (Just x, _) -> do
            putStrLn $ show board 
            putStrLn $ show x ++ " has won!"
            return True
        (_, []) -> do
            putStrLn $ show board 
            putStrLn "It is a tie!"
            return True
        otherwise -> return False

playGame :: Show s => Game s -> s -> (Player -> s -> IO s) -> IO ()
playGame game initial humanIO = do
    players <- getPlayers
    gameLoop players initial
  where getPlayers = do
            human <- prompt (const True) "Would you like to be X or O?"
                "Please enter X or O."
            return $ case human of
                X -> cycle [(X, humanIO), (O, computerIO)]
                O -> cycle [(X, computerIO), (O, humanIO)]
        gameLoop ((p,m):ps) b = do
            d <- done game p b
            unless d $ m p b >>= gameLoop ps
        computerIO p b = return $ minimax game p b

prompt :: Read a => (a -> Bool) -> String -> String -> IO a
prompt validator msg err = prompt'
  where prompt' = do
            putStrLn msg
            input <- reads `fmap` getLine
            case input of
                [(a, "")] -> if validator a 
                                then return a
                                else tryAgain
                otherwise -> tryAgain
        tryAgain = putStrLn err >> prompt'

