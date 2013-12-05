
module Game (Player(..), other, Game, GameTree, buildGameTree,
             xtoCount, minimax, playGame, prompt, unique) where

import Prelude hiding (lookup)

import Data.Maybe (fromJust)
import Control.Monad (unless)
import Data.Function (fix, on)
import Data.List (minimumBy, maximumBy)
import Data.Map (Map, member, lookup, insert, empty)
import Data.Foldable (Foldable, foldMap, toList)
import Data.Monoid (mempty)

newtype Mu f = InF { outF :: f (Mu f) }

ana :: Functor f => (a -> f a) -> a -> Mu f
ana f = InF . fmap (ana f) . f

cataF :: Functor f => ((f a -> a) -> Mu f -> a) -> (f a -> a) -> Mu f -> a
cataF rec f = f . fmap (rec f) . outF

cata :: Functor f => (f a -> a) -> Mu f -> a
cata = fix cataF

cataM :: (Ord (f ()), Foldable f, Functor f) => (f a -> a) -> Mu f -> a
cataM f mu =
    let memoized = fmap (cataF fn f) $ unique mu
        fn _ mu' = fromJust $ lookup (fmap (const ()) $ outF mu') memoized
    in fn f mu

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
  deriving (Eq, Ord)

type GameTree s = Mu (GameTreeF s)

instance Functor (GameTreeF s) where
    fmap f (Turn p s subs) = Turn p s (fmap f subs)
    fmap f (Tie s) = Tie s
    fmap f (Win p s) = Win p s

instance Foldable (GameTreeF s) where
    foldMap fn (Turn _ _ l) = foldMap fn l
    foldMap _ _ = mempty

unique :: (Ord (f ()), Functor f, Foldable f) => Mu f -> Map (f ()) (Mu f)
unique mu = cata unique' mu mu empty
  where unique' f mu m
          | member blank m = m
          | otherwise =
              let m' = insert blank mu m
                  fns = toList f
                  mus = toList $ outF mu
              in foldr ($) m' $ zipWith ($) fns mus
          where blank = fmap (const ()) f

buildGameTree :: Game s -> Player -> s -> GameTree s
buildGameTree (moves, winner) player board = ana tree (player, board)
  where tree (p, b) =
            case (winner p b, moves p b) of
                (Just x, _) -> Win x b
                (_, []) -> Tie b
                (_, plays) -> Turn p b [(other p, play) | play <- plays]

xtoCount :: Ord s => GameTree s -> (Int, Int, Int)
xtoCount = cataM counts
  where addAll (a,b,c) (x,y,z) = (a+x,b+y,c+z)
        counts (Win X _) = (1, 0, 0)
        counts (Win O _) = (0, 0, 1)
        counts (Tie _) = (0, 1, 0)
        counts (Turn _ _ subs) = foldr addAll (0, 0, 0) subs

data Outcome = Defeat | Boredom | Victory
  deriving (Read, Show, Eq, Ord)

minimax :: Ord s => Game s -> Player -> s -> s
minimax game player = outcome . cataM mm . buildGameTree game player
  where outcome (_, s) = s !! 1
        mm (Win p s)
            | player == p = (Victory, [s])
            | otherwise = (Defeat, [s])
        mm (Tie s) = (Boredom, [s])
        mm (Turn p s subs) =
            let fn = if p == player then maxGame else minGame
                (oc, ms) = fn subs
            in (oc, s:ms)
        maxGame = maximumBy (compare `on` fst)
        minGame = minimumBy (compare `on` fst)

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

playGame :: (Ord s, Show s) => Game s -> s -> (Player -> s -> IO s) -> IO ()
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

