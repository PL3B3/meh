module Game (
            ) where

import LinearAlgebra as L

data Network = Network Int

type Player = ((L.Tensor Double -> [Double]), Double)

{-


-}

next_turn :: [Player] -> L.Tensor Double -> ([Player], L.Tensor Double)
next_turn players gameboard@(L.Tensor shape info) = (new_players, new_gameboard)
  where new_players = foldl (\ls dex -> ls ++ (fst (player !! dex), (snd (player !! dex)) + (sum $ zipWith (score) (int_decision_list !! dex) (map (!! dex) (except dex int_decision_list))))) [] (ps num_players)
        num__players = length players
        --num_decisions = div (length decision_list) (num_players ^ 2)
        new_gameboard = sd [1,0] $ L.makeTensor (1 + shape !! 1, shape !! 0) ((infoOf (sd [1,0] gameboard)) ++ (concat decision_list))
        decision_list = foldl (\a b -> a ++ [(b gameboard)]) [] (map fst players)
        int_decision_list = map (map round_sigmoid) decision_list


except :: Int -> [a] -> [a]
except dex ls = let split = splitAt dex ls in (fst split) ++ (tail $ snd split)


ps a = [0..(pred a)]

round_sigmoid :: Double -> Int
round_sigmoid dub = fromEnum (dub > 0.5)

score :: Int -> Int -> Int
score ourdecision theirdecision
  | ourdecision == 0 = if theirdecision == 0 then 4 else -4
  | otherwise = if theirdecision == 0 then 10 else 0
