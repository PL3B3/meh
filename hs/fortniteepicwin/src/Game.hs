module Game (
  Network,
  Player,
  game,
  game2,
  next_turn,
  except,
  newturn,
  ps,
  round_sigmoid,
  score
            ) where

import LinearAlgebra as L

data Network = Network Int

type Player = ((L.Tensor Double -> [Double]), Int)

game :: [Player] -> L.Tensor Double -> Int -> ([Player], L.Tensor Double)
game preteenbattleroyalecannonfodder gameboard turns = if turns == 1 then next_turn preteenbattleroyalecannonfodder gameboard else game (fst nextgen) (snd nextgen) (pred turns)
  where nextgen = next_turn preteenbattleroyalecannonfodder gameboard

game2 p g t = foldl (\a@(bu,zu) b -> next_turn bu zu) (p, g) [1..t]

--cols = players, rows = turns
--corrected player referencing
newturn :: ([Player], L.Tensor Double) -> ([Player], L.Tensor Double)
newturn prev@(p,g@(L.Tensor s i)) = (nps, ngb)
  where nps = 
        ngb = L.makeTensor [s !! 0 + 1, s !! 1] (i ++ dcs)
        cnt = length p
        dcs = foldl (\a b -> a ++ [b (L.get_row gameboard (pred $ s !! 0))]) [] (map fst players)
        dci = map (map round_sigmoid) decision_list


next_turn :: [Player] -> L.Tensor Double -> ([Player], L.Tensor Double)
next_turn players gameboard@(L.Tensor shape info) = (new_players, new_gameboard)
  where new_players = foldl (\ls dex -> ls ++ [(fst (players !! dex), (snd (players !! dex)) + (sum $ zipWith (score) (int_decision_list !! dex) (map (!! dex) (except dex int_decision_list)))) :: Player]) [] (ps num_players)
        num_players = length players
        --num_decisions = div (length decision_list) (num_players ^ 2)
--        new_gameboard = sd (L.makeTensor [1 + shape !! 1, shape !! 0] ((infoOf (sd gameboard [1,0])) ++ (concat decision_list))) [1,0]
        new_gameboard = (L.makeTensor [1 + shape !! 1, shape !! 0] ((infoOf (sd gameboard [1,0])) ++ (concat decision_list)))
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
