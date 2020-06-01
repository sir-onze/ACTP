{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import DurationMonad

-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)
-- Adventurers + the lantern
type Objects = Either Adventurer ()

-- The time that each adventurer needs to cross the bridge
-- To implement 
getTimeAdv :: Adventurer -> Int
getTimeAdv P1 = 1
getTimeAdv P2 = 2
getTimeAdv P5 = 5
getTimeAdv P10 = 10


{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state
of the game, with all adventurers and the lantern on the left side of
the bridge. Similarly, the function (const True) represents the end
state of the game, with all adventurers and the lantern on the right
side of the bridge.  --}
type State = Objects -> Bool

instance Show State where
  show s = (show . (fmap show)) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (Right ())]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]



-- The initial state of the game
gInit :: State
gInit = const False

test :: State
test (Left P1) = True
test (Left P2) = True
test (Left P5) = True
test (Left P10) = False 
test (Right ()) = True


-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)

-- Changes the state of the game of a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os


-- Gets the adventurers that can grab the lantern to cross the bridge
validAdv :: [Adventurer] -> State -> [Adventurer]
validAdv [] _ = []
validAdv (h:t) s = if s (Left h) == s (Right ()) then h : validAdv t s else validAdv t s

-- Forms a combination of 2 Adventurers from a list of Objects
combine :: Int -> [Adventurer] -> [[Adventurer]]
combine 0 _ = [[]]
combine _ [] = []
combine n (x:xs) = map (x :) (combine (n-1) xs) ++ combine n xs


-- Creates a list of Durations from a state and List of List of adventurers
dList :: State -> [[Adventurer]] -> [Duration State]
dList _ [] = []
dList s (x:xs)
   | length x == 1 =  
      [wait (getTimeAdv(x !! 0)) $ return $ mChangeState [Left (x !! 0), Right ()] s] ++ dList s xs
   | length x == 2 =
      [wait (max (getTimeAdv (x !! 0)) (getTimeAdv(x !! 1))) $ return $ mChangeState [Left (x !! 0), Left (x !! 1), Right ()] s] ++ dList s xs


{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
-- To implement
allValidPlays :: State -> ListDur State
allValidPlays s = let x = validAdv [P1,P2,P5,P10] s in LD (dList s (combine 2 x ++ combine 1 x))

{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
-- To implement
exec :: Int -> State -> ListDur State
exec 0 s = return s
exec n s = do s1 <- allValidPlays s
              exec (n-1) s1

{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement
leq17 :: Bool
leq17 = length x > 0 
         where y = map (\x -> (getDuration x, getValue x)) $ remLD (exec 5 gInit)
               x = filter (\x -> fst x <= 17 && snd x == const True) y
{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- To implement
l17 :: Bool
l17 = length x > 0 
         where y = map (\x -> (getDuration x, getValue x)) $ remLD (exec 5 gInit)
               x = filter (\x -> fst x < 17 && snd x == const True) y


--------------------------------------------------------------------------
{-- Implementation of the monad used for the problem of the adventurers.
Recall the Knight's quest --}

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

-- To implement
instance Functor ListDur where
   fmap f = let f' = \x -> Duration(getDuration x, f (getValue x)) in
      LD . (map f') . remLD

-- To implement
instance Applicative ListDur where
   pure x = LD [Duration (0,x)]
   l1 <*> l2 = LD $ do x <- remLD l1
                       y <- remLD l2
                       g(x,y) where
                          g(Duration(d,f),Duration(d',x)) = return (Duration(d+d',f x))

-- To implement
instance Monad ListDur where
   return = pure
   l >>= k = LD $ do x <- remLD l
                     g x where
                        g(Duration(d,x)) = let u = (remLD (k x)) in map (\x -> Duration(getDuration x + d,getValue x)) u

manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)
--------------------------------------------------------------------------
