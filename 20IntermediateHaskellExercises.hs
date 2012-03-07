-- Taken from blog post http://blog.tmorris.net/20-intermediate-haskell-exercises/

class Fluffy f where
  furry :: (a -> b) -> f a -> f b
 
-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  -- furry :: (a -> b) -> [a] -> [b]
  furry = map
 
-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  -- furry :: (a -> b) -> Maybe a -> Maybe b
  furry f (Just x) = Just $ f x
  furry _ Nothing  = Nothing
 
-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  -- furry :: (a -> b) -> (t -> a) -> t -> b
  furry f g = f . g
 
newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)
 
-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  -- furry :: (a -> b) -> EitherLeft t a -> EitherLeft t b
  furry _ (EitherLeft (Right x)) = EitherLeft $ Right x
  furry f (EitherLeft (Left x))  = EitherLeft $ Left $ f x
 
-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  -- furry :: (a -> b) -> EitherRight t a -> EitherRight t b
  furry f (EitherRight (Right x)) = EitherRight $ Right $ f x
  furry _ (EitherRight (Left x))  = EitherRight $ Left x
 
class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana $ \x ->
              unicorn $ f x
 
-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  -- banana :: (a -> [b]) -> [a] -> [b]
  banana f = concat . (map f)
  -- unicorn :: a -> [a]
  unicorn = (:[])
 
-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  -- banana :: (a -> Maybe b) -> Maybe a -> Maybe b
  banana f (Just x) = f x
  banana _ Nothing  = Nothing
  -- unicorn :: a -> Maybe a
  unicorn = Just
 
-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  -- banana :: (a -> t -> b) -> (t -> a) -> t -> b
  banana f g x = f (g x) x
  -- unicorn :: a -> t -> a
  unicorn = \x _ -> x
 
-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  -- (a -> EitherLeft t b) -> EitherLeft t a -> EitherLeft t b
  banana f (EitherLeft (Left x))  = f x
  banana _ (EitherLeft (Right x)) = EitherLeft $ Right x
  -- a -> EitherLeft t a
  unicorn = EitherLeft . Left
 
-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  -- (a -> EitherRight t b) -> EitherRight t a -> EitherRight t b
  banana _ (EitherRight (Left x))  = EitherRight $ Left x
  banana f (EitherRight (Right x)) = f x
  -- a -> EitherRight t a
  unicorn = EitherRight . Right
 
-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id
 
-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple x mf = banana (\f -> furry' f x) mf
 
-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy xs f = foldr (\x acc -> banana (\x' -> furry' (x':) acc) x) (unicorn []) (map f xs)
 
-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage xs = moppy xs id
 
-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f x y = apple y $ furry' f x
 
-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f x y w = apple w $ banana2 f x y
 
-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f x y w z = apple z $ banana3 f x y w
 
newtype State s a = State {
  state :: (s -> (s, a))
}
 
-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  -- furry :: (a -> b) -> State s a -> State s b
  furry f = \(State sf) ->
            State $ \s ->
                let
                    (s', x) = sf s
                in
                  (s', f x)
 
-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  -- banana :: (a -> State s b) -> State s a -> State s b
  banana f = \(State sf) ->
             State $ \s ->
                 let
                     (s', x)     = sf s
                     (State sf') = f x
                 in
                   sf' s'
  -- unicorn :: a -> State s a
  unicorn x = State $ \s -> (s, x)
