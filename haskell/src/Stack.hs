{-# LANGUAGE InstanceSigs #-}

module Stack where

newtype Stack a = Stack [a] deriving (Show, Eq)

initStack :: Stack a
initStack = Stack []

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack s) = case s of
    [] -> (Nothing, Stack [])
    as -> (Just (last as), Stack $ init as)

push :: a -> Stack a -> Stack a
push a (Stack s) = Stack $ s ++ [a]

peek :: Int -> Stack a -> Maybe a
peek i (Stack s) = case drop i $ reverse s of
    [] -> Nothing
    (x:xs) -> Just x

peekTop :: Stack a -> Maybe a
peekTop = peek 0 

adjustStack :: Int -> (a -> a) -> Stack a -> Stack a
adjustStack i f (Stack s) = if length s <= i 
    then Stack s else
        let (xs, y : ys) = splitAt i $ reverse s
        in Stack $ reverse $ xs ++ (f y) : ys
        
stackSize :: Stack a -> Int
stackSize s = go s 0
    where
        go :: Stack a -> Int -> Int
        go stack count = case pop stack of
            (Nothing, _) -> count
            (_, stack') -> go stack' (count + 1)

toList :: Stack a -> [a]
toList (Stack as) = as

instance Foldable Stack where
    foldMap :: Monoid m => (a -> m) -> Stack a -> m
    foldMap f (Stack as) = foldMap f as
