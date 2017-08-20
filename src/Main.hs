module Main where


type Size = Int


(<>) = mappend


class Monoid v => Measured a v where
    measure :: a -> v


data Tree v a = Leaf   v a
              | Branch v (Tree v a) (Tree v a)


toList :: Tree v a -> [a]
toList (Leaf _ a)     = [a]
toList (Branch _ x y) = toList x ++ toList y


tag :: Tree v a -> v
tag (Leaf v _)     = v
tag (Branch v _ _) = v


leaf :: Measured a v => a -> Tree v a
leaf a = Leaf (measure a) a


branch :: Monoid v => Tree v a -> Tree v a -> Tree v a
branch x y = Branch (tag x <> tag y) x y


(!!) :: Tree Size a -> Int -> a
(Leaf _ a)      !! 0 = a
(Branch _ x y)  !! n
     | n < tag x     = x !! n
     | otherwise     = y !! (n - tag x)


search :: Measured a v => (v -> Bool) -> Tree v a -> Maybe a
search p t
    | p (measure t) = Just (go mempty p t)
    | otherwise     = Nothing
    where
    go i p (Leaf _ a) = a
    go i p (Branch _ l r)
        | p (i <> measure l) = go i p l
        | otherwise          = go (i <> measure l) p r



main :: IO ()
main = putStrLn "Hello, Eta!"
