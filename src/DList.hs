module DList where
-- implementation similar to Data.DList package
-- based on https://kseo.github.io/posts/2017-01-21-writer-monad.html
newtype DList a = DL { unDL :: [a] -> [a] }

instance Monoid (DList a) where
    mempty  = DL id
    mappend xs ys = DL (unDL xs . unDL ys)


toDList :: [a] -> DList a
toDList l = DL (l++)


singleton e = toDList [e]


toList :: DList a -> [a]
toList dl = (unDL dl) []
