module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Nil | BST a (BST a) (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Nil = Nothing
bstLeft (BST _ left _) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight Nil = Nothing
bstRight (BST _ _ right) = Just right

bstValue :: BST a -> Maybe a
bstValue Nil = Nothing
bstValue (BST x _ _) = Just x

empty :: BST a
empty = Nil

fromList :: Ord a => [a] -> BST a
fromList cs = fromList' empty cs

fromList' :: Ord a => BST a -> [a] -> BST a
fromList' root [] = root
fromList' root (c:cs) = fromList' (insert c root) cs

insert :: Ord a => a -> BST a -> BST a
insert x Nil = singleton x
insert x (BST y left right)
  | x <= y = BST y (insert x left) right
  | otherwise = BST y left (insert x right)

singleton :: a -> BST a
singleton x = BST x Nil Nil

toList :: BST a -> [a]
toList Nil = []
toList (BST x l r) = concat [leftList, [x], rightList]
  where
    leftList = toList l
    rightList = toList r
