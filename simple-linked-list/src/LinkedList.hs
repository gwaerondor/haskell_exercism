module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = LinkedList a (LinkedList a) | Nil

datum :: LinkedList a -> a
datum (LinkedList x _) = x

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (l:ls) = LinkedList l (fromList ls)

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x xs = LinkedList x xs

next :: LinkedList a -> LinkedList a
next (LinkedList _ xs) = xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = (fromList . reverse . toList)

toList :: LinkedList a -> [a]
toList Nil = []
toList (LinkedList x xs) = (x:toList xs)
