data Tree a = Node a (Tree a) (Tree a) | Empty
    deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ b c) = 1 + size b + size c

height :: Tree a -> Int
height Empty = 0
height (Node _ b c) = 1 + max (height b) (height c)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal _ Empty = False
equal Empty _ = False
equal (Node a b c) (Node d e f) = (a == d) && (equal b e) && (equal c f)

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node a b c) (Node d e f) = (a == d) && ((isomorphic b e && isomorphic c f) || ((isomorphic b f && isomorphic c e)))

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a b c) = [a] ++ preOrder b ++ preOrder c

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a b c) = postOrder b ++ postOrder c ++ [a]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a b c) = inOrder b ++ [a] ++ inOrder c

bfs [] = []
bfs (Empty:xs) = bfs xs
bfs ((Node a b c):xs) = a : (bfs $ xs ++ [b,c])

breadthFirst :: Tree a -> [a]
breadthFirst t = bfs [t]
