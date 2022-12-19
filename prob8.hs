data Queue a = Queue [a] [a]
     deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue b c) = Queue b (a:c)

pop :: Queue a -> Queue a
pop (Queue [] []) = (Queue [] [])
pop (Queue [] bs) = (Queue (reverse $ init bs) [])
pop (Queue as bs) = (Queue (tail as) bs)

top :: Queue a -> a
top (Queue [] bs) = last bs
top (Queue as _) = head as

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

len::Queue a -> Int
len (Queue l r) = length(l) + length(r)

instance Eq a => Eq (Queue a) where
    q1@(Queue ar al) == q2@(Queue br bl)
        | len(q1) /= len(q2) = False
        | otherwise  = all (==True) $ zipWith (==) (ar ++ (reverse al)) (br ++ (reverse bl))
