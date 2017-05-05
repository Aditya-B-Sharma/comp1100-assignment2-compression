-------------------------------------------------------------------------
--                              
--         MakeTree.hs                          
--                              
--         Turn a frequency table into a Huffman tree
--                              
--         (c) Addison-Wesley, 1996-2011.                   
--                          
-------------------------------------------------------------------------
module MakeTree
  ( makeTree
  ) where

import Types (Tree(Leaf, Node), Bit(L, R), HCode, Table)
import Frequency (frequency)

-- Some imports used in code provided to you, you can ignore them.
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.List (sortBy)
import Data.Ord  (comparing)
import Data.Char (isDigit)

-- Convert the trees to a list, then amalgamate into a single tree.
makeTree :: Ord a => [(a, Int)] -> Tree a
makeTree = makeCodes . toTreeList
-- u6051965
-- Huffman codes are created bottom up: look for the least two frequent
-- letters, make these a new "isAlpha" (i.e. tree) and repeat until one tree
-- formed.  The function toTreeList makes the initial data structure.
toTreeList :: Ord a => [(a, Int)] -> [Tree a]
toTreeList list = case list of
    [] -> []
    (key, val):xs -> [Leaf val key] ++ toTreeList xs

-- u6051965
-- The value of a tree.                     
value :: Tree a -> Int
value tree = case tree of
  Leaf val key -> val
  Node val left right -> val

-- u6051965
-- Merge two trees.                          
merge :: Tree a -> Tree a -> Tree a
merge first second = Node (value first + value second) (first) (second)


-- u6051965 -- Collaboration with Arham Qureshi - u6378881
-- Sort a list of frequency trees by value (in ascending order)
sort :: [Tree a] -> [Tree a]
sort tree = sortBy (comparing value) tree


--case tree of
--  [] -> []
--  [a] -> [sortTree a]
--  x:x1:xs
--    | value x <= value x1 -> [sortTree x]++sort(x1:xs)
--    | otherwise -> sort(x1:xs)++[sortTree x]

--sortTree :: Tree a -> Tree a
--sortTree tree = case tree of
--  Leaf val key -> Leaf val key
-- Node val left right
--    | value left >= value right -> Node val (sortTree right) (sortTree left)
--    | otherwise -> Node val (sortTree left) (sortTree right)

-- u6051965
-- Merge the pair of trees at the front of the list
mergeFirstPair :: [Tree a] -> [Tree a]
mergeFirstPair list = case sort list of
  [] -> []
  [a] -> [a]
  x:x1:xs -> (merge x x1):xs

-- u6051965
-- Make codes: amalgamate the whole list.               
makeCodes :: [Tree a] -> Tree a
makeCodes tree = case tree of
   [] -> error "No valid list argument given."
   [a] -> a
   tree -> makeCodes (mergeFirstPair tree)

------------------------------------ Tests -----------------------------------
test :: Bool
test =
  sort (toTreeList $ frequency "Hello") ==
  sort [Leaf 1 'H', Leaf 1 'e', Leaf 2 'l', Leaf 1 'o'] &&
  sort
    (toTreeList $
     frequency "The quick brown fox jumped over the lazy dog.") ==
  sort
    [ Leaf 1 '.'
    , Leaf 1 'T'
    , Leaf 1 'a'
    , Leaf 1 'b'
    , Leaf 1 'c'
    , Leaf 1 'f'
    , Leaf 1 'g'
    , Leaf 1 'i'
    , Leaf 1 'j'
    , Leaf 1 'k'
    , Leaf 1 'l'
    , Leaf 1 'm'
    , Leaf 1 'n'
    , Leaf 1 'p'
    , Leaf 1 'q'
    , Leaf 1 't'
    , Leaf 1 'v'
    , Leaf 1 'w'
    , Leaf 1 'x'
    , Leaf 1 'y'
    , Leaf 1 'z'
    , Leaf 2 'd'
    , Leaf 2 'h'
    , Leaf 2 'r'
    , Leaf 2 'u'
    , Leaf 4 'e'
    , Leaf 4 'o'
    , Leaf 8 ' '
    ]
  where
    sort = sortBy (comparing value)

-- Make trees print out in a pretty way
instance Show a => Show (Tree a) where
    show (Leaf n x) = let xstr = show x in pad (length xstr) ' ' (show n) ++ "\n" ++ xstr
    show (Node n l r) = let
        llines = lines (show l)
        lmax   = maximum (map length llines)
        rlines = lines (show r)
        rmax   = maximum (map length rlines)
        nstr   = show n
        nwidth = length nstr
        depth  = length llines `max` length rlines
        spaces n     = replicate n ' '
        joined = zipWith3 (\left mid right -> left ++ mid ++ right)
                    (llines ++ repeat (spaces lmax))
                    (replicate depth (spaces nwidth))
                    (rlines ++ repeat (spaces rmax))
        
        nLine = spaces lmax ++ nstr ++ spaces rmax
        (barLine1, rest1) = break isDigit (head joined) -- find first digit
        (barLine2, rest2) = break (not . isDigit) rest1 -- skip digits
        (barLine3, rest3) = break isDigit rest2         -- find next digit
        (barLine4       ) = map (const ' ') rest3       -- fill the rest with spaces
        barLine = barLine1 ++ " " ++
                  "/" ++
                  map (const '-') (drop 2 $ barLine2 ++ barLine3) ++
                  "\\" ++
                  drop 1 barLine4
        in unlines (nLine:barLine:joined) 

pad :: Int -> Char -> String -> String
pad width char str =
    let len = length str
        lenHalf = (width - len) `div` 2
    in replicate (width - len - lenHalf) char ++ str ++ replicate lenHalf char
 
instance Binary a => Binary (Tree a) where
    put (Leaf n x) = putWord8 0 >> putWord32be (fromIntegral n) >> put x
    put (Node _ l r) = putWord8 1 >> put l >> put r
    get = do
        b <- getWord8
        case b of
            0 -> do
                n <- getWord32be
                x <- get
                return $ Leaf (fromIntegral n) x
            1 -> do
                l <- get
                r <- get
                return $ merge l r

instance Functor Tree where
    fmap f (Leaf n x) = Leaf n (f x)
    fmap f (Node n l r) = Node n (fmap f l) (fmap f r)
