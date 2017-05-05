-------------------------------------------------------------------------
--  
--         Coding.hs                            
--                              
--         Huffman coding in Haskell.                   
--
--         The top-level functions for coding and decoding.
--
-------------------------------------------------------------------------
module Coding
  ( encodeMessage
  , decodeMessage
  ) where

import Types (Tree(Leaf, Node), Bit(L, R), HCode, Table)

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- Code a message according to a table of codes.            
--encodeMessage :: Ord a => Table a -> [a] -> HCode
--encodeMessage table message = case message of
--  [] -> error "blah"
--  x:xs -> (lookup table x)++(encodeMessage table xs)  -- TODO

encodeMessage :: Ord a => Table a -> [a] -> HCode
encodeMessage table [] = []
encodeMessage table (x:xs) = (lookup table x) ++ (encodeMessage table xs)
-- TODO

-- lookup looks up the meaning of an individual char in a Table.
lookup :: Ord a => Table a -> a -> HCode
lookup m c = fromMaybe (error "lookup") (Map.lookup c m)

-- Decode a message according to a tree.                
--                              
-- The first tree argument is constant, being the tree of codes;
-- the second represents the current position in the tree relative  
-- to the (partial) HCode read so far.               
decodeMessage :: Ord a => Tree a -> HCode -> [a]
decodeMessage tree (x:xs) = help tree (x:xs)
                             where
                               help (Node val left right) (L:xs) = help left xs
                               help (Node val left right) (R:xs) = help right xs
                               help (Leaf val key) xs = key : (help tree xs)
                               help _ [] = []
--help :: Ord a => Tree a -> HCode -> (a, HCode)
--help tree (x:xs) = case tree of
--  Leaf val a -> (a, (x:xs))
--  Node a left right -> case (x:xs) of
--    [] -> []
--    L:xs -> help left xs
--    R:xs -> help right xs

