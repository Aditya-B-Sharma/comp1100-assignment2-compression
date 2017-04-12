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
encodeMessage
  :: Ord a
  => Table a -> [a] -> HCode
encodeMessage = undefined -- TODO

-- lookup looks up the meaning of an individual char in a Table.
lookup
  :: Ord a
  => Table a -> a -> HCode
lookup m c = fromMaybe (error "lookup") (Map.lookup c m)

-- Decode a message according to a tree.                
--                              
-- The first tree argument is constant, being the tree of codes;
-- the second represents the current position in the tree relative  
-- to the (partial) HCode read so far.               
decodeMessage
  :: Ord a
  => Tree a -> HCode -> [a]
decodeMessage = undefined -- TODO
