-------------------------------------------------------------------------
--
--         CodeTable.hs
--
--         Converting a Huffman tree to a table.
--
-------------------------------------------------------------------------
module CodeTable
  ( codeTable
  ) where

import Types (Tree(Leaf, Node), Bit(L, R), HCode, Table)
import Data.Map (fromList, assocs)

-- Making a table from a Huffman tree.
codeTable :: Ord a => Tree a -> Table a
codeTable t = fromList (convert [] t)

-- u6051965
-- Auxiliary function used in conversion to a table. The first argument is
-- the HCode which codes the path in the tree to the current Node, and so
-- codeTable is initialised with an empty such sequence.
convert :: Ord a => HCode -> Tree a -> [(a,HCode)]
convert bitList tree = case tree of
   Leaf num a -> [(a, bitList)]
   Node a left right -> convert (bitList++[L]) left ++ convert (bitList++[R]) right
--     [] -> error "Empty"
--     L:xs -> convert xs left
--     R:xs -> convert xs right


