module AOC.Imports.P
  ( module Export
  , read
  ) where

import Prelude as Export hiding (read)
import Text.ParserCombinators.ReadP as Export

read :: Read a => ReadP a
read = readS_to_P reads
