{-|
Module      : Kwakwala.Parsers.Helpers
Description : Parser Helpers
Copyright   : (c) David Wilson, 2022
License     : BSD-3

Some helper parsing functions
that occur frequently in multiple
parser modules.

Note that not all common parsers
can be moved here; some rely on
orthography-specific definitions
of some characters; e.g. which
codepoints count as apostrophes.
-}

module Kwakwala.Parsers.Helpers
  ( parsePipe
  ) where

import Data.Attoparsec.Text qualified as AT
import Data.Text            qualified as T
import Control.Monad
import Control.Applicative
import Kwakwala.Sounds

-- | For Parsing 'Escaped' Text
parsePipe :: AT.Parser CasedChar
parsePipe = Punct <$> ((AT.char '|') `comb1` (AT.takeWhile1 (/= '|')) `comb2` (AT.char '|'))
    where comb1 = liftM2 (T.cons)
          comb2 = liftM2 (T.snoc)
