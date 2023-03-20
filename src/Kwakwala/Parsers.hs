{-|
Module      : Kwakwala.Parsers
Description : All Parsers
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This module contains all output functions
in one place. For more information on the
orthographies involved, look into the 
documentation for the individual modules.

-}

module Kwakwala.Parsers
    -- * Umista
    -- ** Parsers
    ( parseUmista
    , parseUmistaOld
    -- ** Direct Encoders
    , encodeFromUmista
    , encodeFromUmistaOld
    -- * NAPA
    -- ** Direct Encoders
    , encodeFromNapa
    , encodeFromNAPA
    -- ** Parsers
    , parseNapa
    , parseNAPA
    -- ** Old Versions
    , encodeFromNapaOld
    , parseNapaOld
    -- * Grubb-ASCII
    , encodeFromGrubbAscii
    , parseGrubbAscii
    -- * Boas
    , encodeFromBoas
    , parseBoas
    -- * Georgian
    , encodeFromGeorgian
    , parseGeorgian
    -- * Island
    , encodeFromIsland
    , encodeFromIslandOld
    , parseIsland
    , parseIslandOld
    ) where

import Kwakwala.Parsers.Boas
import Kwakwala.Parsers.GeorgianParser
import Kwakwala.Parsers.GrubbAsciiParser
import Kwakwala.Parsers.NapaParser
import Kwakwala.Parsers.Umista
import Kwakwala.Parsers.Island
