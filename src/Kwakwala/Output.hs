{-|
Module      : Kwakwala.Output.UmistaOutputNew
Description : All output functions
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This module contains output functions for all
orthographies. Look into their respective modules
for more information on the individual orthographies.

-}

module Kwakwala.Output
    -- * Umista
    -- ** Exclusively Using Strict Text
    ( decodeToUmista
    , decodeToUmistaAlt
    -- ** Strict Text with Builders
    , decodeToUmista2
    , decodeToUmistaAlt2
    -- ** Lazy Text Output
    , decodeToUmistaLazy
    , decodeToUmistaAltLazy
    -- * NAPA-Southern
    -- ** Exclusively Using Strict Text
    , decodeToNapa
    , decodeToNAPA
    -- ** Strict Text with Builders
    , decodeToNapa2
    , decodeToNAPA2
    -- ** Lazy Text Output
    , decodeToNapaLazy
    , decodeToNAPALazy
    -- * NAPA-Original
    -- ** Exclusively Using Strict Text
    , decodeToNapaAlt
    , decodeToNAPAalt
    -- ** Strict Text with Builders
    , decodeToNapaAlt2
    , decodeToNAPAalt2
    -- ** Lazy Text Output
    , decodeToNapaAltLazy
    , decodeToNAPAaltLazy
    -- ** Configurable Napa
    , decodeToNapaC
    , decodeToNapaLazyC
    , NapaOutputConfig(..)
    -- * Grubb-ASCII
    -- ** Exclusively Using Strict Text
    , decodeToGrubbAscii
    , decodeToGrubbAscii7
    , decodeToGrubbAsciiX
    , decodeToGrubbAsciiX7
    , decodeToGrubbAsciiJ
    , decodeToGrubbAsciiJ7
    , decodeToGrubbAsciiJX
    , decodeToGrubbAsciiJX7
    -- ** Strict Text with Builders
    , decodeToGrubbAscii2
    , decodeToGrubbAscii27
    , decodeToGrubbAsciiX2
    , decodeToGrubbAsciiX27
    , decodeToGrubbAsciiJ2
    , decodeToGrubbAsciiJ27
    , decodeToGrubbAsciiJX2
    , decodeToGrubbAsciiJX27
    -- ** Lazy Text Output
    , decodeToGrubbAsciiLazy
    , decodeToGrubbAsciiLazy7
    , decodeToGrubbAsciiLazyX
    , decodeToGrubbAsciiLazyX7
    , decodeToGrubbAsciiLazyJ
    , decodeToGrubbAsciiLazyJ7
    , decodeToGrubbAsciiLazyJX
    , decodeToGrubbAsciiLazyJX7
    -- * Pseudo-Boas
    , decodeToPseudoBoas
    , decodeToPseudoBoas2
    , decodeToPseudoBoasLazy
    -- * IPA
    -- ** Exclusively Using Strict Text
    , decodeToIpa
    , decodeToIpaAlt
    -- ** Strict Text with Builders
    , decodeToIpa2
    , decodeToIpaAlt2
    -- ** Lazy Text Output
    , decodeToIpaLazy
    , decodeToIpaLazyAlt
    -- * Georgian
    -- ** Exclusively Using Strict Text
    , decodeToGeorgian
    , decodeToGeorgianC
    , decodeToGeorgianAlt
    , decodeToGeorgianTitle
    -- ** Strict Text with Builders
    , decodeToGeorgian2
    , decodeToGeorgianC2
    , decodeToGeorgianAlt2
    , decodeToGeorgianTitle2
    -- ** Lazy Text Output
    , decodeToGeorgianLazy
    , decodeToGeorgianLazyC
    , decodeToGeorgianLazyAlt
    , decodeToGeorgianLazyTitle
    -- ** Georgian Configuration Options
    , GeorgianOutputConfig(..)
    -- * Island
    , decodeToIsland
    , decodeToIsland2
    , decodeToIslandLazy
    ) where

import Kwakwala.Output.UmistaOutputNew
import Kwakwala.Output.NapaOutput
import Kwakwala.Output.NapaOutputAlt
import Kwakwala.Output.GrubbAscii
import Kwakwala.Output.PseudoBoasOutput
import Kwakwala.Output.IPAOutput
import Kwakwala.Output.Georgian
import Kwakwala.Output.Island
