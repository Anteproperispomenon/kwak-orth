{-|
Module      : TextUTF8
Description : Simple IO in UTF-8.
Copyright   : (c) David Wilson, 2022
License     : BSD-3

This module has simple alternatives to
file I\/O and stdin\/stdout\/stderr that
ensure that UTF-8 is being used.
Essentially, it's just a wrapper
that uses `Data.Text.Encoding`.
together with I\/O from 
`Data.ByteString`, so you don't
have to use the conversion functions
every time you want to do I\/O.

Since this module has names that clash
with common functions from `Prelude`,
you'll want to import this module
qualified.
-}

module TextUTF8
    -- * Direct file I\/O
    ( readFile
    , writeFile
    , appendFile
    
    -- * Handle-Based I\/O
    , hGetContents
    , hGetLine
    , hPutStr
    , hPutStrLn
    
    -- * Functions for stdin\/stdout
    -- ** Working with standard I\/O
    , fixLocale
    -- ** ByteString-based I\/O
    , getContents
    , getLine
    , putStr
    , putStrLn
    , interact
    ) where

-- This line copy-pasted from https://hackage.haskell.org/package/text-1.2.2.1/docs/src/Data-Text-Lazy-IO.html
-- on 2018-05-03 13:04
import Prelude hiding (appendFile, getContents, getLine, interact,
                       putStr, putStrLn, readFile, writeFile)

import Data.ByteString    qualified as BS
import Data.Text          qualified as T
import Data.Text.Encoding qualified as T
import Data.Text (Text)
import System.IO (Handle, hSetEncoding, stdin, stdout, stderr, utf8)

----------------------------------------------
-- Working with default I\/O.

-- | Ensure that stdin\/stdout\/stderr use UTF-8 for I\/O.
-- Useful to run at the beginning of main to ensure that
-- output is consistent.
fixLocale :: IO ()
fixLocale = hSetEncoding stdin utf8 >> hSetEncoding stdout utf8 >> hSetEncoding stderr utf8

----------------------------------------------
-- Simple File Reading/Writing

-- | Read a UTF-8 encoded file strictly into `Text`.
readFile :: FilePath -> IO Text
readFile fp = T.decodeUtf8 <$> BS.readFile fp

-- | Write some `Text` encoded in UTF-8 directly to a file.
writeFile :: FilePath -> Text -> IO ()
writeFile fp txt = BS.writeFile fp $ T.encodeUtf8 txt

-- | Append some `Text` encoded in UTF-8 directly to a file.
-- Note that this does not check that the initial contents
-- are encoded in UTF-8.
appendFile :: FilePath -> Text -> IO ()
appendFile fp txt = BS.writeFile fp $ T.encodeUtf8 txt

----------------------------------------------
-- Handle-Based Reading/Writing

-- | Read all the contents of a handle strictly into a `Text`.
hGetContents :: Handle -> IO Text
hGetContents hnd = T.decodeUtf8 <$> BS.hGetContents hnd

-- | Read a line from a handle interpreted as UTF-8 into a `Text`.
hGetLine :: Handle -> IO Text
hGetLine hnd = T.decodeUtf8 <$> BS.hGetLine hnd

-- | Output some `Text` encoded as UTF-8 to handle.
hPutStr :: Handle -> Text -> IO ()
hPutStr hnd txt = BS.hPut hnd $ T.encodeUtf8 txt

-- | Outputs some `Text` followed by a newline encoded as UTF-8 to handle.
hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn hnd txt = BS.hPut hnd $ T.encodeUtf8 (txt `T.snoc` '\n')

----------------------------------------------
-- STDIO reading/writing

getLine :: IO Text
getLine = T.decodeUtf8 <$> BS.getLine

getContents :: IO Text
getContents = T.decodeUtf8 <$> BS.getContents

putStr :: Text -> IO ()
putStr txt = BS.putStr $ T.encodeUtf8 txt

putStrLn :: Text -> IO ()
putStrLn txt = BS.putStr $ T.encodeUtf8 (txt `T.snoc` '\n')

interact :: (Text -> Text) -> IO ()
interact f = BS.interact (T.encodeUtf8 . f . T.decodeUtf8)


