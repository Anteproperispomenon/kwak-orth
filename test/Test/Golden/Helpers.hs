
module Test.Golden.Helpers 
  ( goldenVsStringDiff'
  ) where

import Data.ByteString.Lazy qualified as BL

import Test.Tasty (TestTree, TestName)
import Test.Tasty.Golden

-- | Drop-in replacement for `goldenVsString`
-- but using diff.
goldenVsStringDiff' :: TestName -> FilePath -> IO BL.ByteString -> TestTree
goldenVsStringDiff' tstN fp bs
  = goldenVsStringDiff tstN (\ref new -> ["diff", "-u", ref, new]) fp bs
