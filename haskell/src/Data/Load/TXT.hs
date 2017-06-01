
module Data.Load.TXT where

import Control.DeepSeq
import Data.ByteString.Lex.Fractional
import qualified Data.ByteString.Char8                    as B

import Data.Array.Accelerate                              ( Array, Shape, Elt, fromList )


-- | Load a whitespace delimited file of numbers into an Accelerate array of the
-- given size. It is an error if the file does not contain enough data.
--
loadTXT :: (Shape sh, Elt e, Fractional e) => FilePath -> sh -> IO (Array sh e)
loadTXT txt sh = do
  bs <- B.readFile txt
  let arr     = fromList sh [ frac x | x <- B.words bs ]
      frac x  = case readSigned readExponential x of
                  Just (val,rest) | B.null rest -> val
                  _                             -> error ("failed to read: " ++ B.unpack x)
  --
  return $!! arr

