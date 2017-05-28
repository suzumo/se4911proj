
module Load where

import Data.Csv
import Data.ByteString.Lex.Fractional
import qualified Data.ByteString.Lazy                     as BL
import qualified Data.ByteString.Char8                    as BS
import qualified Data.Vector                              as V
import Prelude                                            as P

import Data.Array.Accelerate                              ( Array, Elt, DIM2, Z(..), (:.)(..), fromList )


-- | Load a CSV file into an Accelerate array.
--
-- Assumes that the CSV file does not have any headers or missing data elements.
-- The size of the resulting array is given by the number of rows and columns in
-- the CSV file.
--
loadCSV :: (Elt e, FromField e) => FilePath -> IO (Array DIM2 e)
loadCSV csv = do
  er  <- decode NoHeader <$> BL.readFile csv
  case er of
    Left err -> error err
    Right vv ->
      let h = V.length vv
          w = V.length (V.head vv)
      in
      return $ fromList (Z :. h :. w) [ x | row <- V.toList vv
                                          , x   <- V.toList row
                                      ]


-- | Load a whitespace delimited file into an Accelerate array.
--
loadTXT :: (Elt e, Fractional e) => FilePath -> IO (Array DIM2 e)
loadTXT txt = do
  bs <- BS.readFile txt
  let
      rows = BS.count '\n' bs
      cols = length . BS.words $ BS.takeWhile (/= '\n') bs
      arr  = fromList (Z :.rows :. cols) [ fractional x | x <- BS.words bs ]
      --
      fractional x = case readSigned readExponential x of
                       Just (val,rest) | BS.null rest -> val
                       _                              -> error ("failed to read: " ++ BS.unpack x)
  --
  return arr

