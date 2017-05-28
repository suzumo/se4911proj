
module Data.Load.CSV where

import Data.Csv
import qualified Data.ByteString.Lazy                     as B
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
  er  <- decode NoHeader <$> B.readFile csv
  case er of
    Left err -> error err
    Right vv ->
      let h = V.length vv
          w = V.length (V.head vv)
      in
      return $ fromList (Z :. h :. w) [ x | row <- V.toList vv
                                          , x   <- V.toList row
                                      ]

