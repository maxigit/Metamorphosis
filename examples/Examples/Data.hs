-- | Data types to be used in examples.
-- There are meant to be import qualified to
-- avoid redeclaration when testing metamorphosis.
module Examples.Data where



data Unique = Unique
data Record = Record { code :: String, price :: Double} deriving (Read, Show, Eq, Ord)
data Plain = Plain String Double
data ABC = A | B | C
data Quantity = Quantity { quantity :: Int } deriving (Read, Show, Eq, Ord)

 
