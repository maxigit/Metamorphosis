{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
import Metamorphosis

data A = A
data F a b = F a b
data B = B Int
data R = R { name :: String, price :: Double}
$(reshape (const Nothing) ''A "A0")
$(reshape (\(n, v) -> Just ("Blob" ++ n, v)) ''B "BLOB")
$(reshape (\(n, v) -> Just (n ++ "Z", v)) ''R "Rob")
