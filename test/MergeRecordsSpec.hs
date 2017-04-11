{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module MergeRecordsSpec where

import Control.Lens
import Test.Hspec
import Metamorphosis
import Control.Monad
import Data.Functor.Identity

data AB = AB { a :: String, b :: Maybe Int } deriving (Show, Read, Eq)
data CD = CD { c :: String, d :: Double } deriving (Show, Read, Eq)


-- Merge AB and CD to ABCD
-- data ABCD = ABCD { a :: String, b :: Int, c :: String, d :: Double }
$(metamorphosis'
  (return
  . (fdTName .~ "ABCD")
  . (fdCName .~ "ABCD")
  . (fdTypes %~ \case
        ("Maybe":ts) ->  ts -- remove the Maybe, to make things more interesting
        ts -> ts
    )
  )
  [''AB, ''CD] ["ABCD"]
 )
$(generateSet
  (return
  . (fdTName .~ "AB")
  . (fdCName .~ "AB")
  . (fdTypes %~ \case
        ("Maybe":ts) ->  ts -- remove the Maybe, to make things more interesting
        ts -> ts
    )
  )
  [''ABCD] [''AB]
  "aBInABCDA"
 )
deriving instance Show ABCD
deriving instance Eq ABCD


abcdSpecs =
  describe  "merge (AB,CD) -> ABCD" $ do
    it "gets abcd from (ab,cd)" $ do
       (flip aB'CDToABCDA) (AB "a" (Just 3)) (CD "c" 5.0) `shouldBe` Just (ABCD "a" 3 "c" 5.0)
    it "gets nothing from (ab,cd)" $ do
       (flip aB'CDToABCDA) (AB "a" Nothing) (CD "c" 5.0) `shouldBe` Nothing
    it "gets (ab, cd) from abcd" $ do
      aBCDToAB'CDA (ABCD "a'" 7 "c" 7.0) `shouldBe` ( Identity (AB "a'" (Just 7))
                                                    , Identity (CD "c" 7.0)
                                                    )

    it "sets an AB" $ do
      aBInABCDA (ABCD "old" 1 "c" 7) (AB "new" (Just 10))  `shouldBe` Just (ABCD "new" 10 "c" 7)
    it "doesn't set an AB" $ do
      aBInABCDA (ABCD "old" 1 "c" 7) (AB "new" Nothing)  `shouldBe` Nothing

spec :: Spec
spec = do
  abcdSpecs
