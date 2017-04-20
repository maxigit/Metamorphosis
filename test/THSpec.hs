{-# LANGUAGE TemplateHaskell #-}
module THSpec where

import Metamorphosis.TH
import Metamorphosis.Types
import Test.Hspec
import Lens.Micro
import Language.Haskell.TH
import Metamorphosis.Internal
import qualified Data.Map as Map
import Data.Char(isSpace)
import Data.List(group)

data A = A Int String | Record { style :: String , price :: Double} deriving Show
aInfo = TyConI (DataD []
               (mkName "A")
               []
               Nothing [ NormalC (mkName "A")
                         [ (Bang NoSourceUnpackedness NoSourceStrictness,ConT (mkName "Int"))
                         , (Bang NoSourceUnpackedness NoSourceStrictness,ConT (mkName "String"))
                         ]
                       , RecC (mkName "Record") [ ((mkName "style") ,Bang NoSourceUnpackedness NoSourceStrictness,ConT (mkName "String"))
                                                , ((mkName "price"),Bang NoSourceUnpackedness NoSourceStrictness,ConT (mkName "Double"))
                                                ]
                       ]
               [ConT (mkName "Show")]
               )
data Point a = Point { x :: a, y :: a } deriving Show
pointInfo = TyConI (DataD []
                   (mkName "Point")
                   []
                   Nothing [ RecC (mkName "Point") [ ((mkName "x") ,Bang NoSourceUnpackedness NoSourceStrictness,ConT (mkName "a"))
                                                   , ((mkName "y"),Bang NoSourceUnpackedness NoSourceStrictness,ConT (mkName "a"))
                                                   ]
                           ]
                   [ConT (mkName "Show")]
                   )

shouldLook expr expected = sanitize (show $ ppr expr) `shouldBe` sanitize expected
  where sanitize str = let
          spc = map (\c -> if isSpace c then ' ' else c)  str
          groups = group spc
          strip (' ':_) = " "
          strip s = s
          in concatMap strip groups
shouldLookQ q expected = do
  expr <- runQ q
  expr `shouldLook` expected
  

spec :: Spec
spec = do
  describe "collectFields" $ do
    let aFields = collectFields aInfo
    it "collect fields for sum types and records" $ do
      aFields ^.. each . fdTypeName `shouldBe` ["A", "A", "A", "A"]
      aFields ^.. each . fdConsName `shouldBe` ["A", "A", "Record", "Record"]
      aFields ^.. each . fdFieldName `shouldBe` [Nothing, Nothing, Just "style", Just "price"]

  let aFields = collectFields  aInfo
      [a] = fieldsToTypes aFields
      aA = a ^?! tdCons . ix 0
      aRecord = a ^?! tdCons . ix 1
      [point] = fieldsToTypes (collectFields  pointInfo)
      pPoint = point ^?! tdCons . ix 0
  describe "genConsBodyE" $ do
    it "uses () when field not found" $ do
      let body = genConsBodyE Map.empty (head $ a ^. tdCons)
      (show . ppr $ body) `shouldBe` "A () ()"
    it "uses field name in the correct order" $ do
      m <- runQ (genNameMap $ a ^. tdCons . each . cdFields )
      let body = genConsBodyE m ((a ^. tdCons) !! 1)
      (show . ppr $ body) `shouldBe` "Record style_0 price_1"
    it "uses position in the correct order" $ do
      m <- runQ (genNameMap $ a ^. tdCons . each . cdFields )
      let body = genConsBodyE m ((a ^. tdCons) !! 0)
      (show . ppr $ body) `shouldBe` "A a1_0 a2_1"
  describe "genConsClause" $ do
    it "generates copy for simple constructor" $ do
      let aA' = aA  & cdFields . each %~ (\f -> f & fpSources .~ [f])
      genConsClause [[aA]] [aA'] `shouldLookQ` "(A a1_0 a2_1) = (A (a1_0) (a2_1))"
    it "generates copy for record constructor" $ do
      genConsClause [[aRecord]] [aRecord] `shouldLookQ` "(Record style_0 price_1) = (Record style_0 price_1)"
    it "generates copy for of tuples" $ do
      genConsClause [[aRecord, pPoint]] [aRecord, pPoint]
        `shouldLookQ` "(Record style_0 price_1, Point x_2 y_3) = (Record style_0 price_1, Point x_2 y_3)"
    it "generates wildcard for non used pattern" $ do
      genConsClause [[aRecord]] [pPoint] `shouldLookQ` "(Record _ _) = Point () ()"

