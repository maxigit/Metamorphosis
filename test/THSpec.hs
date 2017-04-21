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
      [a'] = applyFieldMapping (:[]) [a]
      aA = a ^?! tdCons . ix 0
      aRecord = a ^?! tdCons . ix 1
      aA' = a' ^?! tdCons . ix 0
      aRecord' = a' ^?! tdCons . ix 1

      [point] = fieldsToTypes (collectFields  pointInfo)
      pPoint = point ^?! tdCons . ix 0
      [point'] = applyFieldMapping (:[]) [point]
      pPoint' = point' ^?! tdCons . ix 0
  

  describe "genConsBodyE" $ do
    it "uses () when field not found" $ do
      let body = genConsBodyE identityBCR Map.empty (head $ a ^. tdCons)
      (show . ppr $ body) `shouldBe` "A () ()"

    it "uses field name in the correct order" $ do
      m <- runQ (genNameMap $ a ^. tdCons . each . cdFields )
      let body = genConsBodyE identityBCR m ((a' ^. tdCons) !! 1)
      (show . ppr $ body) `shouldBe` "Record (style_0) (price_1)"

    it "uses position in the correct order" $ do
      m <- runQ (genNameMap $ a ^. tdCons . each . cdFields )
      let body = genConsBodyE identityBCR m ((a' ^. tdCons) !! 0)
      (show . ppr $ body) `shouldBe` "A (a1_0) (a2_1)"
  

  describe "genConsClause" $ do
    let genCopy = genConsClause identityBCR

    it "generates copy for simple constructor" $ do
      genCopy [[aA]] [aA'] `shouldLookQ` "(A a1_0 a2_1) = (A (a1_0) (a2_1))"

    it "generates copy for record constructor" $ do
      genCopy [[aRecord]] [aRecord'] `shouldLookQ` "(Record style_0 price_1) = (Record (style_0) (price_1))"
    it "generates copy for of tuples" $ do
      genCopy [[aRecord, pPoint]] [aRecord', pPoint']
        `shouldLookQ` "(Record style_0 price_1, Point x_2 y_3) = (Record (style_0) (price_1), Point (x_2) (y_3))"
    it "generates wildcard for non used pattern" $ do
      genCopy [[aRecord]] [pPoint] `shouldLookQ` "(Record _ _) = (Point () ())"

    it "generates applicatives" $ do
      genConsClause applicativeBCR [[aA]] [aA'] `shouldLookQ` "(A a1_0 a2_1) = (A <$> (a1_0) <*> (a2_1))"

    it "generates to monoid" $ do
      genConsClause (monoidBCR "return") [[aA]] [aA'] `shouldLookQ` "(A a1_0 a2_1) = (mempty <> (return a1_0) <> (return a2_1))"

    it "generates to monoid with Pure" $ do
      genConsClause (monoidPureBCR "show") [[aA]] [aA'] `shouldLookQ` "(A a1_0 a2_1) = (mempty <> ((pure . show) a1_0) <> ((pure . show) a2_1))"

    it "generates extractor" $ do
      genConsClause extractBCR [[aA]] [aA'] `shouldLookQ` "(A a1_0 a2_1) = (A <$> (extract a1_0) <*> (extract a2_1))"

    it "use multiple constructors (tuples)" $ do
       let f = ( return . (fdFieldName .~ (Just "P")) . (fdTConsName .~ "P"))
           [p] = applyFieldMapping f [point]
       genConsClause identityBCR [[pPoint]] (p ^. tdCons) `shouldLookQ` "(Point x_0 y_1) = (P (GHC.Tuple.(,) x_0 y_1))"

  describe "genConversion" $ do

    it "generates simple converter" $ do
      genConversion identityBCR [[point]] [point'] `shouldLookQ`
        "iPointToPoint (Point x_0 y_1) = (Point (x_0) (y_1))"

    it "uses correct constructor for sum types" $ do
      genConversion identityBCR [[a]] [a'] `shouldLookQ`
        "iAToA (A a1_0 a2_1) = (A (a1_0) (a2_1)) \
        \iAToA (Record style_2 price_3) = (Record (style_2) (price_3))"

  describe "generateType" $ do

    it "generates complex type" $ do
      generateType [''Show] a `shouldLook`
        "data A = A Int String | Record {style :: String, price :: Double} deriving GHC.Show.Show"

    it "generates parametric type" $ do
      generateType [] point `shouldLook`
        "data Point a = Point {x :: a, y :: a}" 

       


