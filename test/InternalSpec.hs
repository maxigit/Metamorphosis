{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module InternalSpec where

import Metamorphosis.Internal
import Data.List (sort)
import Test.Hspec
import Lens.Micro

(<$$$>) = map . map . map
fields@[a1, a2, b1, b2] = [ mkField  "A" 1 "quantity" "Int"
                          , mkField  "A" 2 "name" "String"
                          , mkField  "B" 1 "quantity'" "Maybe Int"
                          , mkField  "B" 2 "name'" "Maybe String"
                          ]
aT = TypeDesc "A"
              Nothing
              [ let aC = ConsDesc "A" aT [ FieldDescPlus a1 aC []
                                         , FieldDescPlus a2 aC []
                                         ]
                in aC
              ]            
bT = TypeDesc "B"
              Nothing
              [ let bC = ConsDesc "B" bT [ FieldDescPlus b1 bC []
                                         , FieldDescPlus b2 bC []
                                         ]
                in bC
              ]            
abT = TypeDesc "AB"
              Nothing
              [ let abaC = ConsDesc "ABA" abT [ FieldDescPlus b1 abaC []
                                              , FieldDescPlus b2 abaC []
                                              ]
                in abaC
              , let abbC = ConsDesc "ABB" abT [ FieldDescPlus b1 abbC []
                                              , FieldDescPlus b2 abbC []
                                              ]
                in abbC
              ]            
xyT = TypeDesc "XY"
              Nothing
              [ let xyxC = ConsDesc "XYX" xyT [ FieldDescPlus b1 xyxC []
                                              , FieldDescPlus b2 xyxC []
                                              ]
                in xyxC
              , let xyyC = ConsDesc "XYX" xyT [ FieldDescPlus b1 xyyC []
                                              , FieldDescPlus b2 xyyC []
                                              ]
                in xyyC
              ]            

tdField cons field = tdCons . ix cons . cdFields . ix field
spec :: Spec
spec = do
  describe "FieldToTypes" $ do

    it "" $ do
      fieldsToTypes fields `shouldBe` [aT, bT]

  describe "applyFieldMapping" $ do
    it "set source properly" $ do
      let f = return . (fdTConsName .~ "B") . (fdFieldName.mapped %~ (++"'")) . (fdTypes %~ ("Maybe":))
      applyFieldMapping f [aT] `shouldBe` [ bT & tdField  0 0 . fpSources .~  aT ^.. tdField 0 0
                                               & tdField 0 1 . fpSources .~  aT ^.. tdField 0 1
                                          ]
  describe "reverseTypeDescs" $ do
    context "one to one" $ do
      it "set sources properly" $ do
        let f = return . (fdTConsName .~ "B") . (fdTypes %~ ("Maybe":))
            [bT'] = applyFieldMapping f [aT]
        reverseTypeDescs [bT']  `shouldBe` [ aT & tdField 0 0 . fpSources .~  bT' ^.. tdField 0 0
                                                & tdField 0 1 . fpSources .~  bT' ^.. tdField 0 1
                                          ]
      it "is idempotent" $ do
        let f = return . (fdTConsName .~ "B") . (fdTypes %~ ("Maybe":))
            [bT'] = applyFieldMapping f [aT]
            -- We just set the sources sources to [] to make the comparison easier
            resetSourceSources = each . tdCons . each . cdFields . each . fpSources . each . fpSources .~ []
        resetSourceSources ( reverseTypeDescs (reverseTypeDescs [bT']))  `shouldBe` [bT'] 
  context "many to one " $ do
    let f = return . (fdTConsName .~ "C")
        fieldDescs = fieldsToTypes fields
        cT = applyFieldMapping f fieldDescs
    it "has one constructor" $ do
      length (cT ^.. each . tdCons) `shouldBe` 1
    it "has 4 fields" $ do
      length (cT ^.. each . tdCons . each . cdFields . each) `shouldBe` 4
    it "has all original fields as sources" $ do
      sort (cT ^.. each . tdCons . each . cdFields . each . fpSources . each)
        `shouldBe` sort (fieldDescs ^.. each . tdCons . each .cdFields . each)
  describe "consCombinations" $ do
    it "combines like tuples" $ do
      (_cdName <$$$> consCombinations [[aT, xyT]])
        `shouldBe` [ [["A", "XYX"]]
                   , [["B", "XYY"]]
                   ]
    it "combines like argument" $ do
      (_cdName <$$$> consCombinations [[aT], [xyT]])
        `shouldBe` [ [["A"], ["XYX"]]
                   , [["B"], ["XYY"]]
                   ]
  it "combines everything" $ do
      (_cdName <$$$> consCombinations [[xyT], [abT, bT]])
        `shouldBe` [ [["XYX"] , ["ABA", "B"]]
                   , [["XYY"] , ["ABA", "B"]]
                   , [["XYX"] , ["ABB", "B"]]
                   , [["XYY"] , ["ABB", "B"]]
                   ]

  


