{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module InternalSpec where

import Metamorphosis.Internal
import Test.Hspec
import Lens.Micro

fields@[a1, a2, b1, b2] = [ mkField  "A" 1 "quantity" "Int"
                          , mkField  "A" 2 "name" "String"
                          , mkField  "B" 1 "quantity" "Maybe Int"
                          , mkField  "B" 2 "name" "Maybe String"
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

tdField cons field = tdCons . ix cons . cdFields . ix field
spec :: Spec
spec = do
  describe "FieldToTypes" $ do

    it "" $ do
      fieldsToTypes fields `shouldBe` [aT, bT]

  describe "applyFieldMapping" $ do
    it "it source properly" $ do
      let f = return . (fdTConsName .~ "B") . (fdTypes %~ ("Maybe":))
      applyFieldMapping f [aT] `shouldBe` [ bT & tdField  0 0 . fpSources .~  aT ^.. tdField 0 0
                                               & tdField 0 1 . fpSources .~  aT ^.. tdField 0 1
                                          ]
  describe "reverseTypeDescs" $ do
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

  


