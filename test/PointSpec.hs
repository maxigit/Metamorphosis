{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module PointSpec where

import Control.Lens
import Test.Hspec

data Point2 = Point2 { x :: Double, y :: Double } deriving (Show, Read, Eq)
data Point3 = Point3 { x :: Double, y :: Double, z :: Double } deriving (Show, Read, Eq)
data PointM = PointM { x :: Double,  y :: Double, z :: Maybe Double} deriving (Show, Read, Eq)

point2 :: Prism' PointM Point2
-- point2 = prism (\(Point2 x y) -> PointM x y Nothing)
--                (\p@(PointM x y zm) -> maybe (Right (Point2 x y)) (\_ -> Left p) zm)
point2 = prism' (\(Point2 x y) -> PointM x y Nothing)
               (\p@(PointM x y zm) -> maybe (Just (Point2 x y)) (const Nothing) zm)
point3 :: Prism' PointM Point3
-- point3 :: (Point3 -> f Point3) -> PointM -> f PointM
point3 = prism (\(Point3 x y z) -> PointM x y (Just z))
               (\p@(PointM x y zm) -> maybe (Left p) (Right . Point3 x y ) zm)

pointM3 :: Lens PointM Point3 (Maybe Double) Double
pointM3 f (PointM x y mz) = (Point3 x y) `fmap` f mz

pm2 = PointM 1 2 Nothing
pm3 = PointM 1 2 (Just 3)
p2 = Point2 1 2
p3 = Point3 1 2 3

spec :: Spec
spec = do
  describe "PointM to Point3" $ do
    it "extract if possible" $ do
        pm3 ^? point3 `shouldBe` (Just p3)
    it "returns nothing if not" $ do
        pm2 ^? point3 `shouldBe` Nothing
    it "contructs" $ do
      point3 # p3 `shouldBe` pm3
    it "updates if possible" $ do
      (point3 %~ (\(Point3 x y z) -> Point3 x y 10) $ pm3) `shouldBe` PointM 1 2 (Just 10)
    it "doesn't updates if not" $ do
      (point3 %~ (\(Point3 x y z) -> Point3 x y 10) $ pm2) `shouldBe` PointM 1 2 Nothing
  describe "PointM to Point2"$ do
    it "extract if possible" $ do
        pm2 ^?! point2 `shouldBe` p2
    it "doesn't extract if not" $ do
        pm2 ^? point3 `shouldBe` Nothing
  describe "Point3 to PointM" $ do
    it "get pointM" $ do
      p3 ^. re point3 `shouldBe` pm3
    -- it "set pointM if possible" $ do
      -- (re point3 .~ (PointM 10 20 (Just 50)) $ p3) `shouldBe` (Point3 10 20 50)
      -- (re point3 %~ _ $ p3) `shouldBe` (Point3 10 20 50)
    -- it "set pointM" $ do
      -- (re point3 .~ (PointM 10 20 Nothing) $ p3) `shouldBe` (Point3 1 2 3)
    
