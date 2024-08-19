import Test.Hspec    (describe, hspec, it, shouldBe)
import ListsPart2 (Bit(..),  charToBits, bits, queens, queenPossiblePositions)

main :: IO ()
main = hspec $ do
  describe "charToBits" $ do
    it "A" $ do
      charToBits 'A' `shouldBe` [F,T,F,F,F,F,F,T]
    it "B" $ do
      charToBits 'B' `shouldBe` [F,T,F,F,F,F,T, F]
    it "Char 0" $ do
      charToBits '\0' `shouldBe` [F,F,F,F,F,F,F,F]
      
      
  describe "bits" $ do
    it "AB" $ do
      bits "AB" `shouldBe` [F,T,F,F,F,F,F,T,F,T,F,F,F,F,T,F]
    it "\"\"" $ do
      bits "" `shouldBe` []
      

  describe "queens" $ do
    it "Possible positions" $ do
       queenPossiblePositions 4 [2, 4, 1] `shouldBe` [3]
    it "Possible positions" $ do
       queenPossiblePositions 4 [2, 4] `shouldBe` [1]
    it "Possible positions" $ do
       queenPossiblePositions 4 [2] `shouldBe` [4]
    it "1x1" $ do
      queens 1 `shouldBe` [[1]]     
    it "2x2" $ do
      queens 2 `shouldBe` []
    it "4x4" $ do
      queens 4 `shouldBe` [[2,4,1,3],[3,1,4,2]]
    it "first (8x8)" $ do
      minimum (queens 8) `shouldBe`[1,5,8,6,3,7,2,4]
    it "number of solutions 8x8" $ do
      length (queens 8) `shouldBe` 92 
               