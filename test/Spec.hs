import Test.Hspec
import Lib
import Hanoi

main :: IO ()
main = hspec $ do
  describe "toDigits and toDigitsRev working correctly" $ do
    it "returns valid sample values" $
      toDigits 1234 `shouldBe` ([1,2,3,4] :: [Integer])
    it "returns valid on empty input" $
      toDigits 0 `shouldBe` ([] :: [Integer])
    it "returns valid reverse value" $
      toDigitsRev 2947 `shouldBe` ([7,4,9,2] :: [Integer])
  describe "doubleEveryOther working correctly" $
    it "returns valid test values" $
      doubleEveryOther [8,7,6,5] `shouldBe` ([16,7,12,5] :: [Integer])
  describe "sumDigits working correctly" $
    it "returns valid test values" $
      sumDigits [16,7,12,5] `shouldBe` (22 :: Integer)
  describe "split functions working correctly" $
    it "returns valid splitInside values" $
      splitInside [16,7,12,5] `shouldBe` ([1,6,7,1,2,5] :: [Integer])
  describe "validate works correctly" $ do
    it "returns valid true value" $
      validate 4012888888881881 `shouldBe` (True :: Bool)
    it "returns valid false value" $
      validate 4012888888881882 `shouldBe` (False :: Bool)
  describe "hanoi working correctly" $ do
    it "returns valid test value" $ 
      hanoi 2 "a" "b" "c" `shouldBe` ([("a","c"), ("a","b"), ("c","b")] :: [(Peg, Peg)])
    it "has correct length on 4 pegs" $
      length (hanoi' 15 "a" "b" "c" "d") `shouldBe` (129 :: Int)
