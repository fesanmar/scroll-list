import Test.Hspec        (Spec, it, shouldBe, runIO)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Data.List.Scroll ( down, up, deleteByIndex )

main :: IO ()
main =  hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
    
    -- Deleting
    it "Removing the middle item from a list" $
     deleteByIndex 1 toThree `shouldBe` ["one", "three"]
    
    it "Removing the first item from a list" $
     deleteByIndex 0 toThree `shouldBe` ["two", "three"]
    
    it "Removing the last item from a list" $
     deleteByIndex 2 toThree `shouldBe` ["one", "two"]
    
    it "Removing an item from a singleton list" $
     null (deleteByIndex 0 ["one"]) `shouldBe` True

    it "Removing an item from an empty list" $
     null (deleteByIndex 1 []) `shouldBe` True

    it "Removing an out of range item from a list" $
     deleteByIndex 3 toThree `shouldBe` toThree
    
    it "Removing an out of range negative index from a list" $
     deleteByIndex (-1) toThree `shouldBe` toThree

    -- Moving up
    it "Moving up an out of range index" $
     up 4 1 toThree `shouldBe` toThree

    it "Moving up a negative index" $
     up (-1) 1 toThree `shouldBe` toThree

    it "Move item one position up" $
     up 1 1 toThree `shouldBe` ["two", "one", "three"]

    it "Move item two positions up" $
     up 2 2 toThree `shouldBe` ["three", "one", "two"]

    it "Move item too many positions up" $
     up 2 3 toThree `shouldBe` ["three", "one", "two"]

    it "Move last item one position up" $
     up 2 1 toThree `shouldBe` ["one", "three","two"]

    it "Move no position up" $
     up 2 0 toThree `shouldBe` toThree

    it "Move negative position up" $
     up 2 (-2) toThree `shouldBe` toThree

    it "Move item three positions up in a longer list" $
     up 7 3 toTen `shouldBe` ["one", "two", "three", "four", "eight", "five", "six", "seven", "nine", "ten"]

    -- Moving down
    it "Moving dow an out of range index" $
     down 4 1 toThree `shouldBe` toThree

    it "Moving down a negative index" $
     down (-1) 1 toThree `shouldBe` toThree

    it "Moving down a negative index as bigger as list's lengt" $
     down (-3) 1 toThree `shouldBe` toThree

    it "Moving down a negative index bigger than list's lengt" $
     down (-4) 1 toThree `shouldBe` toThree

    it "Move item one position down" $
     down 0 1 toThree `shouldBe` ["two", "one", "three"]

    it "Move item two positions down" $
     down 0 2 toThree `shouldBe` ["two", "three", "one"]

    it "Move item too many positions down" $
     down 0 4 toThree `shouldBe` ["two", "three", "one"]

    it "Move no position down" $
     down 2 0 toThree `shouldBe` toThree

    it "Move negative position down" $
     down 2 (-2) toThree `shouldBe` toThree

    it "Move negative position down" $
     down 2 (-2) toThree `shouldBe` toThree

    it "Move item three positions down in a longer list" $
     down 5 3 toTen `shouldBe` ["one", "two", "three", "four", "five", "seven", "eight", "nine", "six", "ten"]

toThree :: [String]
toThree = ["one", "two", "three"]

toTen :: [String]
toTen = toThree ++ ["four", "five", "six", "seven", "eight", "nine", "ten"]