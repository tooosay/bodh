import Test.HUnit
import Control.Monad (void)
----------------------------------------------------------------
-- import modules to Test here
import Convert.BO
----------------------------------------------------------------
--- define test here
testBO :: Test
testBO = TestCase $ do 
    assertEqual "binary to octal" "250" (bToO "010101000")


----------------------------------------------------------------
-- add tests to this list here
tests :: Test
tests = TestList [TestLabel "B=>O conversion" testBO]



----------------------------------------------------------------
--main
main :: IO ()
main = do
    void (runTestTT tests)