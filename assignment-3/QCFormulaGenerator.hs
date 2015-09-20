-- A QuickCheck Formula Generator.
module QCFormulaGenerator (main) where

import Test.QuickCheck
import Data.List
import Control.Monad

import Lecture3

-- Options
propNumber = 4
negationDepth = 3

-- Preconditions: We do not have any preconditions
-- Postconditions:
-- Returns true iff:
--      Originally generated formula and its stringified and then parsed version be equivalent
prop_parsed :: Form -> Bool
prop_parsed form =
    let formulaStr = show form in
        show (head $ parse formulaStr) == formulaStr

-- As mentioned in (http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html),
-- There is always a risk that a recursive generator like this may fail to terminate,
-- or produce very large results. To avoid this, recursive generators should always use the size control mechanism.
instance Arbitrary Form where
  arbitrary = sizedRandomForm

sizedRandomForm :: Gen Form
sizedRandomForm = sized rndForm

rndForm :: Integral a => a -> Gen Form
rndForm 0 =
    oneof [
        elements [T, F],
        liftM Prop (elements [1..propNumber]),
        liftM Neg  (resize (negationDepth) arbitrary)
      ]

rndForm n | n > 0 =
    oneof [
        elements [T, F],
        liftM  Prop  (elements [1..propNumber]),
        liftM  Neg   (resize (negationDepth) arbitrary),
        liftM  Cnj   (listOf subform),
        liftM  Dsj   (listOf subform),
        liftM2 Impl  subform subform,
        liftM2 Equiv subform subform
      ]
    where
        -- Generates a form and divides n to control the size of recursive depth.
        subform :: Gen Form
        subform = rndForm (n `div` 8)

---------------------------------
-- Test runner options
options :: Args
options = Args {
    replay = Nothing,
    maxSuccess = 100,
    maxDiscardRatio = 100,
    maxSize = 100,
    chatty = True
}

runTests :: Args -> IO ()
runTests args = do
    f prop_parsed "prop_parsed OK?"
    where
        f prop str = do
            putStrLn str
            quickCheckWithResult args prop
            return ()

main :: IO ()
main = do
    runTests options