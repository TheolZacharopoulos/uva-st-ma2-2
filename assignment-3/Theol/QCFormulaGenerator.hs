module QCFormulaGenerator(main) where

import Test.QuickCheck
import Data.List
import Control.Monad

import Lecture3

prop_parsed :: Form -> Bool
prop_parsed f =
    let formula = show f in
        show (head $ parse formula) == formula

-- As mention in (http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html),
-- There is always a risk that a recursive generator like this may fail to terminate,
-- or produce very large results. To avoid this, recursive generators should always use the size control mechanism.
instance Arbitrary Form where
  arbitrary = sizedRandomForm

sizedRandomForm :: Gen Form
sizedRandomForm = sized rndForm

rndForm :: Integral a => a -> Gen Form
rndForm 0 =
    oneof [
        liftM Prop (resize (3) arbitrary),
        liftM Neg  (resize (3) arbitrary)
      ]

rndForm n | n > 0 =
    frequency
        [ (10, liftM  Prop  (resize (3) arbitrary))
        , (10, liftM  Neg   (resize (3) arbitrary))
        , (7,  liftM  Cnj   subforms)
        , (7,  liftM  Dsj   subforms)
        , (5,  liftM2 Impl  subform subform)
        , (5,  liftM2 Equiv subform subform)]
    where
        subform :: Gen Form
        subform = rndForm (n `div` 2)

        -- TODO: Refactor this.
        subforms :: Gen [Form]
        subforms = do
            fs <- vectorOf 1 subform
            return fs

options :: Args
options = Args
    { replay = Nothing
    , maxSuccess = 100
    , maxDiscardRatio = 100
    , maxSize = 100
    , chatty = True
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