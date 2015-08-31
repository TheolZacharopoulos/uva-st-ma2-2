module Main (main) where

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

xor :: Bool -> Bool -> Bool

xor True x = not x
xor False x = x


says :: Boy -> Boy -> Bool

says Matthew Peter = True
says Matthew Jack = True
says Matthew Arnold = True

says Peter Matthew = True
says Peter Jack = True

says Jack x = if (says Matthew x) || (says Peter x) then False else True

says Arnold x = (says Matthew x) `xor` (says Peter x)

says Carl x = not (says Arnold x)

says x y = False



main :: IO ()
main = putStrLn $ show $ says Carl Jack