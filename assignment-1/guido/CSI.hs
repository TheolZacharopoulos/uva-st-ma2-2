module CSI (main) where

data Boy = Matthew | Peter | Jack | Arnold | Carl 
    deriving (Eq,Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

main = putStrLn "Hello, world"
