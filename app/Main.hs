module Main (main) where

main :: IO ()
main = print "Hello, world!"
-- (fn xs (flatten (list (list xs ..) ..)) -> (list xs .. ..))
-- main :: IO ()
-- main =
--   putStrLn
--     . Display.display0
--     . Interpret.interpret (fromLeft $ Read.read "(flatten (list (list 1 2 3) (list 4 5 6) (list) (list a b c d e f g)))")
--     . Compile.compile (H.fromList [("xs", [AstC0.ZeroPlus 1, AstC0.Between 1 0, AstC0.Between 1 0])])
--     $ Read.read "(list (xs ..) ..)"