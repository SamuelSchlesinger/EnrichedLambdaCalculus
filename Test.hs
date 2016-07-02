import Text.Parsec
import Parser
import Pretty
import AST

main = do
  c <- readFile "Test.lambda"
  let e = parse bindings "" c
  case e of
    Left err -> print err
    Right r  -> mapM_ (\(fst, snd) -> putStr fst >> putStr " = " >> putStr snd >> putStr "\n") $ map (\(fst, snd) -> (pretty fst, pretty snd)) r
