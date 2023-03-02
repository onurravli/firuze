import Tokenizer (parseTokens)

main :: IO ()
main = do
  let input = "abcdefg"
  let input2 = "printf(\"%d\\n\", 23) + 5 / 2 &&"
  case parseTokens input of
    Left err -> print err
    Right tokens -> print tokens
  case parseTokens input2 of
    Left err -> print err
    Right tokens -> print tokens