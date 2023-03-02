import Tokenizer (parseTokens)

main :: IO ()
main = do
  let input = "abcdefg"
  case parseTokens input of
    Left err -> print err
    Right tokens -> print tokens
