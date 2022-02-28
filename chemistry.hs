import Data.List
import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad 
import Data.Char


newtype Parser a = Parser (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (Parser p) s = p s


instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Alternative Parser where
    empty = Parser $ \char -> empty
    (Parser cont) <|> (Parser cont') = Parser $ \char -> (cont char) <|> (cont' char)




instance Monad Parser where
  return x = Parser (\s -> [(x,s)])
  p >>=  q = Parser  (\s -> [(y,s'')
                            | (x,s') <- apply p s, 
                            (y,s'') <- apply (q x) s']) 



goParseN :: [Int] -> Int -> String -> [Int]
goParseN res n [] = reverse $ n:res
goParseN res n (h:t)
  | h `elem` "1234567890"  = goParseN res (n * 10 + ord h - ord '0') t
  | n == 0 = goParseN res 0 t
  | otherwise  = goParseN (n:res) 0 t

parseN = goParseN [] 0


main = (do 
    putStrLn ""
    putStrLn "Enter unbalanced chemical equations with spaces between molecules. When done, press enter."
    putStrLn "Following atoms by numbers is required, and indicates bonding." 
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn "Input Reactants:"
    reactants <- getLine
    print $ parseN reactants
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn "Input Products:"
    products <- getLine
    print $ parseN products
    coefMatrix = [[]]
    )
