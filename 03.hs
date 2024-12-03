import Data.List
import Text.Parsec
import Debug.Trace

import Common

data Op = Mul Int Int
    | On
    | Off
    deriving (Eq, Show)

main = do
    content <- readContent
    putStrLn $ show $ solve1 content
    putStrLn $ show $ solve2 content


mul_p f = f <$> ((string "mul(") *> int_p) <*> ((char ',') *> int_p <* (char ')'))
int_p = readInt <$> (many $ oneOf "0123456789")

parse1 = parse (with_remainder p) ""
    where
        p = mul_p (*)

solve1 content = solve content 0
    where
        solve [] sum = sum
        solve cnt sum =
            case parse1 cnt of
                Left _ -> solve (tail cnt) sum
                Right (p, rem) -> solve rem (sum + p)


parse2 = parse (with_remainder p) ""
    where
        p = try on_p <|> try off_p <|> mul_p Mul
        on_p = On <$ string "do()"
        off_p = Off <$ string "don't()"

solve2 content = solve content 0 1
    where
        solve [] sum _ = sum
        solve cnt sum is_on =
            case parse2 cnt of
                Left _ -> solve (tail cnt) sum is_on
                Right (On, rem) -> solve rem sum 1
                Right (Off, rem) -> solve rem sum 0
                Right ((Mul a b), rem) -> solve rem (sum + a * b * is_on) is_on
