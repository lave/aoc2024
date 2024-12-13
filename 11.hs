import qualified Data.Map as Map
import Data.Maybe
import Text.Parsec

import Common

main = do
    content <- readContent
    let Right row = parse_ content
    putStrLn $ show $ solve1 row
    putStrLn $ show $ solve2 row

parse_ content = parse p "" content
    where
        p = int_p `sepBy` (many1 $ char ' ')
        int_p = readInt <$> (many $ oneOf ['0'..'9'])

nDigits n
    | n < 10 = 1
    | otherwise = 1 + nDigits (n `div` 10)

solve1 row = length $ (iterate blink row) !! 25
    where
        blink row = concatMap transform row
            where
                transform n
                    | n == 0 = [1]
                    | even nd = [n `div` pow, n `mod` pow]
                    | otherwise = [n * 2024]
                    where
                        nd = nDigits n
                        pow = 10 ^ (nd `div` 2)


solve2 row = sum $ map (\n -> fst $ count n 75 Map.empty) row
    where
        count n i memo
            --  if this result has already been memoized - return it
            | isJust memoed = (fromJust memoed, memo)
            --  we've dug down to the initial state - return result (which is always 1)
            | i == 0 = (1, memo)
            --  transform rules - return count and new memoization cache
            | n == 0 = (count1, Map.insert key count1 memo1)
            | even nd = (count2, Map.insert key count2 memo2b)
            | otherwise = (count3, Map.insert key count3 memo3)
            where
                i' = i-1
                key = (n, i)
                memoed = Map.lookup key memo
                nd = nDigits n
                pow = 10 ^ (nd `div` 2)

                (count1, memo1) = count 1 i' memo

                (count2a, memo2a) = count (n `div` pow) i' memo
                (count2b, memo2b) = count (n `mod` pow) i' memo2a
                count2 = count2a + count2b

                (count3, memo3) = count (n*2024) i' memo
