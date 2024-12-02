import Data.List
import Text.Parsec

import Common

main = do
    content <- readContent
    let Right reports = parse_ content
    putStrLn $ show $ solve1 reports
    putStrLn $ show $ solve2 reports

parse_ content = parse p "" content
    where
        p = line_p `endBy` newline <* eof
        line_p = int_p `sepBy` (many1 $ char ' ')
        int_p = readInt <$> (many $ oneOf "0123456789")

solve1 reports = sum $ map (fromEnum . isSafe) reports

isSafe report = (all (==1) diffs) || (all (== -1) diffs)
    where
        diffs = zipWith diff report (tail report)
        diff a b
            | a < b = if b - a > 3 then 0 else 1
            | a > b = if a - b > 3 then 0 else -1
            | otherwise = 0

solve2 reports = sum $ map (fromEnum . isSafe2) reports

isSafe2 report = any isSafe woLevel
    where
        woLevel = map removeNth [0 .. length report - 1]
        removeNth n = h ++ (tail t)
            where
                (h, t) = splitAt n report
                

