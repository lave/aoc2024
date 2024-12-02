import Data.List
import Text.Parsec

import Common

main = do
    content <- readContent
    let Right lists = parse_ content
    putStrLn $ show $ solve1 lists
    putStrLn $ show $ solve2 lists

parse_ content = parse p "" content
    where
        p = (\[x, y] -> (x, y)) <$> transpose <$> line_p `endBy` newline <* eof
        line_p = int_p `sepBy` (many1 $ char ' ')
        int_p = readInt <$> (many $ oneOf "0123456789")

solve1 (l1, l2) = sum $ map abs diffs
    where
        diffs = zipWith (-) (sort l1) (sort l2)

solve2 (l1, l2) = sum $ map score l1
    where
        score n = n * (sum $ map (fromEnum . (== n)) l2)
    
