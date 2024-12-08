import Text.Parsec

import Common

main = do
    content <- readContent
    let Right equations = parse_ content
    putStrLn $ show $ solve1 equations
    putStrLn $ show $ solve2 equations

parse_ content =  parse p "" content
    where
        p = line_p `endBy` newline <* eof
        line_p = (,) <$> int_p <* char ':' <* spaces_p <*> (int_p `sepBy` spaces_p)
        int_p = readInt <$> (many1 $ oneOf "0123456789")
        spaces_p = many1 $ char ' '

solve1 = solve [(+), (*)]

solve2 = solve [(+), (*), concatNum]

solve ops equations = sum $ map fst $ filter isValid equations
    where
        isValid (target, (val : vals)) = isValid' target val vals

        isValid' target current [] = current == target
        isValid' target current (val : vals)
            | current > target = False
            | otherwise = foldr1 (||) $ map (\op -> isValid' target (op current val) vals) ops

concatNum s n = addZeros s n + n
    where
        addZeros s 0 = s
        addZeros s n = addZeros (s*10) (n `div` 10)
