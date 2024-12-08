import Data.Array
import Text.Parsec

import Common

main = do
    content <- readContent
    let Right m = parse_ content
    putStrLn $ show $ solve1 m
    putStrLn $ show $ solve2 m

parse_ content = parse p "" content
    where
        p = toArray <$> line_p `endBy` newline <* eof
        line_p = many $ oneOf "AMSX"

solve1 m = sum counts
    where
        (w, h) = bounds0 m
        coords = (,) <$> [0..w-1] <*> [0..h-1]
        counts = map (wordsCount m) coords

wordsCount m coord = sum $ map fromEnum results
    where
        dirs = [(-1,-1), (0,-1), (1,-1),
                (-1, 0),          (1, 0),
                (-1, 1), (0, 1), (1, 1)]
        results = map (isWord "XMAS" m coord) dirs

isWord [] _ _ _ = True
isWord (char : chars) m (x, y) (dx, dy)
    | x < 0 || x >= w || y < 0 || y >= h = False
    | m ! (x, y) /= char = False
    | otherwise = isWord chars m (x+dx, y+dy) (dx, dy)
    where
        (w, h) = bounds0 m
    
    
solve2 m = sum $ map fromEnum results
    where
        word = "MAS"
        n = length word
        (w, h) = bounds0 m
        coords = (,) <$> [0..w-n] <*> [0..h-n]
        results = map (isXWord word m) coords

isXWord word m (x, y) =
       (isWord word m (x,     y) ( 1,1) || isWord word m (x+n-1, y+n-1) (-1,-1))
    && (isWord word m (x+n-1, y) (-1,1) || isWord word m (x,     y+n-1) ( 1,-1))
    where
        n = length word
