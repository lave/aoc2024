import Data.Array
import Data.Function
import Data.List
import Text.Parsec
import Debug.Trace

import Common

main = do
    content <- readContent
    let Right grid = parse_ content
    putStrLn $ show $ solve1 grid
    putStrLn $ show $ solve2 grid

parse_ content =  parse p "" content
    where
        p = toArray <$> line_p `endBy` newline <* eof
        line_p = many $ oneOf $ "." ++ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

solve antinodesForPair grid = length $ nub antinodes
    where
        (w, h) = bounds0 grid
        groups = groupBy ((==) `on` snd) $ sortOn snd $ filter ((/='.') . snd) $ assocs grid
        antinodes = concatMap antinodesForGroup groups
        antinodesForGroup group = concatMap (uncurry $ antinodesForPair isOnMap) pairs
            where
                positions = map fst group
                pairs = [(p1, p2) | p1 <- positions, p2 <- positions, p1 < p2]
                isOnMap (y, x) = x >= 0 && y >= 0 && x < w && y < h


solve1 = solve closest
    where
        closest isOnMap (y1, x1) (y2, x2) = filter isOnMap [(2*y1-y2, 2*x1-x2), (2*y2-y1, 2*x2-x1)]

solve2 = solve withHarmonics
    where
        withHarmonics isOnMap p1 p2 = (scan p1 p2) ++ (scan p2 p1)
            where
                scan (y1, x1) (y2, x2) = scan' (y1, x1) (y1-y2, x1-x2) []
                    where
                        scan' p@(y, x) dp@(dy, dx) result
                            | isOnMap p = scan' (y+dy, x+dx) dp (p : result)
                            | otherwise = result
