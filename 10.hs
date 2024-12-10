import Data.Array
import qualified Data.Set as Set
import Text.Parsec

import Common

main = do
    content <- readContent
    let Right map_ = parse_ content
    let trailheads = map fst $ filter ((==0) . snd) $ assocs map_
    putStrLn $ show $ solve1 map_ trailheads
    putStrLn $ show $ solve2 map_ trailheads

parse_ content =  parse p "" content
    where
        p = toArray <$> line_p `endBy` newline <* eof
        line_p = map (\c -> if c == '.' then -1 else charToInt c) <$> (many $ oneOf ('.' : ['0'..'9']))

solve initScoredThing updateScoredThing map_ trailheads = sum $ map score trailheads
    where
        (w, h) = bounds0 map_
        isOnMap (y, x) = x >= 0 && y >= 0 && x < w && y < h

        score trailhead = Set.size $ makeStep trailhead 0 initScoredThing Set.empty

        makeStep pos@(y, x) alt scoredThing scoredThings
            | alt == 9 = Set.insert scoredThing' scoredThings
            | otherwise = foldr (\p' hikes' -> makeStep p' alt' scoredThing' hikes') scoredThings moves
            where
                alt' = alt + 1
                scoredThing' = updateScoredThing pos scoredThing
                moves = filter (\pos -> isOnMap pos && (map_ ! pos == alt')) [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]

solve1 = solve (-1,-1) (\pos scoredThing -> pos)

solve2 = solve [] (\pos scoredThing -> pos : scoredThing)
