import Data.Array
import qualified Data.Set as Set
import Text.Parsec

import Common

data Direction = North | South | West | East deriving (Eq, Ord, Show)

main = do
    content <- readContent
    let Right map = parse_ content
    let start = findStart map
    putStrLn $ show $ solve1 map start
    putStrLn $ show $ solve2 map start

parse_ content =  parse p "" content
    where
        p = toArray <$> line_p `endBy` newline <* eof
        line_p = many $ oneOf ".#^"

findStart map = head $ filter (\p -> map ! p == '^') $ indices map

walk map pos dir = walk' map pos dir (speed dir) Set.empty
    where
        (w, h) = bounds0 map
        speed dir = case dir of
            North -> (-1, 0)
            South -> ( 1, 0)
            West ->  ( 0,-1)
            East ->  ( 0, 1)

        walk' map pos@(y, x) dir spd@(dy, dx) visited
            | x' < 0 || x' >= w || y' < 0 || y' >= h = (visited', False)
            | Set.member (x, y, dir) visited         = (visited, True)
            | obstacle  = walk' map pos  dir' (speed dir') visited
            | otherwise = walk' map pos' dir  spd          visited'
            where
                x' = x + dx
                y' = y + dy
                pos' = (y', x')
                dir' = case dir of
                    North -> East
                    East -> South
                    South -> West
                    West -> North
                obstacle = map ! pos' == '#'
                visited' = Set.insert (x, y, dir) visited

solve1 map start = length $ Set.map (\(x, y, _) -> (x, y)) visited
    where
        (visited, False) = walk map start North

solve2 map start = length $ filter isLoop $ filter (\i -> map ! i == '.') $ indices map
    where
        isLoop pos = snd $ walk map' start North
            where
                map' = map // [(pos, '#')]
