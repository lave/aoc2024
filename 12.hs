import Data.Array
import Data.Function
import Data.List
import Data.Tuple
import qualified Data.Set as Set
import Text.Parsec

import Common

data Side = North | South | West | East deriving (Eq, Ord, Show)

main = do
    content <- readContent
    let Right garden = parse_ content
    putStrLn $ show $ solve1 garden
    putStrLn $ show $ solve2 garden

parse_ content =  parse p "" content
    where
        p = toArray <$> line_p `endBy` newline <* eof
        line_p = many $ oneOf (['A'..'Z'])


solve1 = solve onPlot (0, 0) calcScore
    where
        onPlot (area, perimeter) bounds = (area + 1, perimeter + length bounds)
        calcScore = uncurry (*)


solve2 = solve onPlot (0, []) calcScore
    where
        onPlot (area, allBounds) bounds = (area + 1, bounds ++ allBounds)
        calcScore (area, sides) = area * nSides
            where
                --  group unit sides by direction and constant coordinate (y for north- and south-facing, x for west and
                --  east)
                sidesN = groupBy ((==) `on` fst) $ sort $ map drop3rd $ filter ((==North) . trd3) sides
                sidesS = groupBy ((==) `on` fst) $ sort $ map drop3rd $ filter ((==South) . trd3) sides
                --  also swap coordinates of west- and east-facing sides, so that constant coordinate is always the
                --  first, and variable is the second (as a bonus resulting groups will also get sorted by variable
                --  coordinate)
                sidesW = groupBy ((==) `on` fst) $ sort $ map (swap . drop3rd) $ filter ((==West) . trd3) sides
                sidesE = groupBy ((==) `on` fst) $ sort $ map (swap . drop3rd) $ filter ((==East) . trd3) sides
                nSides = sum $ map countSidesForDirection [sidesN, sidesS, sidesW, sidesE]

                countSidesForDirection = sum . map countSidesForRow
                    where
                        -- here all side unit pieces are for one direction and row (first coordinate is the same), and
                        -- also sorted (by second coordinate)  - we just need to find amount of continuous chunks
                        countSidesForRow sides = length $ group xs'
                            where
                                xs = map snd sides
                                --  if we subtract incrementing value from array of sorted coordintates, for all
                                --  subsequent coordinates we get the same result - so it's easy to group. E.g. here's
                                --  two continuous sides:
                                --
                                --      3,4,5,8,9
                                --
                                --  after subtracting we get:
                                --
                                --      3,3,3,5,5
                                xs' = zipWith (-) xs [0..]

solve onPlot initState calcScore garden = score
    where
        (w, h) = bounds0 garden
        isOnMap (y, x) = x >= 0 && y >= 0 && x < w && y < h

        (_, score) = foldl floodFill (Set.empty, 0) $ indices garden

        floodFill (visited, score) p0 = (visited', score')
            where
                id = garden ! p0
                (visited', state) = fillStep (visited, initState) p0
                score' = score + calcScore state

                fillStep ctx@(visited, state) p@(y, x)
                    --  if we've already been there - return immediately
                    | Set.member p visited = ctx
                    --  otherwise call `onPlot` to account for this plot and try adjacent plots of same kind
                    | otherwise = foldl fillStep ctx' $ map drop3rd neighbours
                    where
                        --  all four sides of the current plot
                        sides = [(y-1, x, North), (y+1, x, South), (y, x-1, West), (y, x+1, East)]
                        --  split sides into bounds (with different regions) and neighbours (plots belonging to the same
                        --  region
                        (neighbours, bounds) = partition (\(y,x,_) -> isOnMap (y,x) && (garden ! (y,x) == id)) sides

                        ctx' = (Set.insert p visited, onPlot state bounds)
