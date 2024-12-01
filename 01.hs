import Data.List

import Common

main = do
    content <- readFile "01.input"
    let pairs = parse content
    let lists = (map fst pairs, map snd pairs)
    putStrLn $ show $ solve1 lists
    putStrLn $ show $ solve2 lists


parse = map (head . parseLine) . lines
    where
        parseLine s = [(n1, n2) |
                (n1, s1) <- readsInt s,
                (n2, "") <- readsInt s1]

solve1 (l1, l2) = sum $ map abs diffs
    where
        diffs = zipWith (-) (sort l1) (sort l2)

solve2 (l1, l2) = sum $ map score l1
    where
        score n = n * (sum $ map (fromEnum . (== n)) l2)
    
