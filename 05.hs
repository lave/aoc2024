import Data.List
import Data.Maybe
import Text.Parsec

import Common

main = do
    content <- readContent
    let Right (rules, updates) = parse_ content
    let (good, bad) = split rules updates
    putStrLn $ show $ solve1 good 
    putStrLn $ show $ solve2 rules bad

parse_ content = parse p "" content
    where
        p = (,) <$> (rule_p `endBy` newline) <* newline <*> (update_p `endBy` newline) <* eof
        rule_p = (,) <$> int_p <* char '|' <*> int_p
        update_p = int_p `sepBy` char ','
        int_p = readInt <$> (many1 $ oneOf "0123456789")

split rules updates = partition isGood updates
    where
        isGood update = all isRuleGood rules
            where
               isRuleGood (l, r)
                    | isJust il && isJust ir = fromJust il < fromJust ir
                    | otherwise = True
                    where
                        il = elemIndex l update
                        ir = elemIndex r update

middle l = l !! (length l `div` 2)


solve1 updates = sum $ map middle updates


solve2 rules bad = sum $ map (middle . sort) bad
    where
        sort update = foldr (insertBy cmp) [] update
        --  luckuly there's comparison rules for all pairs of values - so we can just search the rule for this two
        --  values, no need to worry about transitivity
        cmp a b
            | ruleExists a b = LT
            | ruleExists b a = GT
            | otherwise = EQ
            where ruleExists a b = elem (a, b) rules
