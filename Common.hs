module Common where

import qualified Data.Array
import qualified Data.Set
import qualified Data.Either
import qualified System.Environment
import Text.Parsec

readSingleLineFile filename = do
    args <- System.Environment.getArgs
    content <- readFile $ head args
    let [line] = lines content
    return line

readContent = do
    args <- System.Environment.getArgs
    readFile $ head args

readInt s = read s :: Int

readsInt s = reads s :: [(Int, String)]

readsStr s = reads s :: [(String, String)]

readsChar [] = []
readsChar (' ' : s) = readsChar s
readsChar (c : s) = [(c, s)]

maybeDo r s =
    case res of
        [] -> [(Nothing, s)]
        [(r', s')] -> [(Just r', s')]
    where
        res = case s of
            [] -> []
            otherwise -> r s

firstDup l = firstDup' l Data.Set.empty 0
    where
        firstDup' [] _ _ = Nothing
        firstDup' (x : xs) xs' i
            | Data.Set.member x xs' = Just (i, x)
            | otherwise = firstDup' xs (Data.Set.insert x xs') (i + 1)

fst3 (x, _, _) = x
snd3 (_, y, _) = y
trd3 (_, _, z) = z

fromLeft_ = Data.Either.fromLeft undefined
fromRight_ = Data.Either.fromRight undefined

--int_p = readInt <$> (many $ oneOf "0123456789")

with_remainder p = (,) <$> p <*> getInput

toArray ls = Data.Array.listArray ((0, 0), (w-1, h-1)) (concat ls)
    where
        h = length ls
        w = length $ head ls

bounds0 m = (w+1, h+1)
    where
        ((0, 0), (w, h)) = Data.Array.bounds m
        
