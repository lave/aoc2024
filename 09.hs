import Data.Array
import Data.Maybe
import Text.Parsec

import Common

main = do
    content <- readContent
    let Right disk = parse_ content
    let chunks = toChunks disk
    putStrLn $ show $ solve1 chunks
    putStrLn $ show $ solve2 chunks

parse_ content =  parse p "" content
    where
        p = map charToInt <$> many (oneOf ['0'..'9']) <* newline <* eof

toChunks disk = toChunks' disk True 0 []
    where
        toChunks' [] _ _ chunks = reverse chunks
        toChunks' (b:bs) isFile id chunks
            | b == 0    = toChunks' bs isFile' id' chunks
            | otherwise = toChunks' bs isFile' id' (chunk : chunks)
            where
                isFile' = not isFile
                id' = id + fromEnum isFile
                chunk = (if isFile then id else -1, b)

solve1 chunks = checksum merged
    where
        n = length chunks
        --  scan list from both ends until we meet in the middle (actually scan two different lists from head until
        --  proper amount of elements are consumed from both lists)
        merged = merge chunks (reverse chunks) n []

        --  we've met at the middle - this is the last chunk; it's at the top of the both lists, but in the reversed list
        --  it may be modified (if it has been been partially moved), so we need to add the value from the reversed list
        --  to the result (and stop here, because there's no more files to move)
        merge chunks (r:_) 1 merged = reverse (r : merged)
        merge chunks@(l:ls) reversedChunks@(r:rs) n merged
            --  skip empty chunk on the right
            | rid == -1 = merge chunks rs (n-1) merged
            --  add non-empty chunk on the left to the result
            | lid /= -1 = merge ls reversedChunks (n-1) (l : merged)
            --  empty on the left and non-empty on the right
            --  if lengths are equal - consume both chunks and add right one to result
            | llen == rlen = merge ls rs (n-2) (r : merged)
            --  free block is larger - consume right block and shrink free blokc on the left
            | llen > rlen = merge ((lid, llen - rlen) : ls) rs (n-1) (r : merged)
            --  free block is smaller - consume it and shrink right block
            | otherwise = merge ls ((rid, rlen - llen) : rs) (n-1) ((rid, llen) : merged)
            where
                (lid, llen) = l
                (rid, rlen) = r

        checksum chunks = snd $ foldl checksum' (0, 0) chunks
            where
                checksum' (pos, s) (id, l) = (pos + l, s + s')
                    where
                        s' = sum $ zipWith (*) [pos..] $ replicate l $ if id == -1 then 0 else id


solve2 chunks = checksum defragged
    where
        --  unwrap chunks to blocks, building disk bitmap
        size = sum $ map snd chunks
        blocks = listArray (0, size-1) $ concatMap (uncurry $ flip replicate) chunks

        --  scan blocks from the right, detecting continuous chunks, then search for free space starting from the left,
        --  and move the chunk if it's found
        defragged = defrag blocks (size-1) 0 (-1) (listArray (1,9) $ replicate 9 0)

        defrag blocks i len id freeStarts
            --  reached beginning - return current map
            | i == 0 = blocks
            --  continue scanning new chunk
            | id' == id = defrag blocks i' (len+1) id freeStarts
            --  chunk is over - if it was free space, just ignore and start new chunk
            | id == -1 = defrag blocks i' 1 id' freeStarts
            --  chunk is over, it wasn't free space, and there's no free space for it on the left - ignore it and start new chunk
            | isNothing free = defrag blocks i' 1 id' $ freeStarts // [(len, size)]
            --  block is over, it wasn't free space, and there is free space for it on the left - move it
            | otherwise = defrag blocks' i' 1 id' $ freeStarts // [(len, ifree+len)]
            where
                i' = i-1
                id' = blocks ! i
                --  i-len+1 is the first block after which we should stop searching form free chunk of size `len`
                free = findFree (freeStarts ! len) (i-len+1) 0 False
                ifree = fromJust free

                --  blocks which are allocated to the new location of the file
                allocated = [(i, id) | i <- [ifree .. ifree+len-1]]
                --  blocks which are freed after moving current file to the free chunk
                freed = [(i, -1) | i <- [i+1 .. i+len]]
                blocks' = blocks // (allocated ++ freed)

                findFree j jmax cur_len isFree
                    --  found
                    | cur_len == len = Just $ j-len
                    --  not found
                    | j > jmax = Nothing
                    --  not empty - reset current empty chunk to 0
                    | blocks ! j /= -1 = findFree j' jmax 0 False
                    --  start new empty chunk
                    | not isFree = findFree j' jmax 1 True
                    --  continue current empty chunk
                    | otherwise = findFree j' jmax (cur_len+1) True
                    where
                        j' = j+1

        checksum blocks = sum $ map (\(i, id) -> if id == -1 then 0 else i * id) $ assocs blocks
