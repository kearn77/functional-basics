module Eol
where

    -- Strip whitespace from the end of a line.
    rStrip :: String -> String
    rStrip = reverse . dropWhile(==' ') . reverse

    {--
    Returns true if a string ends in sentence ending 
    punctuation.
    --}
    puncEnd :: String -> Bool
    puncEnd [] = False
    puncEnd xs = last xs `elem` ['.','!','?']

    -- Merges two strings, accounting for punctuation.
    merge :: String -> String -> String
    merge x y
        |null x = y
        |null y = x
        |puncEnd x = x ++ "  " ++ y
        |otherwise = x ++ " " ++ y

    -- Represents text files as a list of lists.
    lnList :: String -> [[String]]
    lnList = map (words . rStrip) . lines

    -- Returns sublists that reflect a text file's paragraphs.
    getPars :: [[a]] -> [[a]]
    getPars xs
        |all null l2 = l1
        |otherwise = concat l1 : getPars (tail l2)
        where
            l1 = takeWhile (not . null) xs
            l2 = dropWhile (not . null) xs

    {--
    Returns line breaks in accordance with a provided
    accumulator.
    --}
    insertBreaks :: [String] -> String -> Int -> String
    insertBreaks [] s _ = if null s then "\n\n" else s
    insertBreaks xs@(x:xs') s acc
        |chars > acc = s ++ "\n" ++ insertBreaks xs "" acc
        |otherwise = insertBreaks xs' nextS acc
        where
            nextS = merge s x
            chars = length nextS

    {--
    Map insertBreaks to a list of lists, where each list
    represents a unique paragraph.
    --}
    mapBreaks :: [[String]] -> Int -> [String]
    mapBreaks x y = map (\a -> insertBreaks a "" y) x

    {--
    Given a list of strings, each of which represents a
    paragraph, map double newline's to the end of each
    string.
    --}
    insertPars :: [String] -> [String]
    insertPars [] = []
    insertPars xs = map (++ "\n\n") (init xs) ++ [last xs]

    {--
    Format a text file represented by a list of lists of
    strings.
    --}
    fmtText :: [[String]] -> Int -> [String]
    fmtText x y = insertPars $ mapBreaks x y

    {--
    Returns False if any word in a text file is greater
    than the accumulator.
    --}
    validAcc :: String -> Int -> Bool
    validAcc xs acc = all (<= acc) $ wordMax xs
        where
            wordMax x = map length (words x)

    -- Reformat a file with custom line lengths. 
    fmtFile :: FilePath  -> FilePath -> Int -> IO ()
    fmtFile f f' acc = do
        file <- readFile f
        if validAcc file acc then do
            let newBody = fmtText (getPars $ lnList file) acc
            writeFile f' (concat newBody)
        else 
            print "Invalid accumulator passed to fmtFile."