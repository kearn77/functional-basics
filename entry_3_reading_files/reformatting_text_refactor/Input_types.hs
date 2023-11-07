module Input_types
where    

    -- Strip trailing newLines.
    rStrip :: String -> String
    rStrip = reverse . dropWhile(=='\n') . reverse
    
    -- Merge two strings, accounting for punctuation.
    merge :: String -> String -> String
    merge x y
        |null x = y
        |last x `elem` ['?','!','.'] = x ++ "  " ++ y
        |otherwise = x ++ " " ++ y

    {--
    Merge a list of strings while less than an accumulator.
    N.B. this list of strings will be created with the words
    function.  E.G. "Hello, world!" -> ["Hello,","world!"].
    --}
    mergeWhile :: Int -> [String] -> String
    mergeWhile _ [] = []
    mergeWhile _ [x] = x ++ "\n\n"
    mergeWhile n (x:y:ys) 
        |length next > n = x ++ "\n" ++ mergeWhile n (y:ys)
        |otherwise = mergeWhile n (next:ys)
        where
            next = merge x y

    {--
    Returns a list of lists of strings, where each list is
    a paragraph from the source formatting.
    --}
    getPars :: [String] -> [[String]]
    getPars [] = []
    getPars (x:xs)
        |not (null x) = [x] : getPars xs
        |otherwise = getPars xs

    {--
    Composition of the above functions.  Given an
    accumulator and a string, returns a string with line
    breaks in accordance with the accumulator.
    --}
    fmtFile :: Int -> String -> String
    fmtFile n =
        rStrip .
        concatMap (mergeWhile n) .
        concatMap (map words) . 
        getPars . 
        lines

    -- Determines whether n is a valid accumulator.
    validAcc :: Int -> String -> Bool
    validAcc n = all ((<n) . length) . words

    -- Write Txt to a file.
    writeTxt :: Int -> FilePath -> FilePath -> IO ()
    writeTxt n f f' = do
        file <- readFile f
        if validAcc n file then
            writeFile f' (fmtFile n file)
        else
            print "Invalid accumulator passed to writeTxt."