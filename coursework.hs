

-- chomp() selects first run from string
chomp :: String -> String 
chomp inString = takeWhile (== head inString) inString
-- end of chomp()

-- munch() selects first run (with length <= 9) from string
munch :: String -> String 
munch inString = take 9 (chomp inString) 
-- end of munch()

-- runs() splits string into list of runs
runs :: String -> [String]
runs inString = 
    recursiveFunc inString []
    where
        recursiveFunc inString list
            | length inString == 0 = list
            | otherwise = recursiveFunc (drop (length (munch inString)) inString) rest
                where rest = list ++ [(munch inString)]
-- end runs()

-- encode() zips character with its number of occurrences
encode :: String -> [(Char,Int)]
encode [] = []
encode inString = ((x !! 0),(length x)):(encode (drop (length x) (inString)))
    where (x:xs) = runs inString
 -- end of encode() 

-- flatten() converts tuples into a list of characters and integers
flatten :: [(Char,Int)] -> String
flatten [] = []
flatten (x:xs) = (fst x):(show (snd x) ++ flatten xs)
-- end flatten()

-- compress() uses previous functions to compress a string
compress :: String -> String
compress [] = []
compress inString = flatten (encode inString)
-- end of compress()

-- decode() unzips characters and the nummber of occurrences into a string
decode :: [(Char,Int)] -> String
decode [] = []
decode (x:xs) = ((take (snd x) (repeat (fst x)))) ++ decode xs
-- end of decode()

-- expand() converts list of occurrences into tuples
expand :: String -> [(Char,Int)]
expand [] = []
expand [x] = []
expand (x:y:xs) = (x,(fromEnum y - fromEnum '0')):(expand xs)
-- end of expand()

-- decompress() uses previous functions to decompress a string
decompress :: String -> String
decompress [] = []
decompress inString = decode (expand inString)
-- end of decompress()