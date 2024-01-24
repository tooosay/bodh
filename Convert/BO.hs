module Convert.BO where
----------------------------------------------------------------
----- Binary <=> Octal conversion

bToO :: String -> String
bToO xs 
    | xs == [] = []
    | div3 == 0 = (bToO' (take 3 xs))++(bToO' (drop 3 xs))
    | otherwise = bToO $ (replicate (3 - div3) '0') ++ xs
    where
        div3 = (length xs) `rem` 3

bToO' :: String -> String
bToO' xs
    | xs == "000" = "0"
    | xs == "001" = "1"
    | xs == "010" = "2"
    | xs == "011" = "3"
    | xs == "100" = "4"
    | xs == "101" = "5"
    | xs == "110" = "6"
    | xs == "111" = "7"

oToB :: String -> String
oToB xs
    | xs == [] = []
    | otherwise = concatMap oToB' xs

oToB' :: Char -> String
oToB' x 
    | x == '0' = "000"
    | x == '1' = "001"
    | x == '2' = "010"
    | x == '3' = "011"
    | x == '4' = "100"
    | x == '5' = "101"
    | x == '6' = "110"
    | x == '7' = "111"

----------------------------------------------------------------
----- Binary <=> Decimal conversion
