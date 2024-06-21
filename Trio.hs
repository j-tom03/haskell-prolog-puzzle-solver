import Data.List

generator2 :: [(String,String,String,String,String)]
generator2 =
    [ (n1,n2,n3,n4,n5)
    | n1 <- map show [123..987], distDigs n1, noZeroDig n1
    , n2 <- len2Digs n1, firstNotShared n1 n2 
    , n3 <- permutations n1
    , n4 <- len2Digs n1
    , n5 <- permutations n1
    ]

distDigs :: String -> Bool
distDigs n
    = length ("123456789" \\ n) == 6

len2Digs :: String -> [String]
len2Digs n1
    = [ drop 1 n1
      , take 2 n1
      , take 1 n1 ++ drop 2 n1
      , take 1 (drop 1 n1) ++ take 1 n1
      , take 1 (drop 2 n1) ++ take 1 n1
      , take 1 (drop 2 n1) ++ take 1 (drop 1 n1)
    ]

firstNotShared :: String -> String -> Bool
firstNotShared n1 n2 
    = take 1 n1 /= take 1 n2

noZeroDig :: String -> Bool
noZeroDig n
    = not ('0' `elem` n)

tester2 :: (String,String,String,String,String) -> Bool
tester2 (n1,n2,n3,n4,n5)
    =  subTrue (n1,n2,n3,n4,n5) && sumLessTwoThou (n1,n3,n5)

sumLessTwoThou :: (String,String,String) -> Bool
sumLessTwoThou (n1,n3,n5)
    = n1Int + n3Int + n5Int < 2000
    where
    n1Int = read n1
    n3Int = read n3
    n5Int = read n5

subTrue :: (String,String,String,String,String) -> Bool
subTrue (n1,n2,n3,n4,n5)
    = firstSub (n1,n2,n3) && secondSub (n3,n4,n5)

firstSub :: (String,String,String) -> Bool
firstSub (n1,n2,n3)
    = n1Int - n2Int == n3Int
    where
    n1Int = read n1
    n2Int = read n2
    n3Int = read n3

secondSub :: (String,String,String) -> Bool
secondSub (n3,n4,n5)
    = n3Int - n4Int == n5Int
    where
    n3Int = read n3
    n4Int = read n4
    n5Int = read n5

x_generator2 :: Int
x_generator2 =
    length [ t | t <- ts, t `elem` g ]
    where
        g = generator2
        ts =
            [ ("123","21","123","12","123")
            , ("162","26","261","12","621")
            , ("219","19","912","21","291")
            , ("329","92","932","32","239")
            , ("439","94","394","43","394")
            , ("549","95","945","95","945")
            , ("568","68","586","56","586")
            , ("769","67","679","97","796")
            , ("879","79","897","98","789")
            , ("987","79","789","79","789")
            ]

x_tester2 :: Int
x_tester2 =
    length [ t | t <- ts, tester2 t ]
    where
        ts =
            [ ("138","01","137","50","87")
            , ("143","01","142","52","90")
            , ("171","02","169","79","90")
            , ("152","03","149","54","95")
            , ("159","04","155","61","94")
            , ("161","05","156","63","93")
            , ("182","06","176","80","96")
            , ("151","07","144","57","87")
            , ("165","08","157","64","93")
            , ("174","09","165","71","94")
            ]

main :: IO ()
main = 
    print (filter tester2 generator2)