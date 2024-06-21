import Data.List

generator1 :: [(Int, Int, Int, Int)]
generator1 
    = [ (hr, mn, dy, mt)
    | hr <- [0..23]
    , mn <- [0..59]
    , dy <- [1..31]
    , mt <- [1..12]
    , validDate (hr, mn, dy, mt)
    ]
    
validDate :: (Int,Int,Int,Int) -> Bool
validDate (hr, mn, dy, mt)
    | mt `elem` [9,4,6,11] && dy<31 = True
    | mt `elem` [1,3,5,7,8,10,12] && dy<32 = True
    | mt == 2 && dy<29 = True
    | otherwise = False
    
tester1 :: (Int,Int,Int,Int) -> Bool
tester1 (hr, mn, dy, mt)
    | isMagic (hr, mn, dy, mt) && isMagic tomorrow && avgCount==minDayCount = True
    | otherwise = False
    where
    tomorrow = dayLater (hr, mn, dy, mt)
    avgCount = avgSegCount (hr, mn, dy, mt) tomorrow
    minDayCount = countSeg (dayMinLater (hr, mn, dy, mt))

isMagic :: (Int,Int,Int,Int) -> Bool
isMagic (hr, mn, dy, mt)
    | segPrime (hr, mn, dy, mt) && distinctDigits (hr, mn, dy, mt) = True
    | otherwise = False

distinctDigits :: (Int,Int,Int,Int) -> Bool
distinctDigits (hr, mn, dy, mt)
    | length remaining == 2 = True
    | otherwise = False
    where 
    remaining = "0123456789" \\ digitStr (hr, mn, dy, mt)

digitStr :: (Int,Int,Int,Int) -> String
digitStr (hr, mn, dy, mt)
    = hrs ++ mins ++ days ++ months
    where 
    hrs = ensureDouble hr
    mins = ensureDouble mn
    days = ensureDouble dy
    months = ensureDouble mt
    
ensureDouble :: Int -> String
ensureDouble n
    | n<10 = "0"++show n
    | otherwise = show n 

dayLater :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
dayLater (hr, mn, dy, mt)
    | dy==31 && mt==12 = (hr, mn, 1, 1)
    | dy == maxDay mt = (hr, mn, 1, mt+1)
    | otherwise = (hr, mn, dy+1, mt)
    
minLater :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
minLater (hr, mn, dy, mt)
    | mn==59 && hr==23 = (00, 00, day, mon)
    | mn==59 && hr<23 = (hr+1,00, dy, mt)
    | otherwise = (hr,mn+1,dy,mt)
      where (_,_,day,mon) = dayLater (hr, mn, dy, mt)

dayMinLater :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
dayMinLater (hr, mn, dy, mt)
    = minLater (dayLater (hr, mn, dy, mt))

maxDay :: Int -> Int
maxDay mt
    | mt `elem` [9,4,6,11] = 30
    | mt `elem` [1,3,5,7,8,10,12] = 31
    | otherwise = 28

segPrime :: (Int,Int,Int,Int) -> Bool
segPrime (hr, mn, dy, mt)
    = prime (countSeg (hr, mn, dy, mt))

countSeg :: (Int,Int,Int,Int) -> Int
countSeg (hr, mn, dy, mt)
    = countSegInt hr + countSegInt mn + countSegInt dy + countSegInt mt

avgSegCount :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Int
avgSegCount (hr1, mn1, dy1, mt1) (hr2, mn2, dy2, mt2)
    | total `mod` 2 == 0 = total `div` 2
    | otherwise = -1
      where total=(countSeg (hr1, mn1, dy1, mt1) + countSeg (hr2, mn2, dy2, mt2))

countSegInt :: Int -> Int
countSegInt n
    = countSegSingle (n `mod` 10) + countSegSingle (n `div` 10)

countSegSingle :: Int -> Int
countSegSingle n
    | n == 0 = 6
    | n == 1 = 2
    | n == 2 = 5
    | n == 3 = 5
    | n == 4 = 4
    | n == 5 = 5
    | n == 6 = 6
    | n == 7 = 3
    | n == 8 = 7
    | n == 9 = 6

prime :: Int -> Bool
prime 
    = not . factorisable 2 
    
factorisable :: Int -> Int -> Bool
factorisable f n
    | f * f <= n = n `mod` f == 0 || factorisable (f+1) n
    | otherwise = False

x_generator1 :: Int
x_generator1 =
    length [ t | t <- ts , t `elem` g ]
    where
    g = generator1
    ts =
        [ ( 2 ,15 ,14 ,11)
        , ( 4 ,31 ,27 , 9)
        , ( 6 ,47 ,10 , 8)
        , ( 9 , 3 ,23 , 6)
        , (11 ,19 , 6 , 5)
        , (13 ,35 ,19 , 3)
        , (15 ,51 , 2 , 2)
        , (18 , 6 ,16 ,12)
        , (20 ,22 ,29 ,10)
        , (22 ,38 ,11 , 9)
        ]

x_tester1 :: Int
x_tester1 =
    length [ t | t <- ts , tester1 t ]
    where
    ts =
        [ ( 6 ,59 ,17 ,24)
        , ( 6 ,59 ,17 ,34)
        , ( 6 ,59 ,27 ,14)
        , ( 6 ,59 ,27 ,41)
        , ( 8 ,59 ,12 ,46)
        , (16 ,59 , 7 ,24)
        , (16 ,59 , 7 ,42)
        , (16 ,59 , 7 ,43)
        , (16 ,59 ,27 ,40)
        , (18 ,59 , 2 ,46)
        ]

main :: IO ()
main = 
    print (filter tester1 generator1)