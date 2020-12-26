length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1+length' xs 

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs





quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort[a | a <- xs, a <= x]
        biggerSorted = quicksort[a | a <- xs, a > x]
    in smallerSorted ++ [x] ++  biggerSorted

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)



-------------------------------------------------------------

--课堂实现

---------------------------------------------------------
head' :: [a] -> a
head' [] = error "empty list";
head' (x:xs) = x



headSecond :: [a] -> a
headSecond [] = error "empty list"
headSecond [x] = error "only one element"
headSecond [x,y] = y
headSecond (x:y:z) = headSecond [x,y]



last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (x:xs) = last' xs
-- 注意：错误写法： last' (x:xs) = last' [xs]
-- 此处的被":" 的 xs 本来就是list


lastSecond :: [a] -> a
lastSecond [] = error "empty list"
lastSecond [x] = error "only one element"
lastSecond [x, y] = x
lastSecond (x:y:xs) = lastSecond (y:xs)
-- 此处的":" x,y是变量；xs是list


-- ch1 :: [a] ->  [b] 这里的变量必不能是一样的
ch1 :: [a] -> [a]
ch1 [] = error "empty list"
ch1 [x] = [x]
ch1 (x:xs) = xs ++ [x]


--删掉最后一个元素
delLast :: [a] -> [a]
delLast [] = error "empty list"
delLast [x] = []
delLast (x:xs) = x : delLast xs


--将最后一个数字移至开头
ch2 :: [a] -> [a]
ch2 [] = error "empty list"
ch2 [x] = [x]
ch2 (x:xs) = [last' xs] ++ delLast xs

--置换开头和最后一个元素
ch3 :: [a] -> [a]
ch3 [] = error "empty list"
ch3 [x] = [x]
ch3 [x,y] = [y, x]
ch3 (x:y:xs) = [last' (y:xs)]  ++ delLast (y:xs) ++ [x]


ch3' :: [a] -> [a]
ch3' [] = []
ch3' [x] = [x]
ch3' (x:xs) = 
    let (f, g) = swap x xs 
    in f:g
    where swap k [y] = (y, [k])
          swap k (y:ys) = let (n,m) = swap k ys in (n, y:m)


-- 删除数组中指定值
delVal :: (Eq) a => a -> [a] -> [a]
delVal elem [] = []
delVal elem (x:xs)
 | elem == x = delVal elem xs
 | otherwise = x : delVal elem xs


--删除与第一个元素值相同的值
get :: (Eq) a => [a] -> [a]
--get [] = error "empty list"
get [] = []
get (x:xs) = x : get ( delVal x xs )



get1 :: (Eq) a => [a] -> [a]
--get [] = error "empty list"
get1 [] = []
get1 (x:xs)
 | delVal x xs == xs = x : get1 xs
 | otherwise = get1 ( delVal x xs )


dump1 :: (Eq a) => [a] -> [a]
dump1 []  = []
dump1 [x] = [x]
dump1 (x:xs) = x: [k | k <- dump1(xs), k /= x]


--删除与第一个元素值相同的值,包括第一个元素
dump2 :: (Eq a) => [a] -> [a]
dump2 [] = []
dump2 [x] = [x]
dump2 (x:xs)
    | [k | k <- xs, k==x] == []  =  x:(dump2 xs)
    | otherwise = dump2[k | k <- (xs) , k /= x]


getLen :: [a] -> Int
getLen [] = 0
getLen [x] = 1
getLen (x:xs) = 1 + getLen xs


delitca :: [a] -> ([a],[a])
delitca [] = ( [],[] )
delitca (x:xs) = ( [ (x:xs)!!k | k<-[0..(getLen (x:xs))`div`2-1] ], [ (x:xs)!!k | k<-[ (getLen (x:xs))`div`2..(getLen xs)] ] )



perevorot :: [Int] -> [Int]
perevorot [] = []
perevorot [x] = [x]
perevorot (x:xs) = perevorot xs ++ [x]

swaps :: [Int] -> ([Int],[Int])
swaps [] = ([],[])
swaps [x] = ([x],[])
swaps x = razb x (length x `div` 2)
 where razb (y:ys) 1 = ([y],perevorot ys)
       razb (y:ys) z = let (n,m) = razb ys (z-1) in (n++[y],m)


mygates :: [a] -> [a]
mygates [] = []
mygates x = let n = mylistlength x
                reversedx = myreverse x
                a = myreverse (mytakefirstnchildren (if (n `mod` 2 == 1)
                then  (n `div` 2 + 1) else (n `div` 2)) x)
                b = mytakefirstnchildren (n `div` 2) reversedx
                in a ++ b
                where myreverse [] = []
                      myreverse [x] = [x]
                      myreverse (x:xs) = myreverse xs ++ [x]
                      mytakefirstnchildren 0 x = []
                      mytakefirstnchildren 1 (x:xs) = [x]
                      mytakefirstnchildren n (x:xs) = x:(mytakefirstnchildren (n-1) xs)
                      mylistlength [] = 0
                      mylistlength [x] = 1
                      mylistlength (x:xs) = mylistlength(xs) + 1


myhometask :: [a] -> [a]
myhometask [] = []
myhometask [x] = [x]
myhometask (x:xs) = let len = mylen (x:xs)
                        half = (len + 1) `div` 2
                        (l,r) = mysplit half (x:xs)
                    in myreverse(l)++myreverse(r)
                    where mylen [] = 0
                          mylen [x] = 1
                          mylen (x:xs) = 1 + mylen xs
                          mysplit 1 (x:xs) = ([x],xs)
                          mysplit n (x:xs) = let (l,r) = mysplit (n - 1) xs
                                             in (x:l,r)
                          myreverse [x] = [x]
                          myreverse (x:xs) = myreverse(xs)++[x]


middleSplitThenFlip :: [a] -> [a]
middleSplitThenFlip [] = []
middleSplitThenFlip zs = list
    where
        list = (rev beg) ++ mid ++ (rev end)

        (beg, mid, end) = go zs zs
        go (x:xs) (_:_:ys) = 
            let 
                (beg, mid, end) = go xs ys 
            in 
                (x:beg, mid, end)
        go (x:xs) [_] = ([], [x], xs)
        go (x:xs) []  = ([], [], x:xs)

        rev [] = []
        rev [x] = [x]
        rev (x:xs) = rev xs ++ [x]


map' :: (a->b) -> [a] -> [b]
map' _[] = []
map' f (x:xs) = f x:map' f xs 


filter' :: (a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    |p x        = x : filter' p xs
    |otherwise  = filter' p xs


doubleMap :: (Double->Double) -> (Double->Double) -> Double -> Double
doubleMap f g c = (g.f) c


tw :: (a->b->c) -> (b->a->c)
tw  f = g
  where g x y = f y x


reca :: ((a -> c) -> t1 -> t2) -> (b -> c) -> (a -> b) -> t1 -> t2
reca f g w x =f(g.w)x  


fun4 :: (a->a)->(a->a)->a->a
fun4 f g x = g (f x)


isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)
isprefix::Eq a => [a] -> [a] -> Bool
isprefix [] _ = True
isprefix _ [] = False
isprefix (x:xs) (y:ys) = x == y && isprefix xs ys

del' :: Eq a => [a] -> [a] -> [a]
del' [] ys = ys
del' _ [] = []
del' xs (y:ys)
    | isprefix xs (y:ys) = mydrop (length xs) (y:ys)
--    | isprefix xs (y:ys) = del xs (mydrop (length xs) (y:ys))
    | otherwise = y : del' xs ys

mydrop' :: Int->[a]->[a]
mydrop' n xs | n<=0 = xs
mydrop' _ [] = []
mydrop' n (_:xs) = mydrop' (n-1) xs

sum1 :: (Num a) => [a] -> a
sum1      =  foldl (+) 0
 
product1 :: (Num a) => [a] -> a
product1  =  foldr (*) 1

concat1 :: [[a]] -> [a]
concat1  =  foldr (++) []

minimum1 :: (Ord a) => [a] -> a
minimum1 [x]     =  x
minimum1 (x:xs)  =  min x (minimum1 xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x]     =  x
foldr1 f (x:xs)  =  f x (foldr1 f xs)

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f [x]         =  x
foldl1 f (x:(y:ys))  =  foldl1 f (f x y : ys)
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x0 []      =  x0
foldr f x0 (x:xs)  =  f x (foldr f x0 xs)
 
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x0 []      =  x0
foldl f x0 (x:xs)  =  foldl f (f x0 x) xs



sort2 ::  [a]->[a]
sort2 [] = []
sort2 [x] = [x]
sort2 (x1 : x2 : xs)
    | (x1<x2) =   let (a:as) = sort2 (x1:xs) in a: sort2 (x2:as)   
    | otherwise = let (a:as) = sort2 (x2:xs) in a: sort2 (x1:as)





length1 :: [a] -> Int
length1 [] = 0
length1 (x:xs) = length1 xs + 1


length2 :: [a] -> Int
length2 xs = let length2 n [] = n
                 length2 n (x:xs) = len


elem2' :: (Eq a) => a -> [a] -> Bool
elem2' a [] = False
elem2' a (x:xs)
    | a == x    = True
    | otherwise = a `elem2'` xs



-- quicksort :: [a] -> [a]
-- quicksort [] = []
-- quicksort (x:xs) = 
--     let smallerSorted = quicksort [a | a <- xs, a <= x]
--         biggerSorted = quicksort [a | a <- xs, a > x]
--     in smallerSorted ++ [x] ++ biggerSorted

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort  [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted


applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith1 :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith1 f _ [] = []
zipWith1 f [] _ = []
zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys

map1 :: (a->b) -> [a] -> [b]
map1 _ [] = []
map1 f (x:xs) = f x : map1 f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f [] = []
filter1 f (x:xs)
    | f x = x : filter1 f xs
    | otherwise = filter1 f xs


test1 :: (Integral a ) => a
test1 = head (filter1 p [100000, 99999..])
    where p x = x `mod` 3829 == 0

test2 :: (Integral a ) => a
test2 =  sum (takeWhile (<10000)( filter odd (map (^2) [1..])))

test2_1 :: (Integral a ) => a
test2_1 =  sum (takeWhile (<10000)( [x | x<-[m^2 | m<-[1..]] ,odd x] ))


test3 :: (Integral a ) => a -> [a]
test3 1 = [1]
test3 x
    | x `mod` 2 == 0    = x: test3 (x `div` 2)
    | x `mod` 2 /= 0    = x: test3 (x*3 + 1)

elem2_1 :: (Eq a) =>a -> [a] -> Bool  -- 声明Eq a ，因为后面需要比较
elem2_1 y ys = foldl (\acc x ->  if x==y then True else acc) False ys

mylast :: [a] -> a
mylast [] = error"empty"
mylast [x] = x
mylast (x:xs) = mylast xs

del_last:: [a] -> [a]
del_last [] = error "empty"
del_last [x] = []
del_last (x:xs) = [x] ++ del_last xs 

change:: [a] -> [a]
change [] = error "empty"
change [x] = []
change (x:xs) = [mylast xs] ++ del_last xs ++ [x]

del_func:: (a->Bool) -> [a] -> [a]
del_func f [] = []
del_func f (x:xs)
    | f x  = x : del_func f xs
    | otherwise = del_func f xs


del_num :: (Eq ) a =>a -> [a] ->[a] -- 当函数中出现了 比较相等性的逻辑操作时，需要添加Eq可判断相等性类型的声明 tip:可比较大小性Ord不包含可相等性Eq
del_num cxd [] = []
del_num cxd (x:xs)
    | cxd == x = del_num cxd xs
    | otherwise =  x : del_num cxd xs


del_dup ::(Eq) a => [a] -> [a] --去重
del_dup [] = []
del_dup (x:xs) = x : del_dup(del_num x xs)


del_dup_2 :: (Eq)t=>[t]->[t]
del_dup_2 [] = []
del_dup_2 (x:xs)
    | del_num x xs == xs = x:del_dup_2 xs
    | otherwise  = del_dup_2 (del_num x xs)

-- divid:: [a] ->[[a]]
-- divid xs = [[xs!!k|k<-[0..length(xs)`div`2-1]],[xs!!k|k<-[length(xs)`div`2..length(xs)-1]]]
divid::[a] ->[[a]]
divid xs = [[xs !! k | k<-[0..length(xs) `div`2 - 1] ],[xs!!k | k<-[length(xs) `div`2..length(xs)-1]]]


reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 [x] = [x]
reverse1 [x,y] = [y,x] 
reverse1 (x:xs) = reverse1(xs) ++ [x]


divid_turn::[a] ->[a]
divid_turn xs = [xs !! k | k<- reverse1[0..length(xs) `div`2 - 1]] ++ [xs!!k | k<-reverse1 [length(xs) `div`2..length(xs)-1] ]


mygates :: [a] -> [a]
mygates [] = []
mygates x = let n = mylistlength x
                reversedx = myreverse x
                a = myreverse (mytakefirstnchildren (if (n `mod` 2 == 1)
                then  (n `div` 2 + 1) else (n `div` 2)) x)
                b = mytakefirstnchildren (n `div` 2) reversedx
                in a ++ b
                where myreverse []     = []
                      myreverse [x]    = [x]
                      myreverse (x:xs) = myreverse xs ++ [x]
                      mytakefirstnchildren 0 x = []
                      mytakefirstnchildren 1 (x:xs) = [x]
                      mytakefirstnchildren n (x:xs) = x:(mytakefirstnchildren (n-1) xs)
                      mylistlength []     = 0
                      mylistlength [x]    = 1
                      mylistlength (x:xs) = mylistlength(xs) + 1


mygate :: [a] -> [a]
mygate [] = []
mygate x = let n = mylength x
               reverselist = myreverse x
               m = if(n `div` 2 == 1) then (n`div`2 + 1) else (n`div`2)
               t = n `div` 2
               a = myreverse(mytake m x)
               b = mytake t reverselist
               in a ++ b               
               where mytake _ [] = []
                     mytake 0 xs = []
                     mytake 1 (x:xs) = [x]
                     mytake n (x:xs) = x : (mytake (n-1) xs)
                     mylength [] = 0
                     mylength (x:xs) = 1 + mylength xs
                     myreverse [] = []
                     myreverse [x] = [x]
                     myreverse (x:xs) = myreverse xs ++ [x]


middleSplitThenFlip :: [a] -> [a]
middleSplitThenFlip [] = []
middleSplitThenFlip zs = list
    where
        list = (rev beg) ++ mid ++ (rev end)

        (beg, mid, end) = go zs zs
        go (x:xs) (_:_:ys) =
            let
                (beg, mid, end) = go xs ys
            in
                (x:beg, mid, end)
        go (x:xs) [_] = ([], [x], xs)
        go (x:xs) []  = ([], [], x:xs)

        rev []     = []
        rev [x]    = [x]
        rev (x:xs) = rev xs ++ [x]


filter11 :: (a -> Bool) -> [a] -> [a]
filter11 _[] = []
filter11 f (x:xs)
    | f x  = x: filter11 f xs
    | otherwise = filter11 f xs

--错误写法 map11 :: (a->b) -> [a] -> [a]
map11 :: (a->b) -> [a] -> [b]
map11 _ [] = []
map11 f (x:xs) = f x:map11 f xs


isPrefix::Eq a => [a] -> [a] -> Bool --前缀
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys


mydrop :: Int->[a]->[a]
mydrop n xs | n<=0 = xs
mydrop _ [] = []
mydrop n (_:xs) = mydrop (n-1) xs


del2 :: Eq a => [a] -> [a] -> [a]
del2 [] ys = ys
del2 _ [] = []
del2 xs (y:ys)
--  | isPrefix xs (y:ys) = mydrop (length xs) (y:ys)
    | isPrefix xs (y:ys) = del2 xs (mydrop (length xs) (y:ys))
    | otherwise = y:del2 xs ys

--区间删除
del22 :: Eq a => [a] -> [a] -> [a]
del22 _ [] = [] 
del22 [] xs = xs
del22 xs (y:ys)
    | isPrefix xs (y:ys) = del22 xs (mydrop (length xs) (y:ys))
    | otherwise =  y: del22 xs ys
  
mininum' :: (Ord a) => [a] -> a
mininum' [] = error "empty"
mininum' [x] = x
mininum' (x:y:xs)
    | x <= y  = mininum' (x:xs)
    | otherwise = mininum' (y:xs)

foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' f n []     =  n
foldr' f n (x:xs)  =  f x (foldr' f n xs)

foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' f n [] = n
foldl' f n (x:xs) = foldl' f (f n x) xs


sum' :: (Num a) => [a] -> a
sum' xs = foldl' (+) 0 xs 

sum'' :: (Num a) => [a] -> a
sum'' = foldl' (+) 0


product1 :: (Num a) => [a] -> a -- 求list中元素乘积之和
product1 xs =  foldr' (*) 1 xs

concat1 :: [[a]] -> [a]
concat1 xs = foldr (++) [] xs


myquickSort:: (Ord a) => [a] -> [a]
myquickSort [] = []
myquickSort (x:xs) = 
    let leftSort = myquickSort [m | m<-xs, m <= x] 
        rightSort = myquickSort [n | n<-xs, n>x]
    in leftSort ++ [x] ++ rightSort

sort2 :: (Ord) a => [a] -> [a] --去掉大的元素开始排列，再取出最小值继续排
sort2 []  = []
sort2 [x] = [x]
sort2 (x:y:xs)
    | (x < y) = let (a:as) = sort2(x:xs) in a:sort2(y:as)
    | otherwise = let (a:as) = sort2(y:xs) in a:sort2(x:as)

-- 冒泡排序

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (x:xs) = last' xs

init' :: [a] -> [a]
init' [] = error "empty list"
init' [x] = []
init' (x:xs) = x:init'(xs)

swaps :: Ord a => [a] -> [a]
swaps [] = []
swaps [x] = [x]
swaps (x1:x2:xs)
    | x1 > x2 = x2 : swaps(x1:xs)
    | otherwise = x1 : swaps(x2:xs)

bubblesort :: Ord a => [a] -> [a]
bubblesort [] = []
bubblesort [x] = [x]
bubblesort xs = bubblesort(init' tmp) ++ [last' tmp]
    where
        tmp = swaps xs



-- 判断list中是否有与首个元素相同的元素
dup_1 :: Eq a => [a] -> Bool
dup_1 []         = False
dup_1 [x]        = False
dup_1 (x1:x2:xs) = x1 == x2 || dup_1(x1:xs)

-- 判断list中是否有重复的元素
dup_l :: Eq a => [a] -> Bool
dup_l []     = False
dup_l [x]    = False
dup_l (x:xs) = dup_1(x:xs) || dup_l(xs)

-- 删除list中指定位置的元素
dely :: Int-> [a] -> [a]
dely 1 (x:xs) = xs
dely k (x:xs) = [x] ++ dely (k-1) xs


-- 删除矩阵中指定列的元素
deletesty::(Num a) => Int -> [[a]] -> [[a]]
deletesty k x = [c |c<- [ dely k q | q<-x ]]

deletesty_2 :: Int->[[a]] -> [[a]]
deletesty_2 k [x] = [dely k x]
deletesty_2 k (x:xs) = dely k x : deletesty_2 k xs


-- 调换list中指定两个位置的顺序
-- list中！！取值是从0开始的
swaps1 :: Int -> Int -> [a] -> [a]
swaps1 i j xs =
    let elemI = xs !! i
        elemJ = xs !! j
        left = take i xs
        middle = take (j-i-1) (drop (i+1) xs)
        right = drop (j+1) xs
    in left ++ [elemJ] ++ middle ++ [elemI] ++ right





transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs     = map head xs : transpose(map tail xs)


-- 形成一行转置矩阵
frstrow :: [[Float]] -> [Float]
frstrow []     = []
frstrow (x:xs) = head x : frstrow xs

-- 帮助删除列（从行中删除所选项目）
minstolbvs :: [Float] -> Int -> [Float]
minstolbvs (x:xs) y = if y==1 then xs else x : minstolbvs xs (y-1)



multi :: [Int] -> [[Int]] ->[Int]
multi [] [] = []
multi [] _ = error "10"
multi _ [] = error "20"
multi (x:xs) (y:ys) = x * head y : multi xs ys

reduce :: [[Int]] -> [[Int]]
reduce [] = []
reduce (x:xxs) = check ((tail x) : reduce xxs)
    where
        check [[]] = []
        check x = x
--calculate one row
firstRow :: [Int] -> [[Int]] -> [Int]
firstRow _ [] = []
firstRow [] _ = []
firstRow x y = sum (multi x y) : firstRow x (reduce y)

matrixMulti :: [[Int]] -> [[Int]] -> [[Int]]
matrixMulti [] [] = []
matrixMulti [] _ = []
matrixMulti (x:xs) y = firstRow x y : matrixMulti xs y

converge::(Ord a, Num a)=>a->[a]->a
converge eps (a:b:xs)
      | abs (a-b)<= eps = a
      | otherwise = converge eps (b:xs)

easydiff::Fractional a=>(a->a)->a->a->a
easydiff f x h = (f(x+h)-2*f x+f(x-h))/h/h

diff::(Ord a, Fractional a)=>a->a->(a->a)->a->a
diff h0 eps f x =converge eps $ map(easydiff f x) $ iterate(/2) h0
     where easydiff f x h = (f(x+h)-2*f x+f(x-h))/h/h

intgrl::(Ord a, Fractional a)=>a->a->(a->a)->a->a->a
intgrl h0 eps f a b =converge eps $ map(easyIntgrl f a b) $ iterate(*2) h0

easyIntgrl::(Eq a, Fractional a)=>(a->a)->a->a->a->a
easyIntgrl f a b h = (b - a)/h * ((f a + f b)/ 2 + cikl f a ((b - a)/h) (h - 1))

cikl::(Eq a, Fractional a)=>(a->a)->a->a->a->a
cikl f x h 0 = 0
cikl f x h i = f (x + h * i) + cikl (f) x h (i - 1)


-- 取最大最小的数
maxmin :: Ord a => [a] -> [a]
mainmin [] = []
maxmin [x] = [x,x]
maxmin (x:y:[]) = if(x>y) then [x,y] else [y,x]
maxmin (x:y:z:xs) = maxmin(max_min++xs)
    where
        max_min = maxmin' x y z

maxmin' x y z
    | x >= y && y >= z = [x,z]
    | x >= z && z >= y = [x,y]
    | y >= x && x >= z = [y,z]
    | y >= z && z >= x = [y,x]
    | z >= x && x >= y = [z,y]
    | z >= y && y >= x = [z,x]

-- 把一个数字或一个数组塞入list间隙中
f :: t -> [t] -> [t]
f n [] = []
f n [x] = [x]
f n (x:xs) = x:n:f n xs

f' (x:xs) [y] = [y]
f' (x:xs) (y:ys) = y:[x]++xs ++(f' (x:xs) ys)


dup22 :: (t1 -> t -> t1) -> t1 -> [t] -> [t1]
dup22 f t [] = []
dup22 f t (x:xs) = res:dup22 f res xs
    where res = f t x

newfold :: Eq t1 => (t1 -> t1 -> t1) -> [t1] -> [t1]
newfold f (x:xs) = x : (dup22 f x xs)




------------------------------------------------------------

import Control.Exception
import Data.List
import Data.Char

code :: Int -> String ->String
code p mess = map (\c -> chr $ ord c + p) mess

decode :: Int->String->String
--decode p mess = code (negate p) mess
decode p mess = code (-p) mess


main :: IO ()
main = do

    result <- try $ readFile path :: IO (Either IOException String)
    case result of

      Left exception -> putStrLn $ "Fault: " ++ show exception

      Right cotent -> putStrLn cotent
      where path = "test17.hs"


data MyException = NoneResult deriving(Show)
instance Exception MyException

solve :: (Ord t, Floating t) => t -> t -> t -> [t]
solve a b c = if(derta' < 0 || a == 0) then throw NoneResult else [x1,x2]
    where
        derta = sqrt derta'
        derta' = b^2-4*a*c
        x1 = (-b+derta)/2
        x2 = (-b-derta)/2
main :: IO ()
main = do
    putStr "Input a: "
    s_a <- getLine
    putStr "Input b: "
    s_b <- getLine
    putStr "Input c: "
    s_c <- getLine
    let a = read s_a :: Double
        b = read s_b :: Double
        c = read s_c :: Double
    result <- try $ evaluate $ solve a b c :: IO (Either IOException [Double])
    case result of

      Left exception -> putStrLn $ "Fault: " ++ show exception

      Right content -> print content


f (n,m) [] = []
f (n,m) (x:xs)
    | (n == 0 || m == 0) = f(n-1,m-1) xs
    | otherwise = x:(f (n-1,m-1) xs)

mydivid :: (Ord a, Num a) => a -> a -> a -> a
mydivid eps c h = converge eps (iterate(\x -> x*(2-c*x)) h)
mydivid' c = converge 1e-20 (iterate(\x -> x*(2-c*x)) 1e-19)

converge::(Ord a, Num a)=>a->[a]->a
converge eps (a:b:xs)
      | abs (a-b)<= eps = a
      | otherwise = converge eps (b:xs)

easydiff::Fractional a=>(a->a)->a->a->a
easydiff f x h = (f(x+h)-2*f x+f(x-h))/h/h

diff::(Ord a, Fractional a)=>a->a->(a->a)->a->a
diff h0 eps f x =converge eps $ map(easydiff f x) $ iterate(/2) h0
     where easydiff f x h = (f(x+h)-2*f x+f(x-h))/h/h