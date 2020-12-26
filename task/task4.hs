-- author: KCNyu(github:https://github.com/KCNyu)


-- 4.编写一个程序，确定给定的整数矩阵是否是魔方。 
-- 阶数为n的幻方是由数字1,2，…，n2组成的正方形n×n矩阵； 1,2，...，n2，
-- 以便每一列，每一行和两个对角线的总和相等。 n是一个奇数。

getrow :: (Eq t, Num t) => [[t1]] -> t -> [t1]
getrow [] y     = []
getrow (x:xs) y = if y == 1 then x else getrow xs (y-1)

getcol :: [[t]] -> Int -> [t]
getcol [] y     = []
getcol (x:xs) y =  [x !! (y-1)] ++ getcol xs y

getelem :: [[a]] -> Int -> Int -> a
getelem xs i j = xs !! (i-1) !! (j-1)

getdiag' xs n = if(n < length xs) then [getelem xs n n]++getdiag' xs (n+1) else [getelem xs n n]
getdiag'' xs n = if(n < len) then [getelem xs n (len-n+1)]++getdiag'' xs (n+1) else [getelem xs n (len-n+1)]
    where
        len = length xs

judge_row_sum x xs res = (row == res)
    where
        row = sum $ getrow xs x

judge_col_sum x xs res = (col == res)
    where
        col = sum $ getcol xs x

judge1 n xs res = if(n < length xs) then (judge_row_sum n xs res) && (judge1 (n+1) xs res) else judge_row_sum n xs res
judge2 n xs res = if(n < length xs) then (judge_col_sum n xs res) && (judge2 (n+1) xs res) else judge_col_sum n xs res
judge3 xs res = diag1 == res && diag2 == res
    where
        diag1 = sum $ getdiag' xs 1
        diag2 = sum $ getdiag'' xs 1

judge xs = judge1 1 xs res && judge2 1 xs res && judge3 xs res
    where
        res = sum $ getrow xs 1


---------------
       

get_n xss = length xss

magic_sum xss = n * (n ^ 2 + 1) `div` 2
    where n = get_n xss

check_row xss = all ( == (magic_sum xss)) (map sum xss)

check_col xss = all ( == (magic_sum xss)) (sum_col xss) 

sum_col xss = (helper (head xss) (tail xss)) 

helper sums [] = sums
helper sums (xs:xss) = helper (zipWith (+) sums xs) xss

check_dia xss = all ( == (magic_sum xss)) ([first_dia xss, second_dia xss]) 

first_dia xss = helper' 0 0 xss

helper' cur_sum pos [] = cur_sum
helper' cur_sum pos (xs:xss) = helper' (cur_sum + (xs !! pos)) (pos + 1) xss

second_dia xss = helper'' 0 (n - 1) xss
    where n = get_n xss

helper'' cur_sum pos [] = cur_sum
helper'' cur_sum pos (xs:xss) = helper'' (cur_sum + (xs !! pos)) (pos - 1) xss

ans xss = (check_row xss) && (check_col xss) && (check_dia xss)
