-- author: KCNyu(github:https://github.com/KCNyu)



-- 2.从此行中，删除最少数量的字符，以使您得到回文（从右到左和从左到右都可读的行）。
-- 输入数据格式程序接收一个非空字符串作为输入。 该字符串仅由大写拉丁字母组成输出格式程序应输出一个最大长度的回文字符串，
-- 该字符串可以从给定的回文中删除几个字母而获得。 如果有几种解决方案，则需要全部显示
--   例：
--      输入：
--      ASDDFSA
--      输出：
--      ASDDSA


getelem xs n = xs !! (n-1)

sub [] i ys j = []
sub xs i [] j = []
sub xs i ys j
    | elem_i == elem_j = (sub xs' (i-1) ys' (j-1)) ++ [elem_i]
    | otherwise = if(len_sub_xs > len_sub_ys) then sub_xs else sub_ys
    where
        sub_xs = sub xs' (i-1) ys j
        sub_ys = sub xs i ys' (j-1)
        xs' = take (len_xs-1) xs
        ys' = take (len_ys-1) ys
        len_xs = length xs
        len_ys = length ys
        elem_i = getelem xs i
        elem_j = getelem ys j
        len_sub_xs = length sub_xs
        len_sub_ys = length sub_ys

sub_final xs = sub xs len rev len
    where
        rev = reverse xs
        len = length xs

 