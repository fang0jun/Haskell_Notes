-- author: KCNyu(github:https://github.com/KCNyu)


-- 1.您将得到一个由m行n列组成的矩形矩阵。 在第i行和第j列的交点处写入整数aij。 
-- 需要找到四个不同的元素，以使它们成为矩形的顶点，其边平行于矩阵的边，并且写入这些元素的数字之和最大。
-- 如果有几种最佳解决方案，请打印任何一种


ex =      [[1, 2, 3, 4], 
          [8, 7, 6, 5],
        [9, 10, 11, 12],
        [16, 15, 14, 13],
        [17, 18, 19, 20]]

shape xss = (length xss, length (head xss))

get_elem (i, j) xss = (xss !! i) !! j

max_sum xss = [get_elem a xss, get_elem b xss, get_elem c xss, get_elem d xss]
    where
        b = (fst a, snd d)
        c = (fst d, snd a)
        a = fst ans
        d = snd ans
        ans = max_sum' xss
get_sum (a, d) xss =    xss !! fst a !! snd a +
                        xss !! fst a !! snd d +
                        xss !! fst d !! snd a +
                        xss !! fst d !! snd d

helper [] ans xss = ans
helper (y:ys) ans xss = if (get_sum y xss) > (get_sum ans xss) then helper ys y xss else helper ys ans xss 

max_sum' xss = helper [((i, j), (k, l)) | i<-[0..(n-2)], j<-[0..(m-2)], k<-[(i+1)..(n-1)], l<-[(j+1)..(m-1)]] tmp xss
    where 
        (n, m) = shape xss
        tmp = ((0, 0), (1, 1))



