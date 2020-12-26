-- author: KCNyu(github:https://github.com/KCNyu)

-- 3.引入了一个少于100的50个自然数的序列，从该序列中选择一个最大长度的子序列，该序列是自然数的连续片段，并打印出来。

substring [x] = [x]
substring [x,y] = if(y /= (x+1)) then [x] else [x,y]
substring (x:y:xs) = if(y /= (x+1)) then [x] else x:(substring (y:xs))

substr s [] = s
substr s (x:xs) = if(len_s > len_str) then (substr s xs) else (substr str xs)
    where
        len_s = length s
        len_str = length str
        str = substring (x:xs)

substr_final s = substr [] s



