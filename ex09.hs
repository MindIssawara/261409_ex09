--rewrite list reverse using left fold
revl :: [a] -> [a]
revl ls = foldl myfunc [] ls
  where
    myfunc result x = x : result

--rewrite list reverse using right fold
revr :: [a] -> [a]
revr ls = foldr myfunc [] ls
  where
    myfunc x result = result ++ [x]

--which version is more efficient, and why?
--  revl(reverse using left fold) is better than revr(reverse using right fold).
--  Because of using (++) in revr, the time complexity of revr is O(n^2). On the other hand, revl is only O(n).

--rewrite map using fold
map' :: (a -> b) -> [a] -> [b]
map' fn ls = foldr myfunc [] ls
  where
    myfunc x result= fn x : result

--rewrite filter using fold
filter' :: (a -> Bool) -> [a] -> [a]
filter' pd ls = foldr myfunc [] ls
  where
    myfunc x result
        |   pd x = x : result
        |   otherwise = result