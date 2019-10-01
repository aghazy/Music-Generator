import MusicResources

--------------------part (a)-----------------------------------------------------------------------
makeStatsList :: [(Char,[(Int,Char)])]
makeStatsList = (makeStatsListhelper chars)

makeStatsListhelper [] = [] -- move chars one by one
makeStatsListhelper (x:xs) = (x,(helper1 x training [])) :(makeStatsListhelper xs)

-- move lists of training one by one
helper1 _ [] l = sorted l
helper1 x (y:ys) l = helper1 x ys (helper2 x y l)

--to find the result of the first list in training
helper2 _ [] l = l
helper2 _ (_:[]) l = l
helper2 x (y:ys:yss) l| y == x  = helper2 x (ys:yss) (editPair ys l)
					  |otherwise = helper2 x (ys:yss) l

editPair x [] = [(1,x)] -- edit the pairs
editPair x ((n,c):ys)| c == x = (n+1,c):ys
					 | otherwise  =(n,c):(editPair x ys)
					 
--------sorting-----------					 
isGreaterOrEqual (x,y) (n,c) =(x>n) || (x==n && y>=c) 
insert (n,c) []=[(n,c)]				 
insert (n,c) ((m,k):ys)| (isGreaterOrEqual(n,c) (m,k)) = (n,c):((m,k):ys)
 | True =(m,k):(insert (n,c) ys)
sorted []=[]
sorted (x:xs)=(insert x (sorted xs))
------------------------
--------------------------------------------------------------------------------------------------
 
-------------------part (b)-----------------------------------------------------------------------
compose _ 0 =[]
compose x 1= [x]
compose x n = x:(compose (getInput2 x) (n-1))
getInput2 x = getInput x (makeStatsList)
getInput x ((z,zs):ys)| (x == z &&zs == []) = error "error in generating the script"
                      | (x == z) = getCharacter (pairSSSToList zs)
                      |True = (getInput x ys)
pairSSSToList [] = []
pairSSSToList ((n,c):ls) = pairToList(n,c) ++ (pairSSSToList ls) 
pairToList (0,_) = []
pairToList (n,c) = c:pairToList(n-1,c)
getCharacter list  =  (getIt (randomZeroToX ((length list)-1))  list)
getIt 0 (l:lx) = l
getIt n (l:lx) = (getIt (n-1) lx)
						

--------------------------------------------------------------------------------------------------