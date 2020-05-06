Michael Quinn
Part 1

Section A

> set= [x*x | x <- [1..10]]

Question 1: How would you describe set with your own words?
	set is a list consisting the first 10 positive (excluding 0) square numbers
	
Question 2: Redefine set using primitive recursion with guards (setG)

> setG :: (Eq n, Num n) => n -> [n]
> setG n
>    | n == 1     = [n * n]
>    | otherwise  = (setG (n-1)) ++ [n * n]

Question 3: Redefine set using primitive recursion with pattern matching style (setP)

> setP :: (Eq n, Num n) => n -> [n]
> setP 1 = [1]
> setP n = (setP (n-1)) ++ [n * n]

Question 4: Redefine set but use map (setM)

> setM :: (Eq n, Num n) => [n]
> setM = map (\n -> n*n) [1,2,3,4,5,6,7,8,9,10]

Section B
Problem 1:

> mm f g = (.) (map f) (map g)

Question 1: What is the type of mm ((-) 20) ((+) 9)?
	:: (Num c) => [c] -> [c]

Question 2: What are the values of the following?
..  > mm ((-) 20) ((+) 9) [3, -2, -9, 50]
..  > mm ((-) 20) ((+) 9) [-3, 2, -9, 50]
	[-8, -13, -20, 39]
	[-14, -9, -20, 39]

Question 3: Describe in words what mm does
	mm effectively takes two functions, uses them to create map functions, and composes them; given two unary functions and a list, it will perform the second function on every element on the list, then perform the first function on every element of the list.

Question 4. Define mm2 to be equivalent to mm but only using one 'map' application, and compose (.).

> mm2 :: (b -> c) -> (a -> c) -> [c]
> mm2 f g = map((.) f g)

Problem 2: "prodPRL nums" produces a list of partial products of the numbers in nums, in which the first value in the resulting list is the product of all the items, the second is the list of all but the first item in the list, etc; and the last element ends up equal to simply the last element in nums.
Define prodPRLHOF to be equivalent to prodPRL but using a fold

> prodPRL (l:ls) = prods [l] ls
>   where
>    prods (a:as) (n:ns) = prods ((a*n):a:as) ns
>    prods as []  = as

> prodPRLHOF nums
>    | length nums == 0  = []
>    | otherwise         = (foldr (*) 1 nums) : prodPRLHOF (tail nums)
