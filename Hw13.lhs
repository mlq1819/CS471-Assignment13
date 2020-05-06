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
>    | otherwise  = (n * n) : (setG (n-1))

Question 3: Redefine set using primitive recursion with pattern matching style (setP)

> setP :: (Eq n, Num n) => n -> [n]
> setP 1 = 1
> setP n = (n * n) : (setP (n-1))

Question 4: Redefine set but use map (setM)

> setM :: (Eq n, Num n) => [n]
> setM = map (\n -> n*n) [1,2,3,4,5,6,7,8,9,10]

