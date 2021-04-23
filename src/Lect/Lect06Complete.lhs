% CS 340: Programming Paradigms and Patterns
% Lect 06 - Recursion
% Michael Lee

> module Lect.Lect06Complete where

Recursion
=========

Agenda:
  - Designing recursive functions
  - Structural vs. Generative recursion
  - Accumulators and Tail recursion


Designing recursive functions
-----------------------------

Steps:
  1. determine the function type
  2. identify which inputs can be decomposed into subproblems
  3. define the function for input patterns that can be handled non-recursively
  4. define the function for input patterns that require recursion
  5. ensure that the values are "shrunk" in recursive calls
  6. generalize and simplify

In steps 3 and 4, we might discover that we need additional functions, which 
themselves require stepping through the design process!

---

E.g., summing integer values from 0 up to N:

1. determine the type of the function

    > sumTo :: Integral a => a -> a

2. identify which inputs can be decomposed into subproblems

  - the input number, when decremented (if the result is greater than 0),
    represents a subproblem of the original input

3. define the function for input patterns that can be handled non-recursively

    > sumTo 0 = 0
    > sumTo 1 = 1
    > sumTo 2 = 3

4. define the function for input patterns that require recursion

    > sumTo n = n + sumTo (n-1)

5. ensure that the values are "shrunk" in recursive calls

    - `(n-1)` is strictly smaller than `n`

6. generalize and simplify

    - `1` and `2` are handled by the `n` pattern; remove them.

> sumTo :: Integral a => a -> a
> sumTo 0 = 0
> sumTo n = n + sumTo (n-1)

--- 

E.g., compute all permutations of values in a list

1. determine the type of the function

    > permutations :: [a] -> [[a]]

2. identify which inputs can be decomposed into subproblems

  - when the input list length is >= 2, permuting the tail of the list can be
    viewed as a subproblem

3. define the function for input patterns that can be handled non-recursively

    > permutations [] = [[]]
    > permutations [x] = [[x]]
    > permutations [x,y] = [[x,y], [y,x]]
    >
    > -- sometimes it doesn't hurt to define a few extra patterns 
    > permutations [x,y,z] = [[x,y,z], [y,x,z], [y,z,x],
    >                         [x,z,y], [z,x,y], [z,y,x]]

4. define the function for input patterns that require recursion

    > permutations (x:xs) = ... permutations xs ...

  - but how do we combine x with the permutations from the recursive call?

  - E.g., if (x:xs) = [1,2,3], we have x=1 and xs=[2,3], giving us
    permutations xs = [[2,3], [3,2]]. For each of the permutations, we 
    need to place x in all possible spots --- for [2,3], we would have
    [[1,2,3], [2,1,3], [2,3,1]], and for [3,2], we would have [[1,3,2],
    [3,1,2], [3,2,1]]. We then need to concatenate these two lists of
    permutations together.

  - Let's call the function that places x in all spots of a given permutation
    `interleave`. We can then write the recursive case of `permutations` like:

    > permutations (x:xs) = concat [interleave x p | p <- permutations xs]

  - Let's design `interleave`:

      1. determine the function type

          > interleave :: a -> [a] -> [[a]]

      2. identify which inputs can be decomposed into subproblems

        - the list argument represents N subproblems, where N is each position
          where the first argument can be interleaved

      3. define the function for input patterns that can be handled non-recursively

          > interleave x [] = [[x]]
          > interleave x [y] = [[x,y], [y,x]]
          > interleave x [y,z] = [[x,y,z], [y,x,z], [y,z,x]]
          
        - above patterns point to a non-recursive solution using a 
          list comprehensions

          > interleave x l@(y:ys) = (x:l) : [y:ll | ll <- interleave x ys]

5. ensure that the values are "shrunk" in recursive calls

  - each recursive call to `permutations` is given a smaller list

6. generalize and simplify

  - we can move `interleave` into a where clause within `permutations`

  - the [x], [x,y], [x,y,z] patterns are all covered by our recursive case;
    remove them

> permutations :: [a] -> [[a]]
> permutations [] = [[]]
> permutations (x:xs) = concat [ interleave x p | p <- permutations xs ]
>   where interleave x ys = [ lhs ++ [x] ++ rhs
>                           | i <- [0..length ys],
>                             let (lhs,rhs) = splitAt i ys]


Structural vs. Generative recursion
-----------------------------------

The above recipe may apply when a problem can be tidily decomposed into
subproblems according to the implicit structure of the values involved. E.g.,
solving a problem represented as a list by processing its head and recursing on
its tail.

But not all recursive functions follow this pattern! Sometimes solving a problem
recursively requires that the input values be transformed into new values which
aren't clearly substructures of nor "smaller" than the originals. 

This is sometimes called "generative" recursion. Their design is the domain of
algorithms.

---

E.g., Newton's method for finding the square root of N starts with a guess g
      (say, N/2), then tests to see if it is good enough (i.e., if the square of
      the g^2 == N); if not, we improve the guess by average g with n/g and try
      again. The intuition is that if g is too small, n/g will be increase the 
      guess, and if g is too big, n/g will decrease.

1. determine the type of the function

    > newtonsSqrt :: (Floating a, Ord a) => a -> a

   but we also need to define a function that takes a value and a guess for the 
   root and determines if the latter is good enough, and another function to
   improve guesses!

    > iter :: (Floating, Ord a) => a -> a
    > improve :: (Floating, Ord) => a -> a

2. identify which inputs can be decomposed into subproblems

    - the argument to `iter`, if not good enough, can be used to compute a 
      new guess

3. define the function for input patterns that can be handled non-recursively

    > newtonsSqrt x = iter x (x/2)

    > iter x g | g^2 =~= x = g

4. define the function for input patterns that require recursion

    > iter x g | g^2 =~= x = g
    >          | otherwise = iter (improve g)
    >   where improve g = (g + x/g) / 2

5. ensure that the values are "shrunk" in recursive calls

    - This is hard to do! We don't know for sure that the guess converges on
      the actual square root of x. (How can we prove this?)

6. generalize and simplify

    - iter should probably just be moved into newtonsSqrt

> infix 4 =~=
> (=~=) :: (Floating a, Ord a) =>  a -> a -> Bool
> x =~= y = abs (x - y) < 0.000001
>
> newtonsSqrt :: (Floating a, Ord a) => a -> a
> newtonsSqrt x = iter (x/2)
>   where iter g | g^2 =~= x = g
>                | otherwise = iter (improve g)
>         improve g = (g + x/g) / 2

    - note: we can generalize Newton's method itself as a higher-order function
      (more on that soon)

---

E.g., sort a list of values by splitting it in two, sorting each half, then 
merging the sorted results:

1. determine the type of the function

    > mergesort :: Ord a => [a] -> [a]

2. identify which inputs can be decomposed into subproblems

  - when the input list length is >= 2, it is to be split into two smaller
    lists, each of which can be sorted

3. define the function for input patterns that can be handled non-recursively

    > mergesort [] = []
    > mergesort [x] = [x]
    > mergesort [x,y] = if x < y then [x,y] else [y,x]

4. define the function for input patterns that require recursion

    > mergesort xs = merge (mergesort lhs) (mergesort rhs)
    >   where (lhs, rhs) = splitAt mid xs
    >         mid = length xs `div` 2

  - we need the function `merge`, which merges together two sorted lists
    to return a larger, sorted list --- let's design it:

      1. determine the type of the function

          > merge :: Ord a => [a] -> [a] -> [a]

      2. identify which inputs can be decomposed into subproblems

          - each input list can be broken into sublists that can also be merged

      3. define the function for input patterns that can be handled non-recursively

          > merge [] [] = []
          > merge xs [] = xs
          > merge [] ys = ys
          > merge [x] [y] = if x < y then [x,y] else [y,x]

      4. define the function for input patterns that require recursion

          > merge l1@(x:xs) l2@(y:ys) | x < y     = x : merge xs l2
          >                           | otherwise = y : merge l1 ys

      5. ensure that the values are "shrunk" in recursive calls

          - length xs + length l2 is less than length l1 + length l2
          - length l1 + length ys is less than length l1 + length l2

      6. generalize and simplify

        - `[x] [y]` pattern is covered by `l1@(x:xs) l2@(y:ys)`; remove it

          > merge [] [] = []
          > merge xs [] = xs
          > merge [] ys = ys
          > merge l1@(x:xs) l2@(y:ys) | x < y     = x : merge xs l2
          >                           | otherwise = y : merge l1 ys

5. ensure that the values are "shrunk" in recursive calls

  - we split the input list into two separate input lists (each half as small),
    but the total number of list elements passed down recursively is the same!

  - but we know that eventually, the lists will have sizes <= 1, which 
    correspond to base cases, so the recursion will end

6. generalize and simplify

  - we can move `merge` into a where clause within `mergesort`

  - the [x,y]` pattern is covered by the `xs` pattern; remove it

> mergesort :: Ord a => [a] -> [a]
> mergesort [] = []
> mergesort [x] = [x]
> mergesort xs = merge (mergesort lhs) (mergesort rhs)
>   where merge :: Ord a => [a] -> [a] -> [a]
>         merge [] [] = []
>         merge xs [] = xs
>         merge [] ys = ys
>         merge l1@(x:xs) l2@(y:ys) | x < y     = x : merge xs l2
>                                   | otherwise = y : merge l1 ys
>         (lhs, rhs) = splitAt mid xs
>         mid = length xs `div` 2


Accumulators and Tail recursion
-------------------------------

Some recursive functions are more naturally written and/or efficient when
implemented with an *accumulator*. 

E.g., consider our original implementation of `reverse`:

> reverse' :: [a] -> [a]
> reverse' [] = []
> reverse' (x:xs) = reverse' xs ++ [x]

This is inefficient, because the concatenation operator (++) needs to "search
for the end" of its first argument list (which is also the result of the
recursive call) in order to do its job. It would be more efficient to use the
`:` operator to incrementally build up a partially reversed list over the course
of the recursion. 

> reverse'' :: [a] -> [a] -> [a]
> reverse'' [] r = r
> reverse'' (x:xs) r = reverse'' xs (x:r)

The second argument of `reverse''` needs to be "primed" with an empty list, and
then gradually accumulates the solution, which we obtain at the end of the 
recursion. 

So that the caller doesn't need to provide the priming value, accumulators are
typically hidden inside where clauses:

> reverse''' :: [a] -> [a]
> reverse''' xs = rev xs []
>   where rev [] r = r
>         rev (x:xs) r = rev xs (x:r)

Try doing ":set +s" in ghci, then comparing outputs for the following:

  - take 5 $ reverse'   [1..1000000]
  - take 5 $ reverse''' [1..1000000]

---

We say that a function like `reverse'''`, where the solution to the problem is
obtained at the end of the recursion instead of being computed on the way "up"
out of a recursion, is *tail recursive*.

Sometimes tail recursion is good in Haskell, as it allows the function to be
more efficient (as above). Sometimes, however, it works against us. 

Consider a function that takes a value x and partitions an input list into two
output lists: one containing values < x, and the other containing values >= x.
Here we have two implementations --- one tail recursive and one not:

> tailPartition :: Ord a => a -> [a] -> ([a],[a])
> tailPartition n xs = part xs ([],[])
>   where part [] r = r
>         part (y:ys) (lts,gts) | y < n     = part ys (y:lts, gts)
>                               | otherwise = part ys (lts, y:gts)
>
>
> nontailPartition :: Ord a => a -> [a] -> ([a],[a])
> nontailPartition n [] = ([],[])
> nontailPartition n (x:xs) | x < n     = (x:lts, gts)
>                           | otherwise = (lts, x:gts)
>   where (lts, gts) = nontailPartition n xs

What happens when we call the two variations on an infinite list, but we only
need to take a fixed number of values from a given partition?

  - E.g., "take 5 $ snd $ XXXPartition 100 [1..]"

The tail recursive version fails! It fails because we cannot start extracting
values from the result of the tail recursive version until the entire recursion
completes (which it never does). 

On the other hand, the non tail recursive version only recurses as far as it 
needs, due to the lazy nature of Haskell --- it works fine on infinite lists!
