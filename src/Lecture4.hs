{- |
Module                  : Lecture4
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 4 of the Haskell Beginners course.

In this task you're going to implement a complete Haskell Program!

The purpose of the program is to read information about various
product trades from a file, calculate some stats about buys and sells
and pretty-print them in the terminal.

The content of the file looks like this:


Name,Type,Amount
Apples,Sell,25
Tomatoes,Sell,10
Pineapples,Buy,50


Specifically:

  1. The first line contains names of columns.
  2. Each line contains exactly 3 comma-separated values.
  3. The first value is a name of a product: a non-empty string
     containing any characters except comma.
  4. Second value is the type of trade. It's either a "Buy" or "Sell" string.
  5. The last, third value, is a non-negative integer number: the cost
     of the product.
  6. Each value might be surrounded by any amount of spaces.
  7. You don't need to trim spaces in the product name. But you need
     to parse the other two values even if they contain leading and
     trailing spaces.

Your program takes a path to a file and it should output several stats
about all the trades. The list of parameters to output is always the
same. Only values can change depending on file content.

For example, for the file content above, the program should print the following:


Total positions        : 3
Total final balance    : -15
Biggest absolute cost  : 50
Smallest absolute cost : 10
Max earning            : 25
Min earning            : 10
Max spending           : 50
Min spending           : 50
Longest product name   : Pineapples


To run the program, use the following command for specifying the
path (the repository already contains a small test file):


cabal run lecture4 -- test/products.csv


You can assume that the file exists so you don't need to handle such
exceptional situations. But you get bonus points for doing so :)

However, the file might contain data in an invalid format.
All possible content errors:

  * There might not be the first line with column names
  * Names of columns might be different
  * Each line can have less than 3 or more than 3 values
  * The product name string can be empty
  * The second value might be different from "Buy" or "Sell"
  * The number can be negative or not integer or not even a number

In this task, for simplicity reasons, you don't need to report any
errors. You can just ignore invalid rows.

Exercises for Lecture 4 also contain tests and you can run them as usual.
-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
module Lecture4
    ( -- * Main running function
      main

      -- * Types
    , TradeType (..)
    , Row (..)
    , MaxLen (..)
    , Stats (..)

      -- * Internal functions
    , parseRow
    , rowToStats
    , combineRows
    , displayStats
    , calculateStats
    , printProductStats
    ) where

import Data.List.NonEmpty (NonEmpty (..), fromList)
import Data.Semigroup (Max (..), Min (..), Sum (..))
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, fromJust, mapMaybe)
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Data.Foldable (foldl')

{- In this exercise, instead of writing the entire program from
scratch, you're offered to complete the missing parts.

Let's use this task as an opportunity to learn how to solve real-world
problems in a strongly-typed, functional, algebraic way.

First, let's define data types to represent a single row of our file.
-}

data TradeType
    = Buy
    | Sell
    deriving (Show, Eq, Read)

data Row = Row
    { rowProduct   :: String
    , rowTradeType :: TradeType
    , rowCost      :: Int
    } deriving (Show, Eq)

{-
Now you can implement a function that takes a String containing a single row and
parses it. The only catch here is that the input string might have format
errors. We will simply return an optional result here.

🕯 HINT: You may need to implement your own function to split 'String' by 'Char'.

🕯 HINT: Use the 'readMaybe' function from the 'Text.Read' module.
-}
dropWhitespace :: [Char] -> [Char]
dropWhitespace = takeWhile (not . isSpace) . dropWhile isSpace

tokenize :: Char -> String -> [String]
tokenize delim string = case string of
    "" -> []
    _ -> listOfTokens
    where
    go :: String -> String -> Char -> (String, String)
    go token str delimiter = case str of
        (x : xs) -> if x == delimiter
                    then (token, xs)
                    else go (token ++ [x]) xs delimiter
        [] -> (token, [])

    parsedResult = go [] string delim
    parsedToken = fst parsedResult
    stringRemainder = snd parsedResult
    listOfTokens = parsedToken : tokenize delim stringRemainder

toTradeType :: String -> Maybe TradeType
toTradeType word = case dropWhitespace word of
    "Buy" -> Just Buy
    "Sell" -> Just Sell
    _ -> Nothing

isNatural :: Double -> Bool
isNatural x = (x == fromInteger (round x)) && x >= 0

toInt :: String -> Maybe Int
toInt word = if isNatural (fromMaybe (-1) (readMaybe word)) then Just (read word) else Nothing

{- This function is really awkward and ugly, but I have no idea how to make it neater :( 
 - I can't really see how the andThen, maybePlus, Monad refactoring could be done here. 
 - I could just extract the pattern matching to a separate function but that only moves 
 - the ugly bits 5 lines up. -}
parseRow :: String -> Maybe Row
parseRow row = parsedRow where
    tokens = tokenize ',' row
    parsedRow = case tokens of
        [x, y, z] -> case x of
            "" -> Nothing
            _ -> case toTradeType y of
                Nothing -> Nothing
                _ -> case toInt z of
                    Nothing -> Nothing
                    _ -> Just Row {rowProduct=x, rowTradeType=fromJust (toTradeType y), rowCost=fromJust (toInt z)}
        _ -> Nothing



{-
We have almost all we need to calculate final stats in a simple and
elegant way.

To use algebraic abstractions for this problem, let's introduce a
custom data type for finding the longest product name.
-}

newtype MaxLen = MaxLen
    { unMaxLen :: String
    } deriving (Show, Eq)

{-
We can implement the 'Semigroup' instance for this data type that will
choose between two strings. The instance should return the longest
string.

If both strings have the same length, return the first one.
-}
instance Semigroup MaxLen where
    (<>) :: MaxLen -> MaxLen -> MaxLen
    x <> y = if length (unMaxLen x) < length (unMaxLen y)
             then y
             else x

{-
It's convenient to represent our stats as a data type that has
'Semigroup' instance so we can easily combine stats for multiple
lines.
-}

data Stats = Stats
    { statsTotalPositions :: Sum Int
    , statsTotalSum       :: Sum Int
    , statsAbsoluteMax    :: Max Int
    , statsAbsoluteMin    :: Min Int
    , statsSellMax        :: Maybe (Max Int)
    , statsSellMin        :: Maybe (Min Int)
    , statsBuyMax         :: Maybe (Max Int)
    , statsBuyMin         :: Maybe (Min Int)
    , statsLongest        :: MaxLen
    } deriving (Show, Eq)

{-
The 'Stats' data type has multiple fields. All these fields have
'Semigroup' instances. This means that we can implement a 'Semigroup'
instance for the 'Stats' type itself.
-}

evaluate :: Semigroup a => Maybe a -> Maybe a -> Maybe a
evaluate x y = evaluated
    where
        accumulated = x <> y
        evaluated = case accumulated of
            Nothing -> Nothing
            Just !z -> Just z

instance Semigroup Stats where
    (<>) :: Stats -> Stats -> Stats
    (Stats totalPos1 totalSum1 absMax1 absMin1 sellMax1 sellMin1 buyMax1 buyMin1 longest1)
        <> (Stats totalPos2 totalSum2 absMax2 absMin2 sellMax2 sellMin2 buyMax2 buyMin2 longest2) =
            evaluatedStats where

            !evaluatedSellMax = evaluate sellMax1 sellMax2
            !evaluatedSellMin = evaluate sellMin1 sellMin2
            !evaluatedBuyMax = evaluate buyMax1 buyMax2
            !evaluatedBuyMin = evaluate buyMin1 buyMin2
            evaluatedStats = Stats {statsTotalPositions=totalPos1 <> totalPos2,
                   statsTotalSum=totalSum1 <> totalSum2,
                   statsAbsoluteMax=absMax1 <> absMax2,
                   statsAbsoluteMin=absMin1 <> absMin2,
                   statsSellMax=evaluatedSellMax,
                   statsSellMin=evaluatedSellMin,
                   statsBuyMax=evaluatedBuyMax,
                   statsBuyMin=evaluatedBuyMin,
                   statsLongest=longest1 <> longest2}

{-
The reason for having the 'Stats' data type is to be able to convert
each row independently and then combine all stats into a single one.

Write a function to convert a single 'Row' to 'Stats'. To implement this
function, think about how final stats will look like if you have only a single
row in the file.

🕯 HINT: Since a single row can only be 'Buy' or 'Sell', you can't
   populate both sell max/min and buy max/min values. In that case,
   you can set the corresponding field to 'Nothing'.
-}

rowToStats :: Row -> Stats
rowToStats row = case row of
    Row name Buy amount -> Stats {statsTotalPositions=Sum 1,
                                  statsTotalSum=Sum (-amount),
                                  statsAbsoluteMax=Max amount,
                                  statsAbsoluteMin=Min amount,
                                  statsSellMax=Nothing,
                                  statsSellMin=Nothing,
                                  statsBuyMax=Just (Max amount),
                                  statsBuyMin=Just (Min amount),
                                  statsLongest=MaxLen name}
    Row name Sell amount -> Stats {statsTotalPositions=Sum 1,
                                   statsTotalSum=Sum amount,
                                   statsAbsoluteMax=Max amount,
                                   statsAbsoluteMin=Min amount,
                                   statsSellMax=Just (Max amount),
                                   statsSellMin=Just (Min amount),
                                   statsBuyMax=Nothing,
                                   statsBuyMin=Nothing,
                                   statsLongest=MaxLen name}




{-
Now, after we learned to convert a single row, we can convert a list of rows!

However, we have a minor problem. Our 'Stats' data type doesn't have a
'Monoid' instance and it couldn't have it! One reason for this is that
there's no sensible "empty" value for the longest product name. So we
simply don't implement the 'Monoid' instance for 'Stats'.

But the list of rows might be empty and we don't know what to return
on empty list!

The solution of this problem is to propagate handling of this
situation upstream. In our type signature we will require to accept a
non-empty list of rows.

We can use the 'NonEmpty' data type from the 'Data.List.NonEmpty'
module for this purpose. 'NonEmpty' is like 'List1' from Lecture 3
exercises (remember that type?) but with a different constructor.

Have a look at the 'sconcat' function from the 'Semigroup' typeclass to
implement the next task.
-}

combineRows :: NonEmpty Row -> Stats
combineRows rows = case rows of
    x :| [] -> rowToStats x
    (x :| xs) -> foldl' (<>) (rowToStats x) (Prelude.map rowToStats xs)

{-
After we've calculated stats for all rows, we can then pretty-print
our final result.

    let stringStats =
            [ "Total positions        : 3"
            , "Total final balance    : -15"
            , "Biggest absolute cost  : 50"
            , "Smallest absolute cost : 10"
            , "Max earning            : 25"
            , "Min earning            : 10"
            , "Max spending           : 50"
            , "Min spending           : 50"
            , "Longest product name   : Pineapples"
            ]
If there's no value for a field (for example, there were not "Buy" products),
you can return string "no value".
-}



displayStats :: Stats -> String
displayStats stats = "Total positions        : " <> show (getSum (statsTotalPositions stats))
                    <> "\nTotal final balance    : " <> show (getSum (statsTotalSum stats))
                    <> "\nBiggest absolute cost  : " <> show (getMax (statsAbsoluteMax stats))
                    <> "\nSmallest absolute cost : " <> show (getMin (statsAbsoluteMin stats))
                    <> "\nMax earning            : " <> case statsBuyMax stats of
                                                        Nothing -> "no value"
                                                        _ -> show (getMax (fromJust (statsSellMax stats)))
                    <> "\nMin earning            : " <> case statsBuyMin stats of
                                                        Nothing -> "no value"
                                                        _ -> show (getMin (fromJust (statsSellMin stats)))
                    <> "\nMax spending           : " <> case statsSellMax stats of
                                                        Nothing -> "no value"
                                                        _ -> show (getMax (fromJust (statsBuyMax stats)))
                    <> "\nMin spending           : " <> case statsSellMin stats of
                                                        Nothing -> "no value"
                                                        _ -> show (getMin (fromJust (statsBuyMin stats)))
                    <> "\nLongest product name   : " <> unMaxLen (statsLongest stats)

{-
Now, we definitely have all the pieces in places! We can write a
function that takes the content of the file (the full content with multiple
lines) and converts it to pretty-printed stats.

The only problem here is that after parsing a file we might end with
an empty list of rows but our 'combineRows' function requires to have
a non-empty list. In that case, you can return a string saying that
the file doesn't have any products.

🕯 HINT: Ideally, the implementation of 'calculateStats' should be just a
   composition of several functions. Use already implemented functions, some
   additional standard functions and maybe introduce helper functions if you need.

🕯 HINT: Have a look at 'mapMaybe' function from 'Data.Maybe' (you may need to import it).
-}

calculateStats :: String -> String
calculateStats inputString = outputString where
    listOfLines = lines inputString
    parsedRows = mapMaybe parseRow listOfLines
    outputString = case parsedRows of 
        [] -> "file doesn't have any products"
        _ -> (displayStats . combineRows . fromList) parsedRows

{- The only thing left is to write a function with side-effects that
takes a path to a file, reads its content, calculates stats and prints
the result.

Use functions 'readFile' and 'putStrLn' here.
-}

printProductStats :: FilePath -> IO ()
printProductStats path = do
    fileContents <- readFile path
    putStrLn (calculateStats fileContents)

{-
Okay, I lied. This is not the last thing. Now, we need to wrap
everything together. In our 'main' function, we need to read
command-line arguments that contain a path to a file and then call
'printProductStats' if the arguments contain a path. If they are invalid,
you can print an error message.

Use the 'getArgs' function from the 'System.Environment' module to read
CLI args:

https://hackage.haskell.org/package/base-4.16.0.0/docs/System-Environment.html#v:getArgs
-}

main :: IO ()
main = do
    args <- getArgs
    if null args
    then putStrLn "No arguments provided"
    else printProductStats (last args)

{-
And that's all!

You solved a difficult problem in functional style! 🥳
You should be proud of yourself 🤗

====================================================

For an extra challenge, you can make sure that your solution is optimally lazy
and streaming. The course contains an additional executable
"generate-many-products" that generates a 2GB file of products.

> NOTE: Make sure you have enough disk space before running the generator and
> make sure to delete the file afterwards to not to waste space

To run the executable that produces a huge file, use the following command:


cabal run generate-many-products


Laziness in Haskell is a double-edged sword. On one hand, it leads to
more composable code and automatic streaming in most cases. On the
other hand, it's easy to introduce space leaks if you're not being
careful.

The naive and straightforward implementation of this task most likely
contains space leaks. To implement the optimal streaming and lazy
solution, consider doing the following improvements:

  1. Enable the {-# LANGUAGE StrictData #-} pragma to this module.

     * Fields in Haskell data types are lazy by default. So, when
       combining 'Stats' with <>, fields on the new 'Stats' value are
       not fully-evaluated. Enabling 'StrictData' fixes this.

  2. Make sure you traverse the list of all products only once in each
     function. In that case, due to laziness, composition of such
     functions will traverse the list only once as well.

     * You can traverse each separate line multiple times because each
       individual line in the file is short and traversing it only
       once won't bring lots of performance improvements.

  3. Don't use 'length' to calculate the total number of rows.

  4. Replace 'sconcat' in 'combineRows' with foldl' or manual recursive
     function using {-# LANGUAGE BangPatterns #-} and strict
     accumulator of type 'Stats'.

     * 'sconcat' is a lazy function. So, even if you force every field
       of the 'Stats' data type with 'StrictData', it won't make a
       difference if you don't force the 'Stats' accumulator itself.

  5. Combine fields of type 'Maybe' in the 'Stats' data type with a
     stricter version of '<>'.

     * The 'Semigroup' instance for 'Maybe' (that you've probably used
       for implementing the 'Semigroup' instance for 'Stats') is lazy
       and doesn't force values inside 'Just' constructors. To fix
       this problem, you can use a custom function that combines two
       values of type 'Maybe' and pattern matches on @Just !x@ to
       ensure that values inside 'Just' are fully-evaluated on each
       step.


You can check memory usage of your program by running `htop` in a
separate terminal window. If you see that the memory usage doesn't
grow indefinitely by eating all your RAM, it means that the solution
requires constant-size memory.

Additionally, on Linux, you can run the following command to see the
actual size of required memory during your program execution:


/usr/bin/time -v cabal run lecture4 -- test/gen/big.csv


You can expect the optimal lazy solution to run in ~20 minutes and
consume ~200 MB of RAM. The numbers are not the best and there's lots
of room for optimization! But at least you've managed to implement a
streaming solution using only basic Haskell 🤗

Here are my best results so far:
	Command being timed: "cabal run lecture4 -- test/gen/big.csv"
	User time (seconds): 861.53
	System time (seconds): 240.15
	Percent of CPU this job got: 137%
	Elapsed (wall clock) time (h:mm:ss or m:ss): 13:19.30
	Average shared text size (kbytes): 0
	Average unshared data size (kbytes): 0
	Average stack size (kbytes): 0
	Average total size (kbytes): 0
	Maximum resident set size (kbytes): 186496
	Average resident set size (kbytes): 0
	Major (requiring I/O) page faults: 588
	Minor (reclaiming a frame) page faults: 115742
	Voluntary context switches: 39929649
	Involuntary context switches: 50429
	Swaps: 0
	File system inputs: 4416560
	File system outputs: 17888
	Socket messages sent: 0
	Socket messages received: 0
	Signals delivered: 0
	Page size (bytes): 4096
	Exit status: 0

-}
