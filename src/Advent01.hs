
module Advent01 where

-- "Specifically, they need you to find the two entries that sum to 2020 and then
--  multiply those two numbers together."

day1  = head . (\x -> [a*b | a<-x, b<-x, a+b == 2020]) . map read . words

-- "What is the product of the three entries that sum to 2020?"

day1b  = head . (\x -> [a*b*c | a<-x, b<-x, c<-x, a+b+c == 2020]) . map read . words


-- "Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579."

-- >>> day1 testInput
-- 514579

-- "Using the above example again, the three entries that sum to 2020 are 979,
--  366, and 675. Multiplying them together produces the answer, 241861950."

-- >>> day1b testInput
-- 241861950

testInput = "1721 979 366 299 675 1456"