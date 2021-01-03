#!/usr/bin/env ijconsole

read   =. 1!:1
input  =. ;: read 2 { ARGV

nums   =. > ". each (2 * i. (# input) % 2) { input
answer =. ~. (2020 = , nums +/ nums) # (, nums */ nums)

echo answer

exit''
