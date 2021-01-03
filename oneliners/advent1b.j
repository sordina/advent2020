#!/usr/bin/env ijconsole

input  =. ;: (1!:1) 2 { ARGV
nums   =. x: (__&< # [) > __ ". each input
answer =. ~. (2020 = , nums +/ nums +/ nums) # (, nums */ nums */ nums)

echo answer

exit''
