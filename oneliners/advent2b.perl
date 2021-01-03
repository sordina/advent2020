#!/usr/bin/env perl

$n=0;
while (<> =~ /(\d+)-(\d+) (\w): (.*)/) {
	($l,$h,$w,$p)=($1,$2,$3,$4); (substr($p,$l-1,1) eq $w xor substr($p,$h-1,1) eq $w) && $n++
}
print $n . "\n"
