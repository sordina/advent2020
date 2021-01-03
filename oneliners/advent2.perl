#!/usr/bin/env perl

$n=0;
while (<> =~ /(\d+)-(\d+) (\w): (.*)/) {
	($l,$h,$w,$p)=($1,$2,$3,$4);
	$i=0;
	while($p =~ /$w/g) {$i++};
		$i>=$l && $i<=$h && $n++
}
print $n . "\n"
