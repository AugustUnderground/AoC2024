#!/usr/bin/env perl

use strict;
use warnings;

my $file = './rsc/day03.txt';
open my $info, $file or die "Could not open $file: $!";

my $regex1 = qr/mul\((\d+),(\d+)\)/;
my $regex2 = qr/don't\(\).*?do\(\)|don't\(\).*$/;

my $total1  = 0;
my $total2  = 0;
my $program = "";

while(my $line = <$info>) { chomp($line); $program .= $line; }
while ($program =~ /$regex1/g) { $total1 = $total1 + ($1 * $2); }
$program =~ s/$regex2//g;
while ($program =~ /$regex1/g) { $total2 = $total2 + ($1 * $2); }

print "Silver: ${total1}\n";
print "Gold:   ${total2}\n";

close $info;
