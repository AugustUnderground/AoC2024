#!/usr/bin/env perl

use strict;
use warnings;

my $file    = "./rsc/day03.txt";
my $program = "";
open my $content, $file or die "Could not open $file: $!";
while(my $line = <$content>) { chomp($line); $program .= $line; }
close $content;

my $regex1 = qr/mul\((\d+),(\d+)\)/;
my $total1 = 0;
my $regex2 = qr/don't\(\).*?do\(\)|don't\(\).*$/;
my $total2 = 0;

while ($program =~ /$regex1/g) { $total1 = $total1 + ($1 * $2); }
$program =~ s/$regex2//g;
while ($program =~ /$regex1/g) { $total2 = $total2 + ($1 * $2); }

print "Silver: ${total1}\n";
print "Gold:   ${total2}\n";
