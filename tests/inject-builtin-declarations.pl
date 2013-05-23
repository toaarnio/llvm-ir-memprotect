#!/usr/bin/env perl
my $file = shift @ARGV;
my $file2 = shift @ARGV;
printf "Injecting $file with $file2\n";
my $tmp = "$file.tmp";
open(OUT, ">", $tmp) or die "Cannot open $tmp for writing";
open(F, "<", $file) or die "Cannot open $file for reading";
$_ = <F>; print OUT;
$_ = <F>; print OUT;
$_ = <F>; print OUT;
open(G, "<", $file2) or die "Cannot open $file2 for reading";
while (<G>) {
  print OUT;
}
while (<F>) {
  print OUT;
}
close(G);
close(F);
close(OUT);
rename($tmp, $file);
