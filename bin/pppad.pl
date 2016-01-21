#!/usr/bin/perl
use File::Copy 'move';

foreach my $file (@ARGV) {
    if (-e $file) {
        my ($numPrefix) = $file =~ /^(\d+)/;
        my $padZeros = 0 x (7 - length($numPrefix));
        my $newName = $padZeros . $file;
        move $file, $newName;
    }
}
