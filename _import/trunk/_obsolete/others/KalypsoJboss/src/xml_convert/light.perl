#!/usr/bin/perl
($infile)=@ARGV;
print "$infile $outfile \n";
open(DAT,"< $infile") or die;
@lines=<DAT>;
close(DAT);

for(@lines)
{
 $_ =~ s/(<|>|=)/ /g;
 $_ =~ s/\".+?\"/ /g;
 $_ =~ s/ +/ /g;
 $_ =~ s/\///g;
}

@sorted=sort(@lines);
for(@sorted)
{
# $_ =~ s/ /\n/g;
# $_ =~ s/( |\n)+/\n/g;
 print $_;
}
