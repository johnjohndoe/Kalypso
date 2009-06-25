#!/usr/bin/perl
#$target="../src/java/";

($target,@srcFiles)=@ARGV;
for(@srcFiles)
  {
    extract($_);
  }

sub extract
  {
    ($file)=@_;
    open(STREAM, "< $file");
    @lines = <STREAM>;
    close(STREAM);

    $total=join($seperator,@lines);
    @sources= split(/package/,$total);
    ($crap,@sources)=@sources; # first one is always crap
    for(@sources)
      {
	print2file("package".$_);
      }
  }

sub print2file
{
  ($text)=@_;

  $text =~ /package\s+(\S+);/;
  $package=$1;

#  $text =~ /(interface|class)\s+(\S+)/;
  $text =~ /[^@ejb:]{5}(interface|class)\s+(\S+)/;
  $file=$2.".java";

  $path=$package;
  $path =~ s/\./\//g;

  print "  save ".$target.$path."/".$file."\n";
  system("mkdirhier ".$target.$path);
  open(STREAM, ">".$target.$path."/".$file);
  print STREAM $text;
  close(STREAM);
}
