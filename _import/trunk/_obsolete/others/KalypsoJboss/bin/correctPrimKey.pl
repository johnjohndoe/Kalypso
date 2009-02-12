#!/usr/bin/perl
#$target="../src/java/";
($jarfile)=@ARGV;

open(STREAM, "< $jarfile") or die("konnte nicht ffnen");
@lines = <STREAM>;
close(STREAM);

$total=join("",@lines);
$total=~ s/(\r|\n)//g;

for("id","tableName")
  {
    $value=$_;
    $pattern="(<cmp-field.+?<field-name>".$value."<\/field-name>.+?<\/cmp-field>)";
    $newfield="<primkey-field>".$value."</primkey-field>";
    $total =~ s/$pattern/$1$newfield/g;	
  }
print $total;
