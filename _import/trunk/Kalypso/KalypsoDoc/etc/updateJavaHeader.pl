#!/usr/bin/perl
#-------------------------------------------------------------
#
# This file is part of kalypso.
# Copyright (C) 2004, 2005 by:
#
# Technical University Hamburg-Harburg (TUHH)
# Institute of River and coastal engineering
# Denickestr. 22
# 21073 Hamburg, Germany
# http://www.tuhh.de/wb
#
# and
# 
# Bjoernsen Consulting Engineers (BCE)
# Maria Trost 3
# 56070 Koblenz, Germany
# http://www.bjoernsen.de
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# Contact:
#
# E-Mail:
# belger@bjoernsen.de
# schlienger@bjoernsen.de
# v.doemming@tuhh.de
# 
# ------------------------------------------------------------- 

# -----------
#
# script to update file headers of kalypso sources
# @author v.doemming@tuhh.de
# -----------

# global vars
$debug="false";
#debug="true"   do not change java-files
#debug="false"  change java-files

$test="false"; 
# test="false": no testing
# test="true" : TEST-MODE enabled, checks if all files can be processed with this program.
#		splits comments from code, rejoins code and comments and compares with original content.
#               if not, program will interrupt.

# TEST-MODE eq "true" will force allways DEBUG-MODE
if($test eq "true")
{
  $debug="true";
}

$headerKalypso=&readHeader("HeaderKalypso.txt");
$headerKalypso2D=&readHeader("HeaderKalypso2D.txt");
$headerDeegreeFork=&readHeader("HeaderDeegreeFork.txt");


# in deegreefork is also code from third parties (neither from deegree nor from kalypso).
# This code went into deegree before this fork. the headers will keep untouched, 
# so defaultHeader should be set to "" when examine deegreefork:
# 
$defaultHeader="";

#&examine("../../backupdeegree/deegree/org/deegree");
#&examine("../../backupdeegree/deegree/org/deegree_impl");

$defaultHeader=$headerKalypso2D;
&examine("../../Kalypso2d/src/org");

# the open source part of kalypso (other things should not occur here or have to be commented out):



$defaultHeader=$headerKalypso;
&examine("../../KalypsoUI/src");
&examine("../../KalypsoCalcService/src");
&examine("../../KalypsoCore/src");
&examine("../../KalypsoCoreServices/src");
&examine("../../KalypsoDWD/src");
#&examine("../../KalypsoFeature/src");
#&examine("../../KalypsoMetaDocService/src");
&examine("../../KalypsoNA/src");
&examine("../../KalypsoObsService/src");
&examine("../../KalypsoOptimizePlugin/src");
#&examine("../../KalypsoPSICompactAdapter/src");
#&examine("../../KalypsoPSICompactImpl/src");
&examine("../../KalypsoUserService/src");
&examine("../../KalypsoUtil/src");

die("ready.");

sub readHeader()
{
 my $headerFile;
 ($headerFile)=@_;
 open(FILE, "< $headerFile");  
 my @lines = <FILE>;
 close(FILE);               
 return join("",@lines);
}


sub examine()
{
 my $dir=@_[0];
 opendir(DIR, $dir) || die "$Verzeichnis:  $!";
 my @files = readdir(DIR);
 closedir(DIR);
 for(@files)
 {
  my $file=$dir."/".$_;
  if( -d $file && not $file =~ /.+\.$/ )
  {
    &processDir($file)
  }
  elsif($_ =~ /.+\.java$/)
  {
    &updateHeader($file);
  }  
 } 
}

sub processDir()
{
 my $dir=@_[0];
 &examine($dir);
}

sub updateHeader()
{  
  my $file=@_[0];
  # read file
  open(FILE, "< $file"); 
  @lines = <FILE>;      
  close(FILE);           
  # put content in one line  
  $inline=join("",@lines);
  # sort comment and code
  # example:
  # |    /* comment */     /* comment */       |
  @code=split(/\/\*.*?\*\//s,$inline);  
  # pattern:
  # /*...*/
  
#  for(@code)
  #{
  	#print "\ncode:\n";
	#print $_;
  #}
  @comment=split(/^.*?\/\*|\*\/.*?\/\*|\*\/.*?$/s,$inline);  
  # patterns:
  # |   /*  or
  # */.../* or
  # */    |
  #for(@comment)
  #{
  	#print "\ncomment:\n";
	#print $_;
  #}

  my $fileStatus="unknown";
  my $newContent="";    
  my $c=0;
  my $first;
  if(inline =~ /^\/\*/)
  {
    $first="comment";
  }
  else
  {
    $first="code";
  }
  
  while($c<=$#comment && $c<=$#code)  
  {
    $codeContent=@code[$c];
    if($test eq "false")
    {
     ($commentContent,$status)=&updateText(@comment[$c+1]);
    }
    else
    {
     $commentContent="@comment[$c+1]";
     $status="testing";
    }
    
    if($status ne "unknown")
    {
     # if only one comment is known, than file is known
     $fileStatus=$status;
    }  
   
    if($first eq "comment")
    {
     if(not $commentContent =~ /^\s*$/)
     {
      $newContent.="/*".$commentContent."*/"; 
     }
     $newContent.=$codeContent; 
    }
    else
    {
     $newContent.=$codeContent; 
     if(not $commentContent =~ /^\s*$/)
     {
      $newContent.="/*".$commentContent."*/"; 
     }
    }      
    $c++;
  }
  
  # if no header is found insert default header
  if($fileStatus eq "unknown" && $defaultHeader ne "")
  {
   $newContent="/*".$defaultHeader."*/\n".$newContent;
  }
  print "\n\n          $file\n";
  print "[ $fileStatus ]";  
  if($newContent eq $inline)
  {
   print " nothing to do";  
  }  
  else
  {
    print " rewrite";
    if($test eq "true")
    {
     print " TEST-MODE failed.\n";
     print "  :-( check programm\n";
     open(OUT, "> test.java");
     print OUT $newContent;
     close(OUT);   
     print "  output is written to test.java\n";
     die();
    }  
    if($debug eq "false")
    {
     open(OUT, "> $file");
     print OUT $newContent;
     close(OUT);   
    }
    else
    {
     print " (DEBUG-MODE)";
    }
  }
  print "\n";
}

sub updateText()
{
 my $text=@_[0]; 
 if(  $text =~ /This file is part of deegree./
   && $text =~ /Files in this package are originally taken from deegree/)
 {
  # file is from deegree
  return ($headerDeegreeFork,"deegree");
 } 
 elsif($text =~ /This file is part of kalypso/)
 {
   # file is from kalypso
   return ($headerKalypso,"kalypso");
 }
 elsif($text =~ /This file is part of ekalypso/)
 {
   # file is from kalypso
   return ($headerKalypso2D,"ekalypso");
 } 
 else
 {
  # noheaderComment
  return ($text,"unknown");
 }
}
