:: Checks out a project from the cvs
:: %1 username
:: %2 co path
:: %3 module name


:: The Revision argument is optional, if you don't want to use one but
:: rather the Head, then remove the "-r rev" from the following command:

set CVS_RSH=ssh8888.bat
cvs -d :ext:%1@172.16.0.252:/usr/local/cvsroot co -P -r branch-2005-12-2-kalypso-1-0 -d %2 %3
pause
exit

