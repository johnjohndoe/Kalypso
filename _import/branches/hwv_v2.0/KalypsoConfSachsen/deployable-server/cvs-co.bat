:: Checks out a project from the cvs
:: %1 username
:: %2 co path
:: %3 module name

set CVS_RSH=ssh8888.bat
cvs -d :ext:%1@172.16.0.252:/usr/local/cvsroot co -P -d %2 %3
pause
exit

