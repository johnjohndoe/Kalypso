@echo off
echo --------
echo Usage:   %0 jdk_home tomcat_home (classic/hotspot/server)
echo NOTE:    You MAY NOT use spaces in the path names. If you know how
echo          to fix this, please tell me.
echo          JDK 1.3 does not come with hotpot server by default, you must
echo          install this seperately if you wish to use it.
echo Example: %0 c:\progra~1\jdk c:\progra~1\tomcat hotspot
echo --------

if "%1" == "" goto eof
if "%2" == "" goto eof
if "%3" == "" goto eof

copy JavaService.exe %2\bin\Tomcat.exe > nul
%2\bin\Tomcat.exe -install Tomcat %1\jre\bin\%3\jvm.dll -Djava.class.path=%2\bin\bootstrap.jar;%2\bin\servlet.jar;%1\lib\tools.jar -Dcatalina.home=%2 -start org.apache.catalina.startup.Bootstrap -params start -stop org.apache.catalina.startup.Bootstrap -params stop -out %2\logs\stdout.log -err %2\logs\stderr.log

goto eof

:eof
