@echo off 

if "%1" == "uninstall" goto uninstall 
if "%1" == "-uninstall" goto uninstall 
if "%1" == "" goto usage 
if "%2" == "" goto usage 
if "%3" == "" goto usage 
if "%1" == "-help" goto usage 
if "%1" == "-?" goto usage 
if "%1" == "/?" goto usage 

:install 
JavaService.exe -install JBoss30 %1\jre\bin\%3\jvm.dll -Djava.class.path=%1\lib\tools.jar;%2\bin\run.jar -start org.jboss.Main -stop org.jboss.Main -method systemExit -out %2\bin\out.txt -current %2\bin 
goto eof 

:uninstall 
JavaService.exe -uninstall JBoss30 
goto eof 

:usage 
echo -------- To Install JBoss 3.0 do 
echo Usage:   %0 jdk_home jboss_home (classic/hotspot/server) 
echo NOTE:    You MAY NOT use spaces in the path names. If you know how 
echo          to fix this, please tell me. 
echo          JDK 1.3 does not come with hotpot server by default, you must 
echo          install this seperately if you wish to use it. 
echo Example: %0 c:\progra~1\jdk c:\progra~1\jboss30 hotspot 
echo -------- 
echo -------- To Uninstall JBoss 3.0 do 
echo Usage:   %0 uninstall 
echo -------- 
goto eof 

:eof 
