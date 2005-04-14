
rem set KALYPSO_SERVER_HOME=\\134.28.87.75\KALYPSOSERVER$

set JAVA_HOME=C:\j2sdk1.4.2_08
set CATALINA_HOME=C:\kalypso_testing\KALYPSOSERVER\jakarta-tomcat-5.0.28

rem hier die Freigabe anwenden

rem set KALYPSO_SERVER_TMP=%KALYPSO_SERVER_HOME%\data\tmp

rem set KALYPSO_SERVER_CONF=%KALYPSO_SERVER_HOME%\data\conf

rem set KALYPSO_SERVER_DATA=%KALYPSO_SERVER_HOME%\data\mirrored

rem set JAVA_OPTS=-Xmx1024m -Dkalypso.server.tempdir=%KALYPSO_SERVER_TMP% -Dkalypso.server.confdir=%KALYPSO_SERVER_CONF% -Dkalypso.server.datadir=%KALYPSO_SERVER_TEMP%
set JAVA_OPTS=-Xmx1024m 
rem set CATALINA_OPTS=-Djava.security.debug=help

catalina.bat run -security 1> %CATALINA_HOME%/logs/kalypso.log 2> %CATALINA_HOME%/logs/kalypso.err