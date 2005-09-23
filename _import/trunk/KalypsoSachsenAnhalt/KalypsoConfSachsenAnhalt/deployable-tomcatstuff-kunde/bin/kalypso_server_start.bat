set KALYPSO_SERVER_HOME=d:\programme\kalypso

set JAVA_HOME=%KALYPSO_SERVER_HOME%\j2sdk1.4.2
set CATALINA_HOME=%KALYPSO_SERVER_HOME%\Tomcat5.0

set KALYPSO_SERVER_TMP=%CATALINA_HOME%\temp
set KALYPSO_SERVER_WEBDAV=%CATALINA_HOME%\webapps\webdav

REM The trailing '/' is important!
set KALYPSO_SERVER_CONF=http://%COMPUTERNAME%:8088/webdav/srvconf/

set JAVA_OPTS=-Xmx512m -Dkalypso.server.tempdir=%KALYPSO_SERVER_TMP% -Dkalypso.server.confdir=%KALYPSO_SERVER_CONF%

rem set CATALINA_OPTS=-Djava.security.debug=help

rem catalina.bat run -security 1> %CATALINA_HOME%/logs/kalypso.log 2> %CATALINA_HOME%/logs/kalypso.err
catalina.bat run 1> %CATALINA_HOME%/logs/kalypso.log 2> %CATALINA_HOME%/logs/kalypso.err