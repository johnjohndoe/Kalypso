set KALYPSO_SERVER_HOME=d:\kalypso

set JAVA_HOME=%KALYPSO_SERVER_HOME%\j2sdk1.4.2_06
set CATALINA_HOME=%KALYPSO_SERVER_HOME%\Tomcat5.0

set KALYPSO_SERVER_WEBDAV=%CATALINA_HOME%\webapps\webdav
set KALYPSO_SERVER_TMP=%CATALINA_HOME%\temp

REM The trailing '/' is important!
set KALYPSO_SERVER_CONF=http://%COMPUTERNAME%:8080/webdav/srvconf/

rem set ECLIPSE_DEBUG=-Xdebug -Xnoagent -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=8000 -Djava.compiler=NONE
set ECLIPSE_DEBUG=-Xdebug -Xnoagent -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8000

set JAVA_OPTS=-Xmx512m -Dkalypso.server.tempdir=%KALYPSO_SERVER_TMP% -Dkalypso.server.confdir=%KALYPSO_SERVER_CONF% %ECLIPSE_DEBUG%

rem set CATALINA_OPTS=-Djava.security.debug=help

catalina.bat run -security 1> %CATALINA_HOME%/logs/kalypso.log 2> %CATALINA_HOME%/logs/kalypso.err