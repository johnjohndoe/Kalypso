set KALYPSO_SERVER_HOME=C:\\Programme\\KalypsoService
set JAVA_HOME=C:\\j2sdk1.4.2_06

set CATALINA_HOME=%KALYPSO_SERVER_HOME%\Tomcat5.0

set KALYPSO_SERVER_TMP=%CATALINA_HOME%\temp
set KALYPSO_SERVER_WEBDAV=%CATALINA_HOME%\webapps\webdav
set KALYPSO_SERVER_CONF=%KALYPSO_SERVER_WEBDAV%\srvconf
set KALYPSO_SERVER_DATA=%KALYPSO_SERVER_WEBDAV%\data

set JAVA_OPTS=-Xmx200m -Dkalypso.server.tempdir=%KALYPSO_SERVER_TMP% -Dkalypso.server.confdir=%KALYPSO_SERVER_CONF% -Dkalypso.server.datadir=%KALYPSO_SERVER_TEMP%

catalina.bat run -security 1> %CATALINA_HOME%/logs/kalypso.log 2> %CATALINA_HOME%/logs/kalypso.err


rem set CATALINA_OPTS=-Djava.security.debug=help