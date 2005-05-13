set KALYPSO_SERVER_HOME=d:\Programme\kalypso

set JAVA_HOME=%KALYPSO_SERVER_HOME%\j2sdk1.4.2_02
set CATALINA_HOME=%KALYPSO_SERVER_HOME%\Tomcat5.0

set KALYPSO_SERVER_TMP=%CATALINA_HOME%\temp
set KALYPSO_SERVER_WEBDAV=%CATALINA_HOME%\webapps\webdav
set KALYPSO_SERVER_CONF=%KALYPSO_SERVER_WEBDAV%\srvconf
set KALYPSO_SERVER_DATA=%KALYPSO_SERVER_WEBDAV%\data

set JAVA_OPTS=-Xmx392m -Dkalypso.server.tempdir=%KALYPSO_SERVER_TMP% -Dkalypso.server.confdir=%KALYPSO_SERVER_CONF% -Dkalypso.server.datadir=%KALYPSO_SERVER_TEMP%

rem set CATALINA_OPTS=-Djava.security.debug=help

catalina.bat run -security 1> %CATALINA_HOME%/logs/kalypso.log 2> %CATALINA_HOME%/logs/kalypso.err