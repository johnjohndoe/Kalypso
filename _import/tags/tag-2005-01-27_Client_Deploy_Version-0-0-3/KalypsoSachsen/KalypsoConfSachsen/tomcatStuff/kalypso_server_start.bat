set KALYPSO_SERVER_HOME=d:\Programme\kalypso

set JAVA_HOME=%KALYPSO_SERVER_HOME%\j2sdk1.4.2_02
set CATALINA_HOME=%KALYPSO_SERVER_HOME%\Tomcat5.0

rem hier die Freigabe anwenden
set KALYPSO_SERVER_TMP=\\LFUG-KV-01\kalypso$

set KALYPSO_SERVER_CONF=%KALYPSO_SERVER_HOME%\data\conf

set KALYPSO_SERVER_DATA=%KALYPSO_SERVER_HOME%\data\mirrored

set JAVA_OPTS=-Xmx1024m -Dkalypso.server.tempdir=%KALYPSO_SERVER_TMP% -Dkalypso.server.confdir=%KALYPSO_SERVER_CONF% -Dkalypso.server.datadir=%KALYPSO_SERVER_TEMP%

rem set CATALINA_OPTS=-Djava.security.debug=help

catalina.bat run -security 1> %CATALINA_HOME%/logs/kalypso.log 2> %CATALINA_HOME%/logs/kalypso.err