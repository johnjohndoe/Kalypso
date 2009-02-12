set JAVA_HOME=C:\\j2sdk1.4.2_06
set CATALINA_HOME=%CURRENT_DIR%
set ECLIPSE_DEBUG=-Xdebug -Xnoagent -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=8099 -Djava.compiler=NONE

set JAVA_OPTS=-Xmx200m %ECLIPSE_DEBUG%
catalina.bat run -security 1  >../logs/kalypso.log 2>../logs/kalypso.err

rem set JAVA_OPTS=-Dkalypso.server.tempdir=%KALYPSO_SERVER_TMP% -Dkalypso.server.confdir=%KALYPSO_SERVER_CONF% -Dkalypso.server.datadir=%KALYPSO_SERVER_TEMP% %ECLIPSE_DEBUG%


rem set KALYPSO_SERVER_HOME=d:\Programme\kalypso
rem set JAVA_HOME=%KALYPSO_SERVER_HOME%\j2sdk1.4.2_02
rem set CATALINA_HOME=%KALYPSO_SERVER_HOME%\Tomcat5.0

rem set KALYPSO_SERVER_TMP=\\LFUG-KV-01\kalypso$
rem set KALYPSO_SERVER_CONF=%KALYPSO_SERVER_HOME%\data\conf
rem set KALYPSO_SERVER_DATA=%KALYPSO_SERVER_HOME%\data\mirrored

rem hier die Freigabe anwenden

rem set CATALINA_OPTS=-Djava.security.debug=help

rem catalina.bat run -security 1> %CATALINA_HOME%/logs/kalypso.log 2> %CATALINA_HOME%/logs/kalypso.err