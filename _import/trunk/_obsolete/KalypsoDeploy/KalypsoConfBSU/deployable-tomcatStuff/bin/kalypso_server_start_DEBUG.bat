set JAVA_HOME=C:\\j2sdk1.4.2_08
set CATALINA_HOME=%CURRENT_DIR%
set ECLIPSE_DEBUG=-Xdebug -Xnoagent -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=8099 -Djava.compiler=NONE
set JAVA_OPTS=-Xmx200m %ECLIPSE_DEBUG%
catalina.bat run -security 1  >../logs/kalypso.log 2>../logs/kalypso.err
