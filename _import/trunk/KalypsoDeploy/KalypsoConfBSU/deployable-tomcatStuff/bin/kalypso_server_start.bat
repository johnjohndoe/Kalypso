set JAVA_HOME=C:\\j2sdk1.4.2_08
set CATALINA_HOME=%CURRENT_DIR%
set JAVA_OPTS=-Xmx200m 
catalina.bat run -security 1  >../logs/kalypso.log 2>../logs/kalypso.err
