package org.kalypso.services.common;

import java.io.File;

/**
 * Helper Klasse für die Services, um auf gemeinsame Konfigurationsdaten
 * zuzugreifen
 * 
 * @author belger
 */
public class ServiceConfig
{
  public final static String CONF_DIR = "kalypso.server.confdir";
  public final static String TEMP_DIR = "kalypso.server.tempdir";
  public final static String DATA_DIR = "kalypso.server.datadir";
  
  private ServiceConfig()
  {
    // wird nicht instantitiert
  }
  
  public static File getConfDir()
  {
    return new File( System.getProperty( CONF_DIR ) );
  }

  public static File getTempDir()
  {
    return new File( System.getProperty( TEMP_DIR ) );
  }
  
  public static File getDataDir()
  {
    return new File( System.getProperty( DATA_DIR ) );
  }
}
