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
  private ServiceConfig()
  {
    // wird nicht instantitiert
  }
  
  public static File getConfDir()
  {
    return new File( System.getProperty( "kalypso.server.confdir" ) );
  }

  public static File getTempDir()
  {
    return new File( System.getProperty( "kalypso.server.tempdir" ) );
  }
  
  public static File getDataDir()
  {
    return new File( System.getProperty( "kalypso.server.datadir" ) );
  }

}
