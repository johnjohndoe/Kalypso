package org.kalypso.ogc.sensor;

import java.util.Properties;

/**
 * Metadata for Observations.
 * 
 * @author schlienger
 */
public class Metadata extends Properties
{
  public final static String MD_NAME = "Name";
  public final static String MD_DESCRIPTION = "Beschreibung";

  public Metadata()
  {
    super();
  }

  public Metadata( Properties arg0 )
  {
    super( arg0 );
  }
}
