package org.kalypso.ogc.sensor;

import java.util.Properties;

/**
 * Metadata for Observations.
 * 
 * @author schlienger
 */
public class MetadataList extends Properties
{
  public final static String MD_NAME = "Name";
  public final static String MD_DESCRIPTION = "Beschreibung";

  public MetadataList()
  {
    super();
  }

  public MetadataList( Properties arg0 )
  {
    super( arg0 );
  }
}
