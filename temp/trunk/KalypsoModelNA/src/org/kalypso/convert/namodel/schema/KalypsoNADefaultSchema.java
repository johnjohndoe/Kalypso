package org.kalypso.convert.namodel.schema;

import java.net.URL;

/**
 * @author doemming
 */
public class KalypsoNADefaultSchema
{
  private static KalypsoNADefaultSchema m_instance;

  public static KalypsoNADefaultSchema getInstance()
  {
    if( m_instance == null )
      m_instance = new KalypsoNADefaultSchema();
    return m_instance;
  }

  private KalypsoNADefaultSchema()
  {
    // nothing
  }

  public URL getDefaultNaModellSchemaURL()
  {
    return getClass().getResource( "namodell.xsd" );
  }

}