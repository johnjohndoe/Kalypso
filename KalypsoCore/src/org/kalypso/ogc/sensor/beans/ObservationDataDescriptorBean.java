package org.kalypso.ogc.sensor.beans;

import java.io.Serializable;

/**
 * Contains meta-information on the localisation of the data for an observation.
 * 
 * @author schlienger
 */
public class ObservationDataDescriptorBean implements Serializable
{
  private String m_location;

  private String m_format;

  public ObservationDataDescriptorBean()
  {
    this( "", "" );
  }

  public ObservationDataDescriptorBean( final String location, final String format )
  {
    m_location = location;
    m_format = format;
  }

  public String getLocation()
  {
    return m_location;
  }

  public String getFormat()
  {
    return m_format;
  }

  public void setFormat( String format )
  {
    m_format = format;
  }

  public void setLocation( String location )
  {
    m_location = location;
  }
}