package org.kalypso.ogc.sensor.beans;

import java.io.Serializable;

/**
 * Contains meta-information on the localisation of the data for an observation.
 * 
 * @author schlienger
 */
public class OCSDataBean implements Serializable
{
  private String m_obsId;

  private String m_location;

  private int m_id;

  public OCSDataBean()
  {
    this( 0, "", "" );
  }

  public OCSDataBean( final int id, final String obsId, final String location )
  {
    m_id = id;
    m_obsId = obsId;
    m_location = location;
  }

  public int getId()
  {
    return m_id;
  }

  public void setId( int id )
  {
    m_id = id;
  }

  public String getObsId()
  {
    return m_obsId;
  }

  public void setObsId( String id )
  {
    m_obsId = id;
  }

  public String getLocation()
  {
    return m_location;
  }

  public void setLocation( String location )
  {
    m_location = location;
  }
}