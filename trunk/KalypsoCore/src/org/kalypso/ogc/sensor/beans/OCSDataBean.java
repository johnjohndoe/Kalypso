package org.kalypso.ogc.sensor.beans;

import java.io.Serializable;

/**
 * Contains meta-information on the localisation of the data for an observation. OCSDataBean
 * are designed to be used within the Kalypso Observation Web Service API. They are used
 * on both client side and server side.
 * 
 * @author schlienger
 */
public class OCSDataBean implements Serializable
{
  /** Identifier of the observation bean it delivers data for */
  private String m_obsId;

  /** Location of the data, URL which should be accessible for both client and server */
  private String m_location;

  /** Internal id of this ODCDataBean */
  private int m_id;

  public OCSDataBean()
  {
    this( 0, "", "" );
  }

  /**
   * Constructs a new OCSDataBean.
   * 
   * @param id Internal id of this ODCDataBean
   * @param obsId Identifier of the observation bean it delivers data for
   * @param location Location of the data, URL which should be accessible for both client and server
   */
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