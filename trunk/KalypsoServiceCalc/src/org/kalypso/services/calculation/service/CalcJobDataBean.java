package org.kalypso.services.calculation.service;

import java.io.Serializable;

/**
 * Bean zum Datenaustausch (Eingabe und Ausgabedaten)
 * 
 * @author belger
 */
public class CalcJobDataBean implements Serializable
{
  private String m_url;
  private String m_name;
  private String m_id;

  public CalcJobDataBean( final String id, final String name, final String url )
  {
    m_id = id;
    m_name = name;
    m_url = url;
  }
  
  public final String getId()
  {
    return m_id;
  }
  public final void setId( String id )
  {
    m_id = id;
  }
  public final String getName()
  {
    return m_name;
  }
  public final void setName( String name )
  {
    m_name = name;
  }
  public final String getUrl()
  {
    return m_url;
  }
  public final void setUrl( String url )
  {
    m_url = url;
  }
}
