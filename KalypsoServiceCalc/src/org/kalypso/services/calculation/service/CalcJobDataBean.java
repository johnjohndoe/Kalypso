package org.kalypso.services.calculation.service;

import java.io.Serializable;

/**
 * Bean zum Datenaustausch (Eingabe und Ausgabedaten)
 * 
 * @author belger
 */
public class CalcJobDataBean implements Serializable
{
  private String m_path;
  private String m_name;
  private String m_id;

  public CalcJobDataBean( final String id, final String name, final String path )
  {
    m_id = id;
    m_name = name;
    m_path = path;
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
  public final String getPath()
  {
    return m_path;
  }
  public final void setPath( String url )
  {
    m_path = url;
  }
}
