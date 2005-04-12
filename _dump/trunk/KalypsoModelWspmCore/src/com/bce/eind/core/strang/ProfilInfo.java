package com.bce.eind.core.strang;

/**
 * @author gernot
 */
public class ProfilInfo
{
  private final String m_name;

  private final String m_path;

  public ProfilInfo( final String name, final String path )
  {
    m_name = name;
    m_path = path;
  }

  public String getName( )
  {
    return m_name;
  }

  public String getPath( )
  {
    return m_path;
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return m_name;
  }
}
