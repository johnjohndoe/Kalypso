package com.bce.eind.core.strang;

/**
 * @author Belger
 */
public class ProfilInfo
{
  private final String m_name;

  private final String m_filename;

  private final String m_station;

  public ProfilInfo( final String station, final String filename )
  {
    m_station = station;
    m_name = station;
    m_filename = filename;
  }

  public String getStation( )
  {
    return m_station;
  }

  public String getName( )
  {
    return m_name;
  }

  public String getFilename( )
  {
    return m_filename;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return m_name;
  }
}
