package org.kalypso.ogc.sensor.timeseries;

import java.util.Properties;

/**
 * Holds properties for a timeseries-feature. This class is used as a struct.
 * 
 * @author schlienger
 */
public class TimeserieFeatureProps
{
  private final static String PROP_DIAGDATEAXIS = "diagDateAxis";

  private final static String PROP_DIAGVALUEAXIS = "diagValueAxis";

  private final static String PROP_NAMECOLUMN = "nameColumn";

  private final static String PROP_LINKCOLUM = "linkColumn";

  private final String m_nameColumn;

  private final String m_linkColumn;

  private final String m_diagDateAxis;

  private final String m_diagValueAxis;

  public TimeserieFeatureProps( final Properties props )
  {
    this( props.getProperty( PROP_DIAGDATEAXIS ), props.getProperty( PROP_DIAGVALUEAXIS ), props
        .getProperty( PROP_NAMECOLUMN ), props.getProperty( PROP_LINKCOLUM ) );
  }

  public TimeserieFeatureProps( final String diagDateAxis, final String diagValueAxis,
      final String nameColumn, final String linkColumn )
  {
    m_diagDateAxis = diagDateAxis;
    m_diagValueAxis = diagValueAxis;
    m_nameColumn = nameColumn;
    m_linkColumn = linkColumn;
  }
  
  /**
   * @return Returns the diagDateAxis.
   */
  public String getDiagDateAxis( )
  {
    return m_diagDateAxis;
  }
  
  /**
   * @return Returns the diagValueAxis.
   */
  public String getDiagValueAxis( )
  {
    return m_diagValueAxis;
  }
  
  /**
   * @return Returns the linkColumn.
   */
  public String getLinkColumn( )
  {
    return m_linkColumn;
  }
  
  /**
   * @return Returns the nameColumn.
   */
  public String getNameColumn( )
  {
    return m_nameColumn;
  }
}