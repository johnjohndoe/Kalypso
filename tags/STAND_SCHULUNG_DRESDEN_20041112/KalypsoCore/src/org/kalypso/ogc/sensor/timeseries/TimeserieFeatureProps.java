package org.kalypso.ogc.sensor.timeseries;

import java.util.Properties;

/**
 * Holds properties for a timeseries-feature. This class is used as a struct.
 * 
 * @author schlienger
 */
public class TimeserieFeatureProps
{
  private final static String PROP_NAMECOLUMN = "nameColumn";

  private final static String PROP_LINKCOLUM = "linkColumn";

  private final String m_nameColumn;

  private final String m_linkColumn;

  private final String m_filter;

  private static final String PROP_FILTER = "filter";

  public TimeserieFeatureProps( final Properties props )
  {
    this( props.getProperty( PROP_NAMECOLUMN ), props.getProperty( PROP_LINKCOLUM ),props.getProperty( PROP_FILTER ) );
  }

  public TimeserieFeatureProps( final String nameColumn, final String linkColumn, final String filter )
  {
    m_nameColumn = nameColumn;
    m_linkColumn = linkColumn;
    m_filter = filter;
  }
  
  public String getFilter()
  {
    return m_filter;
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