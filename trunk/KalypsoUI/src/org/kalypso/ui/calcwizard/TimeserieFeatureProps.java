package org.kalypso.ui.calcwizard;

import java.util.Properties;

/**
 * Holds properties for a timeseries-feature
 * 
 * @author schlienger
 */
public class TimeserieFeatureProps
{
  public final static String PROP_TYPE = "type";

  public final static String PROP_TYPENAME = "typeName";

  public final static String PROP_NAMECOLUMN = "nameColumn";

  public final static String PROP_LINKCOLUM = "linkColumn";

  public final String m_type;

  public final String m_typeName;

  public final String m_nameColumn;

  public final String m_linkColumn;

  public TimeserieFeatureProps( final Properties props )
  {
    this( props.getProperty(PROP_TYPE), props.getProperty(PROP_TYPENAME), props.getProperty(PROP_NAMECOLUMN), props.getProperty(PROP_LINKCOLUM) );
  }

  public TimeserieFeatureProps( final String type, final String typeName, final String nameColumn,
      final String linkColumn )
  {
    m_type = type;
    m_typeName = typeName;
    m_nameColumn = nameColumn;
    m_linkColumn = linkColumn;
  }
}