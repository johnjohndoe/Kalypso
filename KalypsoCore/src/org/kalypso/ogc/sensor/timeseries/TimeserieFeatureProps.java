package org.kalypso.ogc.sensor.timeseries;

import java.util.Properties;

/**
 * Holds properties for a timeseries-feature. This class is used as a struct.
 * 
 * @author schlienger
 */
public class TimeserieFeatureProps
{
  //  public final static String PROP_TYPE = "type";
  //
  //  public final static String PROP_TYPENAME = "typeName";
  public final static String PROP_DIAGDATEAXIS = "diagDateAxis";

  public final static String PROP_DIAGVALUEAXIS = "diagValueAxis";

  public final static String PROP_NAMECOLUMN = "nameColumn";

  public final static String PROP_LINKCOLUM = "linkColumn";

  //  public final String _type;
  //
  //  public final String _typeName;

  public final String _nameColumn;

  public final String _linkColumn;

  public final String _diagDateAxis;

  public final String _diagValueAxis;

  public TimeserieFeatureProps( final Properties props )
  {
    this( props.getProperty( PROP_DIAGDATEAXIS ), props.getProperty( PROP_DIAGVALUEAXIS ), props
        .getProperty( PROP_NAMECOLUMN ), props.getProperty( PROP_LINKCOLUM ) );
  }

  public TimeserieFeatureProps( final String diagDateAxis, final String diagValueAxis,
      final String nameColumn, final String linkColumn )
  {
    _diagDateAxis = diagDateAxis;
    _diagValueAxis = diagValueAxis;
    _nameColumn = nameColumn;
    _linkColumn = linkColumn;
  }
}