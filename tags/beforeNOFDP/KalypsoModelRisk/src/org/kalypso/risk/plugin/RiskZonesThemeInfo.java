package org.kalypso.risk.plugin;

import java.util.Formatter;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.kalypso.gml.ui.map.CoverageThemeInfo;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeInfo;
import org.kalypsodeegree.model.geometry.GM_Position;

public class RiskZonesThemeInfo extends CoverageThemeInfo implements IKalypsoThemeInfo
{
  public static final String DEFAULT_FORMAT_STRING = "Zone: %s";

  private static Map<Double, String> RISK_ZONES_MAP = new HashMap<Double, String>();

  /**
   * @see org.kalypso.gml.ui.map.CoverageThemeInfo#init(org.kalypso.ogc.gml.IKalypsoTheme, java.util.Properties)
   */
  @Override
  public void init( final IKalypsoTheme theme, final Properties props )
  {
    super.init( theme, props );
    m_formatString = props.getProperty( PROP_FORMAT, DEFAULT_FORMAT_STRING );
  }

  public static void updateZonesDefinition( final HashMap<Double, String> values )
  {
    RISK_ZONES_MAP.clear();
    for( final Double key : values.keySet() )
      RISK_ZONES_MAP.put( key, values.get( key ) );
  }

  /**
   * @see org.kalypso.gml.ui.map.CoverageThemeInfo#appendQuickInfo(java.util.Formatter,
   *      org.kalypsodeegree.model.geometry.GM_Position)
   */
  @Override
  public void appendQuickInfo( final Formatter formatter, final GM_Position pos )
  {
    try
    {
      final Double value = getValue( pos );
      if( value == null )
        return;
      formatter.format( getFormatString(), RISK_ZONES_MAP.get( value ) );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      formatter.format( "Fehler: %s%n", e.toString() );
    }
  }

}
