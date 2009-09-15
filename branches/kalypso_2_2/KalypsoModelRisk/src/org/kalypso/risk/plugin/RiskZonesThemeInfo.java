package org.kalypso.risk.plugin;

import java.util.Formatter;
import java.util.Map;
import java.util.Properties;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.gml.ui.map.CoverageThemeInfo;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeInfo;
import org.kalypso.risk.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_Position;

public class RiskZonesThemeInfo extends CoverageThemeInfo implements IKalypsoThemeInfo
{
  //  public static final String DEFAULT_FORMAT_STRING = Messages.getString( "org.kalypso.risk.plugin.RiskZonesThemeInfo.0" ); //$NON-NLS-1$

  private static SortedMap<Double, String> RISK_ZONES_MAP = new TreeMap<Double, String>();

  /**
   * @see org.kalypso.gml.ui.map.CoverageThemeInfo#init(org.kalypso.ogc.gml.IKalypsoTheme, java.util.Properties)
   */
  @Override
  public void init( final IKalypsoTheme theme, final Properties props )
  {
    super.init( theme, props );
    m_formatString = props.getProperty( PROP_FORMAT, "%.2f \u20ac/m\u00b2/a - %s" ); //$NON-NLS-1$
  }

  public static void updateZonesDefinition( final Map<Double, String> values )
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
      final Double key = value < 0.0 ? RISK_ZONES_MAP.tailMap( value ).firstKey() : RISK_ZONES_MAP.headMap( value ).lastKey();
      formatter.format( m_formatString, Math.abs( value ), RISK_ZONES_MAP.get( key ) );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      formatter.format( Messages.getString( "org.kalypso.risk.plugin.RiskZonesThemeInfo.1" ), e.toString() ); //$NON-NLS-1$
    }
  }

}
