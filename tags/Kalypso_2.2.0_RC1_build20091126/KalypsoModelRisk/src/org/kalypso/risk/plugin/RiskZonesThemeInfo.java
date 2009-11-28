package org.kalypso.risk.plugin;

import java.util.Formatter;
import java.util.Map;
import java.util.Properties;
import java.util.SortedMap;
import java.util.TreeMap;

import org.eclipse.jface.preference.IPreferenceStore;
import org.kalypso.gml.ui.map.CoverageThemeInfo;
import org.kalypso.ogc.gml.IKalypsoThemeInfo;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.preferences.KalypsoRiskPreferencePage;
import org.kalypsodeegree.model.geometry.GM_Position;

public class RiskZonesThemeInfo extends CoverageThemeInfo implements IKalypsoThemeInfo
{
  private static SortedMap<Double, String> RISK_ZONES_MAP = new TreeMap<Double, String>();

  /**
   * @see org.kalypso.gml.ui.map.CoverageThemeInfo#initFormatString(java.util.Properties)
   */
  @Override
  protected String initFormatString( Properties props )
  {
    final IPreferenceStore preferences = KalypsoRiskPreferencePage.getPreferences();
    final int digits = preferences.getInt( KalypsoRiskPreferencePage.KEY_RISKTHEMEINFO_IMPORTANTDIGITS );
    return props.getProperty( PROP_FORMAT, "%." + digits + "g \u20ac/m\u00b2/a - %s" ); //$NON-NLS-1$
  }

  // TODO: comment this strange stuff!
  public static void updateZonesDefinition( final Map<Double, String> values )
  {
    // FIXME: Bad! This class should pull information, not depend on another class to push the information here!
    RISK_ZONES_MAP.clear();
    RISK_ZONES_MAP.putAll( values );
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
      formatter.format( getFormatString(), Math.abs( value ), RISK_ZONES_MAP.get( key ) );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      formatter.format( Messages.getString( "org.kalypso.risk.plugin.RiskZonesThemeInfo.1" ), e.toString() ); //$NON-NLS-1$
    }
  }

}
