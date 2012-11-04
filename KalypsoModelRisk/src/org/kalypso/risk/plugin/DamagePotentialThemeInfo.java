package org.kalypso.risk.plugin;

import java.util.Formatter;
import java.util.Properties;

import org.kalypso.gml.ui.coverage.CoverageThemeInfo;
import org.kalypso.risk.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_Position;

public class DamagePotentialThemeInfo extends CoverageThemeInfo
{
  @Override
  protected String initFormatString( final Properties props )
  {
    final int digits = KalypsoRiskPlugin.getThemeInfoPrecision();
    final String i18Format = Messages.getString( "SpecificDamagePotentialMap.gismapview.themeInfoLabel" ); //$NON-NLS-1$
    final String customPrecision = String.format( "%%,.%df", digits ); //$NON-NLS-1$

    // FIXME: this prohibits that the annuality is displayed in the info tooltip!

    return props.getProperty( PROP_FORMAT, i18Format.replaceAll( "\\[CUSTOM_PRECISION\\]", customPrecision ) ); //$NON-NLS-1$
  }

  @Override
  public void appendQuickInfo( final Formatter formatter, final GM_Position pos )
  {
    try
    {
      final Double value = getValue( pos );
      if( value == null )
        return;
      formatter.format( getFormatString(), Math.abs( value ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      formatter.format( Messages.getString( "org.kalypso.risk.plugin.RiskZonesThemeInfo.1" ), e.toString() ); //$NON-NLS-1$
    }
  }
}