package org.kalypso.ogc.sensor.tableview.rules;

import org.kalypso.java.util.StringUtilities;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.tableview.ITableViewRules;
import org.kalypso.template.obstableview.TypeRenderingRule;

/**
 * RulesFactory
 * 
 * @author schlienger
 */
public class RulesFactory
{
  private static ITableViewRules DEFAULT_RULES = null;

  private RulesFactory( )
  {
    // not to be instanciated
  }

  /**
   * Factory method for creating a RenderingRule object with a binding object.
   * 
   * TODO: extend binding type to include name of an icon that would be loaded
   * here.
   * 
   * @param rr
   * @return RenderingRule
   */
  public static RenderingRule createRenderingRule( final TypeRenderingRule rr )
  {
    int mask = rr.getMask();
    String fg = rr.getForegroundcolor();
    String bg = rr.getBackgroundcolor();
    String font = rr.getFont();
    String tt = rr.getTooltip();

    return new RenderingRule( mask, fg == null ? null : StringUtilities
        .stringToColor( fg ), bg == null ? null : StringUtilities
        .stringToColor( bg ), font == null ? null : StringUtilities
        .stringToFont( font ), tt, null );
  }

  /**
   * @return default rules for Kalypso
   */
  public static ITableViewRules getDefaultRules( )
  {
    // lazy loading
    if( DEFAULT_RULES == null )
    {
      DEFAULT_RULES = new Rules();

      final int[] bits = { KalypsoStati.BIT_CHECK, KalypsoStati.BIT_REQUIRED,
          KalypsoStati.BIT_USER_MODIFIED };

      for( int i = 0; i < bits.length; i++ )
      {
        DEFAULT_RULES.addRule( new RenderingRule( bits[i], null, null, null,
            KalypsoStatusUtils.getTooltipFor( bits[i] ), KalypsoStatusUtils
                .getIconFor( bits[i] ) ) );
      }
    }

    return DEFAULT_RULES;
  }
}