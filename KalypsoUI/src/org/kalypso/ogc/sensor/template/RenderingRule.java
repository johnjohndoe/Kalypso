package org.kalypso.ogc.sensor.template;

import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.ObstableviewType.RulesType.RenderingruleType;

/**
 * Eine RenderingRule. Beinhaltet die Darstellung-Spezifikation Mask-abhängig.
 * 
 * @author schlienger
 */
public class RenderingRule
{
  private final RenderingruleType m_rule;

  public RenderingRule( final ObstableviewType.RulesType.RenderingruleType rule )
  {
    m_rule = rule;
  }
  
  /**
   * Testet ob diese Rule den Mask entspricht.
   */
  public boolean isMask( int mask )
  {
    return ( m_rule.getMask() & mask ) == m_rule.getMask();
  }
  
  public String getBackgroundcolor()
  {
    return m_rule.getBackgroundcolor();
  }
  public String getForegroundcolor()
  {
    return m_rule.getForegroundcolor();
  }
  public String getTooltip()
  {
    return m_rule.getTooltip();
  }
}
