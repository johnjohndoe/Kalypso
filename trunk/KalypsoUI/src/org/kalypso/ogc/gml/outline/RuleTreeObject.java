package org.kalypso.ogc.gml.outline;

import org.deegree.graphics.sld.Rule;
import org.kalypso.ogc.gml.KalypsoFeatureTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;

public class RuleTreeObject
{
  private Rule m_rule = null;

  private KalypsoUserStyle m_userStyle = null;

  private final KalypsoFeatureTheme m_theme;

  public RuleTreeObject( final Rule rule, final KalypsoUserStyle style, final KalypsoFeatureTheme theme )
  {
    m_rule = rule;
    m_userStyle = style;
    m_theme = theme;
  }

  public KalypsoUserStyle getStyle()
  {
    return m_userStyle;
  }

  public KalypsoFeatureTheme getTheme()
  {
    return m_theme;
  }
  
  public Rule getRule()
  {
    return m_rule;
  }

  public String toString()
  {
    if( m_rule == null )
      return "<no styles set>";

    if( m_rule.getName() != null )
      return m_rule.getName();
    else if( m_rule.getTitle() != null )
      return m_rule.getTitle();
    else
      return m_rule.toString();
  }
}