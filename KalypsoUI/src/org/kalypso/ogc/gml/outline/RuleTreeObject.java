package org.kalypso.ogc.gml.outline;

import org.deegree.graphics.sld.Rule;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleCollection;

public class RuleTreeObject
{
  private Rule m_rule = null;

  private KalypsoUserStyle m_userStyle = null;

  private final IKalypsoFeatureTheme m_theme;

  public RuleTreeObject( final Rule rule, final KalypsoUserStyle style,
      final IKalypsoFeatureTheme theme )
  {
    m_rule = rule;
    m_userStyle = style;
    m_theme = theme;
  }

  public RuleTreeObject( final Object ruleObject, final KalypsoUserStyle style,
      final IKalypsoFeatureTheme theme )
  {
    // can be either a simple Rule or a collection of Pattern-Rules
    if( ruleObject instanceof Rule )
      m_rule = (Rule)ruleObject;
    // in case of pattern rules, just take the first rule for labeling the
    // outline view
    else if( ruleObject instanceof RuleCollection && ( (RuleCollection)ruleObject ).size() > 0 )
    {
      m_rule = ( (RuleCollection)ruleObject ).get( 0 );
    }
    m_userStyle = style;
    m_theme = theme;
  }

  public KalypsoUserStyle getStyle()
  {
    return m_userStyle;
  }

  public IKalypsoFeatureTheme getTheme()
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

    if( m_rule.getTitle() != null )
      return m_rule.getTitle();
    else if( m_rule.getName() != null )
      return m_rule.getName();
    else
      return m_rule.toString();
  }
}