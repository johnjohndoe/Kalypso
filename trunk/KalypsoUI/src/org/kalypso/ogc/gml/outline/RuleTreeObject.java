package org.kalypso.ogc.gml.outline;

import org.deegree.graphics.sld.Rule;
import org.deegree.model.feature.FeatureType;
import org.kalypso.ogc.gml.KalypsoUserStyle;

public class RuleTreeObject
{
  private Rule m_rule = null;
  private KalypsoUserStyle m_userStyle = null;
  private FeatureType m_ft = null;

  public RuleTreeObject( final Rule rule, final KalypsoUserStyle style, final FeatureType ft )
  {
    this.m_rule = rule;
    this.m_userStyle = style;
    this.m_ft	= ft;
  }
  
  public KalypsoUserStyle getStyle()
  {
  	return m_userStyle;
  }
  
  public FeatureType getFeatureType()
  {
  	return m_ft;
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
    else if(m_rule.getTitle() != null)
    	return m_rule.getTitle();
    else 
    	return m_rule.toString();    
  }
}