package org.kalypso.ogc.gml.outline;

import org.deegree.graphics.sld.Rule;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.KalypsoUserStyle;

public class RuleTreeObject
{
  private Rule m_rule = null;
  private KalypsoUserStyle m_userStyle = null;
  private IKalypsoLayer layer = null;

  public RuleTreeObject( final Rule rule, final KalypsoUserStyle style, final IKalypsoLayer layer )
  {
    this.m_rule = rule;
    this.m_userStyle = style;
    this.layer	= layer;
  }
  
  public KalypsoUserStyle getStyle()
  {
  	return m_userStyle;
  }
  
 
  public Rule getRule() 
  {
	return m_rule;
  }
  
  public IKalypsoLayer getLayer() 
  {
	return layer;
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