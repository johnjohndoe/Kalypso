/*
 * Created on 22.07.2004
 * 
 * Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.ogc.gml.outline;

import org.deegree.graphics.sld.Rule;
import org.deegree.model.feature.FeatureType;
import org.kalypso.ogc.gml.KalypsoUserStyle;

public class RuleTreeObject
{
  private Rule rule = null;
  private KalypsoUserStyle userStyle = null;
  private FeatureType ft = null;

  public RuleTreeObject(Rule rule, KalypsoUserStyle style, FeatureType ft)
  {
    this.rule = rule;
    this.userStyle = style;
    this.ft	= ft;
  }
  
  public KalypsoUserStyle getStyle()
  {
  	return userStyle;
  }
  
  public FeatureType getFeatureType()
  {
  	return ft;
  }
 
  public Rule getRule() 
  {
	return rule;
  }
  
  public String toString()
  {
    if( rule == null )
      return "<no styles set>";
    
    if( rule.getName() != null )
      return rule.getName();
    else if(rule.getTitle() != null)
    	return rule.getTitle();
    else 
    	return rule.toString();    
  }
}