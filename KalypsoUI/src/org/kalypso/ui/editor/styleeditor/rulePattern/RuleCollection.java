/*
 * Created on 12.09.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.rulePattern;

import java.util.ArrayList;

import org.deegree.graphics.sld.Rule;

/**
 * @author F.Lindemann
 *  
 */
public class RuleCollection
{

  private ArrayList rules = null;

  private String id = null;

  private RuleCollection( Rule rule )
  {
    rules = new ArrayList();
    id = rule.getTitle();
    rules.add( rule );
  }

  public static RuleCollection getInstance( Rule rule )
  {
    return new RuleCollection( rule );
  }

  public void addRule( Rule rule )
  {
    rules.add( rule );
  }

  public void removeRule( Rule rule )
  {
    rules.remove( rule );
  }

  public String getId()
  {
    return id;
  }

  public Rule get( int index )
  {
    return (Rule)rules.get( index );
  }

  public int size()
  {
    return rules.size();
  }
}