package org.kalypso.ogc.sensor.tableview.rules;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import org.kalypso.ogc.sensor.tableview.ITableViewRules;

/**
 * Holds a list of rules.
 * 
 * @author schlienger
 */
public class Rules implements ITableViewRules
{
  private final List m_rules = new ArrayList();

  private final Map m_map = new HashMap();

  public Rules()
  {
    // empty
  }
  
  /**
   * Constructor with given rules
   */
  public Rules( final RenderingRule[] rules )
  {
    m_rules.addAll(  Arrays.asList( rules ) );
  }

  public void addRule( final RenderingRule rule )
  {
    m_rules.add( rule );
  }
  
  public void removeRule( final RenderingRule rule )
  {
    m_rules.remove( rule );
  }
  
  /**
   * Finds a rule that contains the mask
   */
  public RenderingRule[] findRules( final int mask ) throws NoSuchElementException
  {
    RenderingRule[] r = (RenderingRule[])m_map.get( new Integer( mask ) );

    if( r != null )
      return r;

    List lrules = new ArrayList();
    
    for( Iterator it = m_rules.iterator(); it.hasNext(); )
    {
      RenderingRule rule = (RenderingRule)it.next();
      
      if( rule.contains( mask ) )
        lrules.add( rule );
    }

    r = (RenderingRule[])lrules.toArray( new RenderingRule[0]);
    m_map.put( new Integer( mask ), r );
    
    return r;
  }
}