package org.kalypso.ogc.sensor.tableview.template;

import java.awt.Color;
import java.awt.Font;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

/**
 * Holds a list of rules.
 * 
 * @author schlienger
 */
public class Rules
{
  private final RenderingRule[] m_rules;

  private final Map m_map = new HashMap();

  /**
   * Constructs this object with default rendering rules.
   */
  public Rules()
  {
    this( new RenderingRule[] { 
        new RenderingRule( 0x01, Color.BLACK, Color.WHITE, Font.decode(null), "" ),
        new RenderingRule( 0x02, Color.BLACK, Color.ORANGE, Font.decode(null), "" ),
        new RenderingRule( 0x04, Color.BLACK, Color.RED, Font.decode(null), "" )
        } );
  }
  
  public Rules( final RenderingRule[] rules )
  {
    m_rules = rules;
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
    
    for( int i = 0; i < m_rules.length; i++ )
    {
      if( m_rules[i].contains( mask ) )
        lrules.add( m_rules[i] );
    }

    r = (RenderingRule[])lrules.toArray( new RenderingRule[0]);
    m_map.put( new Integer( mask ), r );
    
    return r;
  }
}