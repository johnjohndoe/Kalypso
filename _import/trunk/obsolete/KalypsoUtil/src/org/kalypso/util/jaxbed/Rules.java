package org.kalypso.util.jaxbed;

import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;

/**
 * Holds a list of rules.
 * 
 * @author schlienger
 */
public class Rules
{
  private final Map m_map;

  public Rules( final RenderingRule[] rules )
  {
    m_map = new HashMap( rules.length );
    
    for( int i = 0; i < rules.length; i++ )
      m_map.put( new Integer( rules[i].getMask() ), rules[i] );
  }

  /**
   * Finds a rule that contains the mask
   */
  public RenderingRule findRule( final int mask ) throws NoSuchElementException
  {
    RenderingRule r = (RenderingRule)m_map.get( new Integer( mask ) );
    
    if( r == null )
      throw new NoSuchElementException();
    
    return r;
  }
}
