package org.kalypso.ogc.sensor.timeseries.wq;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * A List of WechmannSets sorted according to the date of validity of the
 * WechmannSet objects. You can call the iterator to step through the list in
 * the ascending order of date of validity.
 * 
 * @author schlienger
 */
public class WechmannSets
{
  private final List m_list;

  public WechmannSets( final WechmannSet[] wsets )
  {
    m_list = new ArrayList();
    m_list.addAll( Arrays.asList( wsets ) );

    Collections.sort( m_list, new WechmannSetComparator() );
  }

  public Iterator iterator()
  {
    return m_list.iterator();
  }
  
  /**
   * Returns a simple XML-Representation of this object.
   * 
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    final StringBuffer bf = new StringBuffer();
    
    for( final Iterator it = iterator(); it.hasNext(); )
    {
      final WechmannSet ws = (WechmannSet)it.next();

      bf.append( ws );
    }
    
    return bf.toString();
  }
}