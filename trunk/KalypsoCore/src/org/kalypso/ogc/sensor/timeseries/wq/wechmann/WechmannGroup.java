package org.kalypso.ogc.sensor.timeseries.wq.wechmann;

import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * A List of WechmannSets sorted according to the date of validity of the
 * WechmannSet objects. You can call the iterator to step through the list in
 * the ascending order of date of validity.
 * 
 * @author schlienger
 */
public class WechmannGroup
{
  private final SortedMap m_map;

  /**
   * @param wsets
   */
  public WechmannGroup( final WechmannSet[] wsets )
  {
    m_map = new TreeMap(  );
    for( int i = 0; i < wsets.length; i++ )
      m_map.put( wsets[i].getValidity(), wsets[i] );
  }

  /**
   * @return Iterator on the WechmannSet objects
   */
  public Iterator iterator()
  {
    return m_map.values().iterator();
  }

  /**
   * Returns the WechmannSet that is valid for the given date.
   * 
   * @param d
   * @return the set
   */
  public WechmannSet getFor(final Date d)
  {
    final Date[] dates = (Date[]) m_map.keySet().toArray( new Date[0] );
    int i = Arrays.binarySearch( dates, d );

    if( i < 0 )
      i = -i - 2;
    
    // TODO: check this please (wenn d smaller than any validity)
    if( i < 0 )
      return null;
    
    return (WechmannSet) m_map.get( dates[i] );
  }
}