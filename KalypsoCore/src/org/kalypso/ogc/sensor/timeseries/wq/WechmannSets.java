package org.kalypso.ogc.sensor.timeseries.wq;

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
public class WechmannSets
{
  // used for persistence
  private final static String TAG_SET = "set";
  
  private final SortedMap m_map;

  public WechmannSets( final WechmannSet[] wsets )
  {
    m_map = new TreeMap( new WechmannSetComparator() );
    for( int i = 0; i < wsets.length; i++ )
      m_map.put( wsets[i].getValidity(), wsets[i] );
  }

  public Iterator iterator()
  {
    return m_map.values().iterator();
  }

  /**
   * Returns the WechmannSet that is valid for the given date.
   */
  public WechmannSet getFor( final Date d )
  {
    final Date[] dates = (Date[])m_map.keySet().toArray( new Date[0] );
    int i = Arrays.binarySearch( dates, d );

    if( i < 0 )
      i = -i - 1;

    return (WechmannSet)m_map.get( dates[i] );
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
      bf.append( "<" ).append( TAG_SET ).append( ">" );

      final WechmannSet ws = (WechmannSet)it.next();
      bf.append( ws );
      
      bf.append( "</" ).append( TAG_SET ).append( ">" );
    }

    return bf.toString();
  }
}