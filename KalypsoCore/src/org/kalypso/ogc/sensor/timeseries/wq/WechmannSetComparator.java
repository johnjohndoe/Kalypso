package org.kalypso.ogc.sensor.timeseries.wq;

import java.util.Comparator;

/**
 * A Comparator for <code>WechmannSet</code> objects. It compares them according
 * to their date of validity.
 * 
 * @author schlienger
 */
public class WechmannSetComparator implements Comparator
{
  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( Object o1, Object o2 )
  {
    WechmannSet ws1 = (WechmannSet)o1;
    WechmannSet ws2 = (WechmannSet)o2;
    
    long t1 = ws1.getValidity().getTime();
    long t2 = ws2.getValidity().getTime();
    
    if( t1 > t2 )
      return 1;
    
    if( t1 < t2 )
      return -1;
    
    return 0;
  }
}
