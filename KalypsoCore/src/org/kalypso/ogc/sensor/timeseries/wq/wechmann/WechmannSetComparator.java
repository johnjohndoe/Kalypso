package org.kalypso.ogc.sensor.timeseries.wq.wechmann;

import java.util.Comparator;

/**
 * A Comparator for <code>WechmannSet</code> objects. It compares them
 * according to their date of validity.
 * 
 * @author schlienger
 */
public class WechmannSetComparator implements Comparator
{
  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare(Object o1, Object o2)
  {
    final WechmannSet ws1 = (WechmannSet) o1;
    final WechmannSet ws2 = (WechmannSet) o2;

    return ws1.getValidity().compareTo( ws2.getValidity() );
  }
}