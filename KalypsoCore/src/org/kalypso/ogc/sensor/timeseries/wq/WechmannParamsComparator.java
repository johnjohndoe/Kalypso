package org.kalypso.ogc.sensor.timeseries.wq;

import org.kalypso.java.util.DoubleComparator;

/**
 * Compares two <code>WechmannParams</code> objects.
 * <p>
 * This comparator internally uses a <code>DoubleComparator</code> to compare
 * the value of the WGR fields of the <code>WechmannParams</code>. Since the
 * unit of this field is 'cm', the DoubleComparator is created using a delta
 * value of 0.001
 * 
 * @author schlienger
 */
public class WechmannParamsComparator implements java.util.Comparator
{
  private final DoubleComparator m_dc;

  public WechmannParamsComparator()
  {
    m_dc = new DoubleComparator( 0.001 );
  }

  /**
   * @see com.sun.msv.datatype.xsd.Comparator#compare(java.lang.Object,
   *      java.lang.Object)
   */
  public int compare( final Object o1, final Object o2 )
  {
    WechmannParams wp1 = (WechmannParams)o1;
    WechmannParams wp2 = (WechmannParams)o2;

    return m_dc.compare( wp1.getWGR(), wp2.getWGR() );
  }
}