package org.kalypso.ogc.sensor.timeseries.wq;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.java.util.DateUtilities;

/**
 * A Set of WechmannParams ordered by WGR and valid for the given Date.
 * 
 * @author schlienger
 */
public class WechmannSet
{
  private final SortedMap m_mapW;

  private final SortedMap m_mapQ;

  private final Date m_validity;

  // used for persistence
  private final static String TAG_VALIDITY = "validity";
  private final static String TAG_PARAMS = "params";
  
  
  /**
   * Constructor with Params only. Takes the minimum Date as delivered by
   * DateUtilities as validity.
   */
  public WechmannSet( final WechmannParams[] wps )
  {
    this( DateUtilities.getMinimum(), wps );
  }

  /**
   * Sets the WechmannParams, they will be sorted by WGR (ascending). The order
   * is taken into account by the iterator when you call
   * <code>WechmannSet.iterator()</code>.
   */
  public WechmannSet( final Date validity, final WechmannParams[] wps )
  {
    m_validity = validity;

    m_mapW = new TreeMap();
    m_mapQ = new TreeMap();

    for( int i = 0; i < wps.length; i++ )
    {
      m_mapW.put( new Double( wps[i].getWGR() ), wps[i] );
      m_mapQ.put( new Double( wps[i].getQ4WGR() ), wps[i] );
    }
  }

  /**
   * Returns an iterator over the WechmannParamList backed by this WechmannSet.
   * The list ist sorted according to the WechmannParams' WGR attribute.
   */
  public Iterator iterator()
  {
    return m_mapW.values().iterator();
  }

  /**
   * Returns the validity of this set of WechmannParams.
   */
  public Date getValidity()
  {
    return m_validity;
  }

  /**
   * Returns the WechmannParams that are relevant for the given Waterlevel.
   */
  public WechmannParams getForW( final double W )
  {
    final Double[] ds = (Double[])m_mapW.keySet().toArray( new Double[0] );
    int i = Arrays.binarySearch( ArrayUtils.toPrimitive( ds ), W );

    if( i < 0 )
      i = -i - 1;

    return (WechmannParams)m_mapW.get( ds[i] );
  }

  /**
   * Returns the WechmannParams that are relevant for the given Runoff.
   */
  public WechmannParams getForQ( final double Q )
  {
    final Double[] ds = (Double[])m_mapQ.keySet().toArray( new Double[0] );
    int i = Arrays.binarySearch( ArrayUtils.toPrimitive( ds ), Q );

    if( i < 0 )
      i = -i - 1;

    return (WechmannParams)m_mapQ.get( ds[i] );
  }

  /**
   * Returns a simple XML-Representation of this object. The format of the XML
   * is as follows:
   * 
   * <pre>
   * &lt;set&gt;
   *  &lt;validity&gt;15.10.2004 17:53:17&lt;/validity&gt;
   *  &lt;params&gt;
   *    &lt;w1&gt;-38,12000&lt;/w1&gt;
   *    &lt;lnk1&gt;-7,87274&lt;/lnk1&gt;
   *    &lt;k2&gt;2,25925&lt;/k2&gt;
   *    &lt;wgr&gt;170,00000&lt;/wgr&gt;
   *  &lt;/params&gt;
   *  &lt;params&gt;
   *    &lt;w1&gt;-43,32000&lt;/w1&gt;
   *    &lt;lnk1&gt;-7,24065&lt;/lnk1&gt;
   *    &lt;k2&gt;2,13100&lt;/k2&gt;
   *  &lt;/params&gt;
   * &lt;/set&gt;
   * </pre>
   * 
   * <p>
   * The attribute validity is optional, if no attribute is provided, it takes
   * the minimum Date that is delivered by the
   * <code>DateUtilities.getMinimum()</code> method.
   * 
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    final StringBuffer bf = new StringBuffer();
    final SimpleDateFormat df = new SimpleDateFormat( "yyyy-MM-dd HH:mm:ss" );

    bf.append( "<" ).append( TAG_VALIDITY ).append( " format=\"").append(df.toPattern()).append("\"" ).append( ">" );
    bf.append( df.format( m_validity ) );
    bf.append( "</" ).append( TAG_VALIDITY ).append( ">" );

    for( final Iterator it = iterator(); it.hasNext(); )
    {
      bf.append( "<" ).append( TAG_PARAMS ).append( ">" );
      
      final WechmannParams wp = (WechmannParams)it.next();
      bf.append( wp );
      
      bf.append( "</" ).append( TAG_PARAMS ).append( ">" );
    }

    return bf.toString();
  }
}