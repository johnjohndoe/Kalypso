package org.kalypso.ogc.sensor.timeseries.wq;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

/**
 * A Set of WechmannParams ordered by WGR and valid for the given Date.
 * 
 * @author schlienger
 */
public class WechmannSet
{
  private final List m_list = new ArrayList();

  private final Date m_validity;

  public WechmannSet( final Date validity )
  {
    m_validity = validity;
  }
  
  public WechmannSet( final Date validity, WechmannParams[] wps )
  {
    this( validity );
    setParamList( wps );
  }

  /**
   * Sets the WechmannParams, they will be sorted by WGR (ascending). The order
   * is taken into account by the iterator when you call <code>WechmannSet.iterator()</code>.
   */
  public void setParamList( final WechmannParams[] wp )
  {
    m_list.addAll( Arrays.asList( wp ) );
    
    Collections.sort( m_list, new WechmannParamsComparator() );
  }

  /**
   * Returns an iterator over the WechmannParamList backed by this WechmannSet. The list
   * ist sorted according to the WechmannParams' WGR attribute.
   */
  public Iterator iterator()
  {
    return m_list.iterator();
  }

  /**
   * Returns the validity of this set of WechmannParams.
   */
  public Date getValidity()
  {
    return m_validity;
  }
  
  /**
   * Returns a simple XML-Representation of this object.
   * 
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    final StringBuffer bf = new StringBuffer();
    final DateFormat df = DateFormat.getDateTimeInstance();

    bf.append( "<set>" );
    
    bf.append( "<validity>" );
    bf.append( df.format( m_validity ) );
    bf.append( "</validity>" );
    
    for( final Iterator it = iterator(); it.hasNext(); )
    {
      final WechmannParams wp = (WechmannParams)it.next();

      bf.append( wp );
    }
    
    bf.append( "</set>" );
    
    return bf.toString();
  }
}