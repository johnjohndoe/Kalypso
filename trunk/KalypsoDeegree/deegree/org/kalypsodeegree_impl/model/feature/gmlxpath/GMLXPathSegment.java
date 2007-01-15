/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.model.feature.gmlxpath;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.gmlxpath.xelement.IXElement;

/**
 * Each GMLXPathSegment represents the parts of the gmlxPath between two '/' <br>
 * 
 * @author doemming
 */
public final class GMLXPathSegment
{
  public static GMLXPathSegment[] addSegments( GMLXPathSegment[] segments, GMLXPathSegment segment )
  {
    final int parentLength = segments.length;
    final GMLXPathSegment[] newSegments = new GMLXPathSegment[parentLength + 1];
    System.arraycopy( segments, 0, newSegments, 0, parentLength );
    newSegments[parentLength] = segment;
    return newSegments;
  }

  public static GMLXPathSegment[] segmentsFromFeature( Feature feature ) throws GMLXPathException
  {
    final String id = feature.getId();
    if( id == null || id.length() < 1 )
      throw new GMLXPathException( "canot build gmlxpath for feature with invalid id" );
    else
      return new GMLXPathSegment[] { new GMLXPathSegment( feature ) };
  }

  public static GMLXPathSegment[] segmentsFromPath( final String path )
  {
    if( path.trim().length() == 0 )
      return new GMLXPathSegment[] {};

    final String[] segments = path.split( "/" );
    final GMLXPathSegment[] xSegments = new GMLXPathSegment[segments.length];
    for( int i = 0; i < segments.length; i++ )
      xSegments[i] = new GMLXPathSegment( segments[i] );

    return xSegments;
  }

  private final static XElementFactory m_fac = new XElementFactory();

  private transient final IXElement m_addressXElement;

  private transient final IXElement m_conditionXElement;

  private final String m_segment;

  public GMLXPathSegment( final Feature feature )
  {
    this( "id( '" + feature.getId() + "' )" );
  }

  public GMLXPathSegment( final String segmentString )
  {
    m_segment = segmentString;
    int index = segmentString.indexOf( "[" );
    final String address;
    final String condition;
    if( index < 0 )
    {
      address = segmentString;
      condition = null;
    }
    else
    {
      address = segmentString.substring( 0, index );
      condition = segmentString.substring( index );
    }

    m_addressXElement = m_fac.create( address );

    if( condition != null )
      m_conditionXElement = m_fac.create( condition );
    else
      m_conditionXElement = null;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return m_segment;
  }

  public IXElement getAddressXElement( )
  {
    return m_addressXElement;
  }

  public IXElement getConditionXElement( )
  {
    return m_conditionXElement;
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    return EqualsBuilder.reflectionEquals( this, obj );
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return HashCodeBuilder.reflectionHashCode( this );
  }

}