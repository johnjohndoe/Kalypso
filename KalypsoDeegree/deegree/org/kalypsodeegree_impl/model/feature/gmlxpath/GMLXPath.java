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

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.kalypsodeegree.model.feature.Feature;

/**
 * GMLXPath represents a xpath that can be processed via GMLXPathUtilities
 * 
 * @author doemming
 */
public class GMLXPath
{
  /** Separates two segments in the feature-path */
  public final static char SEGMENT_SEPARATOR = '/';

  /** Something between two '/' */
  private final GMLXPathSegment[] m_segments;

  private GMLXPath( final GMLXPathSegment[] segments )
  {
    m_segments = segments;
  }

  /**
   * creates a GMLXPath from a string
   * 
   * @param namespaceContext
   *            The context against which qname's within the path are resolved. Not to implementors: the xpath does not
   *            store a reference to this context and uses it only within the constructor.
   * @deprecated Use {@link #GMLXPath(String, NamespaceContext)} instead.
   */
  @Deprecated
  public GMLXPath( final String path )
  {
    this( path, null );
  }

  /**
   * creates a GMLXPath from a string
   * 
   * @param namespaceContext
   *            The context against which qname's within the path are resolved. Not to implementors: the xpath does not
   *            store a reference to this context and uses it only within the constructor.
   */
  public GMLXPath( final String path, final NamespaceContext namespaceContext )
  {
    this( GMLXPathSegment.segmentsFromPath( path, namespaceContext ) );
  }

  /**
   * creates a GMLXPath that points to feature
   */
  public GMLXPath( final Feature feature ) throws GMLXPathException
  {
    this( GMLXPathSegment.segmentsFromFeature( feature ) );
  }

  public GMLXPath( final GMLXPath parent, final QName segment )
  {
    this( GMLXPathSegment.addSegments( parent.m_segments, GMLXPathSegment.forQName( segment ) ) );
  }

  public GMLXPath( final QName qname )
  {
    this( GMLXPathSegment.forQName( qname ) );
  }

  public GMLXPath( final GMLXPathSegment segment )
  {
    this( new GMLXPathSegment[] { segment } );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buffer = new StringBuffer();

    for( int i = 0; i < m_segments.length; i++ )
    {
      buffer.append( m_segments[i].toString() );
      if( i != m_segments.length - 1 )
        buffer.append( SEGMENT_SEPARATOR );
    }
    return buffer.toString();
  }

  public int getSegmentSize( )
  {
    return m_segments.length;
  }

  public GMLXPathSegment getSegment( final int index )
  {
    return m_segments[index];
  }

  public GMLXPath getParentPath( )
  {
    final GMLXPathSegment[] segments = new GMLXPathSegment[m_segments.length - 1];
    System.arraycopy( m_segments, 0, segments, 0, m_segments.length - 1 );
    return new GMLXPath( segments );
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
