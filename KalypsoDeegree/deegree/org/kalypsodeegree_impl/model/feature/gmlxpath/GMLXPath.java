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

  public final static char TYPENAME_TAG_OPEN = '[';

  public final static char TYPENAME_TAG_CLOSE = ']';

  /** Something between two '/' */
  private final GMLXPathSegment[] m_segments;

  /**
   * creates a GMLXPath from a string
   */
  public GMLXPath( final String path )
  {
    if( path.trim().length() == 0 )
      m_segments = new GMLXPathSegment[] {};
    else
    {
      final String[] segments = path.split( "/" );
      m_segments = new GMLXPathSegment[segments.length];
      for( int i = 0; i < segments.length; i++ )
        m_segments[i] = new GMLXPathSegment( segments[i] );
    }
  }

  /**
   * creates a GMLXPath that points to feature
   */
  public GMLXPath( final Feature feature ) throws GMLXPathException
  {
    final String id = feature.getId();
    if( id == null || id.length() < 1 )
      throw new GMLXPathException( "canot buil gmlxpath for feature with invalid id" );
    else
      m_segments = new GMLXPathSegment[] { new GMLXPathSegment( feature ) };
  }

  // public GMLXPath( final GMLXPath parent, final String segment )
  // {
  // final int parentLength = parent.m_segments.length;
  // m_segments = new GMLXPathSegment[parentLength + 1];
  // System.arraycopy( parent.m_segments, 0, m_segments, 0, parentLength );
  // m_segments[parentLength] = new GMLXPathSegment( segment );
  // }

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

  public GMLXPathSegment getSegment( int index )
  {
    return m_segments[index];
  }
}
