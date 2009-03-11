/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.filterencoding;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;

import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.model.feature.Feature;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Encapsulates the information of a <Filter>element that consists of a number of FeatureId constraints (only) (as
 * defined in the FeatureId DTD).
 * 
 * @author Markus Schneider
 * @version 06.08.2002
 */
public class FeatureFilter extends AbstractFilter
{

  /** FeatureIds the FeatureFilter is based on */
  ArrayList<FeatureId> m_featureIds = new ArrayList<FeatureId>();

  /** Adds a FeatureId constraint. */
  public void addFeatureId( FeatureId featureId )
  {
    m_featureIds.add( featureId );
  }

  /** Returns the contained FeatureIds. */
  public ArrayList<FeatureId> getFeatureIds( )
  {
    return m_featureIds;
  }

  /**
   * Calculates the <tt>FeatureFilter</tt>'s logical value based on the ID of the given <tt>Feature</tt>. FIXME!!!
   * Use a TreeSet (or something) to speed up comparison.
   * 
   * @param feature
   *          that determines the Id
   * @return true, if the <tt>FeatureFilter</tt> evaluates to true, else false
   */
  public boolean evaluate( Feature feature )
  {
    String id = feature.getId();
    for( int i = 0; i < m_featureIds.size(); i++ )
    {
      FeatureId featureId = m_featureIds.get( i );
      if( id.equals( featureId.getValue() ) )
        return true;
    }
    return false;
  }

  /** Produces an indented XML representation of this object. */
  @Override
  public StringBuffer toXML( )
  {
    StringBuffer sb = new StringBuffer( 500 );
    sb.append( "<ogc:Filter xmlns:ogc='http://www.opengis.net/ogc'>" );
    for( int i = 0; i < m_featureIds.size(); i++ )
    {
      FeatureId fid = m_featureIds.get( i );
      sb.append( fid.toXML() );
    }
    sb.append( "</ogc:Filter>" );
    return sb;
  }

  /**
   * @see org.kalypsodeegree_impl.filterencoding.AbstractFilter#clone()
   */
  @Override
  public Filter clone( ) throws CloneNotSupportedException
  {
    StringBuffer buffer = toXML();
    ByteArrayInputStream input = new ByteArrayInputStream( buffer.toString().getBytes() );
    Document asDOM = null;
    try
    {
      asDOM = XMLHelper.getAsDOM( input, true );
      Element element = asDOM.getDocumentElement();
      return AbstractFilter.buildFromDOM( element );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    throw new CloneNotSupportedException();
  }

  public void removeFeatureId( FeatureId id )
  {
    m_featureIds.remove( id );
  }
}