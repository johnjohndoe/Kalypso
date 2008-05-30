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
package org.kalypsodeegree_impl.io.sax;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.org.xml.sax.AttributesUtilities;
import org.kalypso.gmlschema.types.ListSimpleTypeHandler;
import org.kalypso.gmlschema.types.UnmarshallResultEater;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerDouble;
import org.kalypsodeegree_impl.model.geometry.GM_Triangle_Impl;
import org.kalypsodeegree_impl.model.geometry.GM_TriangulatedSurface_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * A content handler which parses a gml:TriangulatedSurface element.
 * <p>
 * Parsing must hence starts with the gml:TriangulatedSurface element.
 * </p>
 * 
 * @author Gernot Belger
 */
public class TriangulatedSurfaceContentHandler extends DefaultHandler
{
  private final static ListSimpleTypeHandler DOUBLE_LIST_PARSER = new ListSimpleTypeHandler( new XsdBaseTypeHandlerDouble() );

  private final List<GM_Position> m_poses = new ArrayList<GM_Position>();

  private List<GM_Triangle> m_triangles = null;

  private String m_crs;

  private Locator m_locator;

  private StringBuffer m_coordBuffer = null;

  private GM_Position[] m_triangle;

  private String m_currentCrs;

  private final UnmarshallResultEater m_resultEater;

  private GM_TriangulatedSurface_Impl m_triangulatedSurface;

  /**
   * @param reader
   *            The xml reader this content handler is currently connected to.
   * @param uri
   *            the uri parameter of the previous startElement call before using this content handler.
   * @param localName
   *            the localName parameter of the previous startElement call before using this content handler.
   * @param name
   *            the name parameter of the previous startElement call before using this content handler.
   * @param attributes
   *            the attributes parameter of the previous startElement call before using this content handler.
   */
  public TriangulatedSurfaceContentHandler( final UnmarshallResultEater resultEater )
  {
    m_resultEater = resultEater;
  }

  private static String parseCrsFromAttributes( final Attributes attributes, final String parentCS )
  {
    final String srsName = AttributesUtilities.getAttributeValue( attributes, "", "srsName", null );

    // TODO: move into helper class
    if( srsName == null )
      return parentCS;

    return srsName;
  }

  /**
   * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String,
   *      org.xml.sax.Attributes)
   */
  @Override
  public void startElement( final String uri, final String localName, final String name, final Attributes attributes ) throws SAXException
  {
    if( uri != NS.GML3 )
      throw new SAXParseException( "Unexpected namespace: " + uri, m_locator );

    if( "TriangulatedSurface".equals( localName ) )
    {
      m_coordBuffer = null;
      // TODO: maybe get parent cs from outside
      m_crs = parseCrsFromAttributes( attributes, null );
    }
    else if( "trianglePatches".equals( localName ) )
    {
      m_coordBuffer = null;
      m_triangles = new ArrayList<GM_Triangle>();
    }
    else if( "Triangle".equals( localName ) )
      m_coordBuffer = null;
    else if( "exterior".equals( localName ) )
      m_coordBuffer = null;
    else if( "LinearRing".equals( localName ) )
      m_coordBuffer = null;
    else if( "pos".equals( localName ) )
    {
      m_coordBuffer = new StringBuffer();
      m_currentCrs = parseCrsFromAttributes( attributes, m_crs );
    }
  }

  /**
   * @see org.xml.sax.helpers.DefaultHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
   */
  @Override
  public void endElement( final String uri, final String localName, final String name ) throws SAXException
  {
    if( uri != NS.GML3 )
      throw new SAXParseException( "Unexpected namespace: " + uri, m_locator );

    if( "TriangulatedSurface".equals( localName ) )
    {
      if( m_triangulatedSurface == null )
        throw new SAXParseException( "Unexpected end of 'TriangulatedSurface': no content found.", m_locator );

      m_resultEater.unmarshallSuccesful( m_triangulatedSurface );
    }
    else if( "trianglePatches".equals( localName ) )
    {
      if( m_triangles == null )
        throw new SAXParseException( "Unexpected end of 'trianglePatches': " + uri, m_locator );

      try
      {
        m_triangulatedSurface = new GM_TriangulatedSurface_Impl( m_triangles, m_crs );
      }
      catch( final GM_Exception e )
      {
        throw new SAXException( e );
      }
    }
    else if( "Triangle".equals( localName ) )
    {
      if( m_triangle == null )
        throw new SAXParseException( "Triangle contains no valid exterior.", m_locator );

      try
      {
        final GM_Triangle_Impl gmTriangle = GeometryFactory.createGM_Triangle( m_triangle[0], m_triangle[1], m_triangle[2], m_currentCrs );
        m_triangles.add( gmTriangle );
      }
      catch( final GM_Exception e )
      {
        throw new SAXException( e );
      }

    }
    else if( "exterior".equals( localName ) )
    {
      if( m_triangle == null )
        throw new SAXParseException( "Exterior contains no valid linear ring.", m_locator );
    }
    else if( "LinearRing".equals( localName ) )
    {
      if( m_poses == null )
        throw new SAXParseException( "Exterior contains no valid linear ring.", m_locator );

      if( m_poses.size() != 4 )
        throw new SAXParseException( "LinearRing must contain exactly 4 coordinates: " + m_poses.size(), m_locator );

      m_triangle = new GM_Position[] { m_poses.get( 0 ), m_poses.get( 1 ), m_poses.get( 2 ) };
      m_poses.clear();
    }
    else if( "pos".equals( localName ) )
    {
      endPos();
    }

    super.endElement( uri, localName, name );
  }

  @SuppressWarnings("unchecked")
  private void endPos( ) throws SAXParseException
  {
    if( m_coordBuffer == null )
      throw new SAXParseException( "Unexpected 'posList'", m_locator );

    final String coordsString = m_coordBuffer.toString();
    try
    {
      final List<Double> doubles = (List<Double>) DOUBLE_LIST_PARSER.parseType( coordsString );

// final int dimension = m_currentCrs.getDimension();
// TODO: check against crs
      final int coordCount = doubles.size();

      // HACK: as long as we have no variable sized coordinates, we have only the choice between dimension 2 or 3.
      if( coordCount >= 3 )
        m_poses.add( GeometryFactory.createGM_Position( doubles.get( 0 ), doubles.get( 1 ), doubles.get( 2 ) ) );
      else if( coordCount == 2 )
        m_poses.add( GeometryFactory.createGM_Position( doubles.get( 0 ), doubles.get( 1 ) ) );
      else
        throw new SAXParseException( "Not enough coords in pos element: " + coordsString, m_locator );

    }
    catch( final ParseException e )
    {
      throw new SAXParseException( "Unable to parse pos-content: " + coordsString, m_locator, e );
    }
    finally
    {
      m_coordBuffer = null;
    }

  }

  /**
   * @see org.xml.sax.helpers.DefaultHandler#setDocumentLocator(org.xml.sax.Locator)
   */
  @Override
  public void setDocumentLocator( final Locator locator )
  {
    m_locator = locator;
  }

  /**
   * @see org.xml.sax.helpers.DefaultHandler#characters(char[], int, int)
   */
  @Override
  public void characters( final char[] ch, final int start, final int length )
  {
    if( m_coordBuffer != null )
      m_coordBuffer.append( ch, start, length );
  }
}