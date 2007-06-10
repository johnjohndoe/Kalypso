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
package org.kalypsodeegree.model.geometry;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.org.xml.sax.AttributesUtilities;
import org.kalypso.gmlschema.types.ListSimpleTypeHandler;
import org.kalypso.gmlschema.types.UnmarshallResultEater;
import org.kalypso.jts.Triangle;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandlerDouble;
import org.kalypsodeegree_impl.model.cs.Adapters;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.cs.CoordinateSystem;
import org.kalypsodeegree_impl.model.geometry.GM_Triangle_Impl;
import org.kalypsodeegree_impl.model.geometry.GM_TriangulatedSurface_Impl;
import org.opengis.cs.CS_CoordinateSystem;
import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

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

  private final static GeometryFactory FACTORY = new GeometryFactory();

  private List<GM_Triangle> m_triangles = null;

  private CS_CoordinateSystem m_crs;

  private Locator m_locator;

  private StringBuffer m_coordBuffer = null;

  private Coordinate[] m_coords;

  private CS_CoordinateSystem m_currentCrs;

  private final UnmarshallResultEater m_resultEater;

  private GM_TriangulatedSurface_Impl m_triangulatedSurface;

  private Triangle m_triangle;

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

  private static CS_CoordinateSystem parseCrsFromAttributes( final Attributes attributes, final CS_CoordinateSystem parentCS )
  {
    final String srsName = AttributesUtilities.getAttributeValue( attributes, "", "srsName", null );

    // TODO: move into helper class
    if( srsName == null )
      return parentCS;

    final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    final Adapters csAdapter = org.kalypsodeegree_impl.model.cs.Adapters.getDefault();
    final CoordinateSystem csByName = csFac.getCSByName( srsName );
    return csAdapter.export( csByName );
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
    else if( "posList".equals( localName ) )
    {
      m_coordBuffer = new StringBuffer();

      m_currentCrs = parseCrsFromAttributes( attributes, m_crs );

      final String countString = AttributesUtilities.getAttributeValue( attributes, "", "count", null );
      if( countString != null )
      {
        final int count = Integer.parseInt( countString );
        m_coords = new Coordinate[count];
      }
      else
        m_coords = new Coordinate[0];
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

      m_triangulatedSurface = new GM_TriangulatedSurface_Impl( m_triangles, m_crs );
    }
    else if( "Triangle".equals( localName ) )
    {
      if( m_triangle == null )
        throw new SAXParseException( "Triangle contains no valid exterior.", m_locator );

      final GM_Triangle_Impl gmTriangle = new GM_Triangle_Impl( m_triangle, m_currentCrs );

      m_triangles.add( gmTriangle );
    }
    else if( "exterior".equals( localName ) )
    {
      if( m_triangle == null )
        throw new SAXParseException( "Exterior contains no valid linear ring.", m_locator );
    }
    else if( "LinearRing".equals( localName ) )
    {
      if( m_coords == null )
        throw new SAXParseException( "Exterior contains no valid linear ring.", m_locator );

      if( m_coords.length != 4 )
        throw new SAXParseException( "LinearRing must contain exactly 4 coodirnates: " + m_coords.length, m_locator );

      final Coordinate c0 = m_coords[0];
      final Coordinate c1 = m_coords[1];
      final Coordinate c2 = m_coords[2];
      m_triangle = new Triangle( c0, c1, c2, FACTORY );

      m_coords = null;
    }
    else if( "posList".equals( localName ) )
    {
      endPosList();
    }

    super.endElement( uri, localName, name );
  }

  @SuppressWarnings("unchecked")
  private void endPosList( ) throws SAXParseException
  {
    if( m_coordBuffer == null )
      throw new SAXParseException( "Unexpected 'posList'", m_locator );

    final String coordsString = m_coordBuffer.toString();
    try
    {
      final List<Double> doubles = (List<Double>) DOUBLE_LIST_PARSER.parseType( coordsString );

      final int dimension = m_currentCrs.getDimension();

      final int count;
      final int dimensionToUse; // the dimension to use for parsing the doubles
      if( m_coords.length == 0 )
      {
        // If the count attribute was omiited, we calculate the number of cooridnates from the dimension
        count = doubles.size() / dimension;
        dimensionToUse = dimension;
        m_coords = new Coordinate[count];
      }
      else
      {
        count = m_coords.length;
        dimensionToUse = (doubles.size() / count);
      }

      for( int i = 0; i < count; i++ )
      {
        // HACK: as long as we have no variable sized coordinates, we have only the choice between dimension 2 or 3.
        final int j = i * dimensionToUse;
        if( dimensionToUse < 3 )
          m_coords[i] = new Coordinate( doubles.get( j ), doubles.get( j + 1 ) );
        else
          m_coords[i] = new Coordinate( doubles.get( j ), doubles.get( j + 1 ), doubles.get( j + 2 ) );
      }

    }
    catch( final ParseException e )
    {
      throw new SAXParseException( "Unable to parse posList-content: " + coordsString, m_locator, e );
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
