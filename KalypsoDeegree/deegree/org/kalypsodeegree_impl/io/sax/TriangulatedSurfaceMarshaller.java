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
package org.kalypsodeegree_impl.io.sax;

import org.deegree.model.crs.UnknownCRSException;
import org.kalypso.commons.xml.NS;
import org.kalypso.transformation.CRSHelper;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.AttributesImpl;

/**
 * Marshalls a {@link org.kalypsodeegree.model.geometry.GM_TriangulatedSurface} into a sax content handler.
 * 
 * @author Gernot Belger
 */
public class TriangulatedSurfaceMarshaller
{
  private static final String TAG_TRIANGULATED_SURFACE = "TriangulatedSurface";

  // TODO: prefix handling is bad; obtain real prefix from where?
  private static final String QNAME_TRIANGULATED_SURFACE = "gml:" + TAG_TRIANGULATED_SURFACE;

  private static final String TAG_TRIANGLE_PATCHES = "trianglePatches";

  private static final String QNAME_TRIANGLE_PATCHES = "gml:" + TAG_TRIANGLE_PATCHES;

  private static final String TAG_TRIANGLE = "Triangle";

  private static final String QNAME_TRIANGLE = "gml:" + TAG_TRIANGLE;

  private static final String TAG_EXTERIOR = "exterior";

  private static final String QNAME_EXTERIOR = "gml:" + TAG_EXTERIOR;

  private static final String TAG_LINEAR_RING = "LinearRing";

  private static final String QNAME_LINEAR_RING = "gml:" + TAG_LINEAR_RING;

  private static final String TAG_POS = "pos";

  private static final String QNAME_POS = "gml:" + TAG_POS;

  private final XMLReader m_reader;

  private final GM_TriangulatedSurface m_surface;

  private static final Attributes EMPTY_ATTRIBUTES = new AttributesImpl();

  private static final char[] WHITESPACE = new char[] { ' ' };

  public TriangulatedSurfaceMarshaller( final XMLReader reader, final GM_TriangulatedSurface surface )
  {
    m_reader = reader;
    m_surface = surface;
  }

  public void marshal( ) throws SAXException
  {
    try
    {
      final ContentHandler contentHandler = m_reader.getContentHandler();

      final String crs = m_surface.getCoordinateSystem();
      final Attributes atts;
      if( crs != null )
        atts = createCrsAttributes( crs );
      else
        atts = EMPTY_ATTRIBUTES;

      contentHandler.startElement( NS.GML3, TAG_TRIANGULATED_SURFACE, QNAME_TRIANGULATED_SURFACE, atts );
      contentHandler.startElement( NS.GML3, TAG_TRIANGLE_PATCHES, QNAME_TRIANGLE_PATCHES, EMPTY_ATTRIBUTES );

      for( final GM_Triangle triangle : m_surface )
        marshalTriangle( triangle );

      contentHandler.endElement( NS.GML3, TAG_TRIANGLE_PATCHES, QNAME_TRIANGLE_PATCHES );
      contentHandler.endElement( NS.GML3, TAG_TRIANGULATED_SURFACE, QNAME_TRIANGULATED_SURFACE );
    }
    catch( final UnknownCRSException e )
    {
      throw new SAXException( e );
    }
  }

  private void marshalTriangle( final GM_Triangle triangle ) throws SAXException, UnknownCRSException
  {
    final ContentHandler contentHandler = m_reader.getContentHandler();

    final String crs = m_surface.getCoordinateSystem();
    final String crsTri = triangle.getCoordinateSystem();

    final AttributesImpl atts;
    if( crsTri != null && !crsTri.equals( crs ) )
      atts = createCrsAttributes( crsTri );
    else
      atts = new AttributesImpl();
    atts.addAttribute( "", "interpolation", "interpolation", "CDATA", "planar" );

    contentHandler.startElement( NS.GML3, TAG_TRIANGLE, QNAME_TRIANGLE, atts );
    contentHandler.startElement( NS.GML3, TAG_EXTERIOR, QNAME_EXTERIOR, EMPTY_ATTRIBUTES );

    // TODO: refaktor to generally write linear rings

    final GM_Position[] exteriorRing = triangle.getExteriorRing();
    if( exteriorRing.length != 0 )
    {
      contentHandler.startElement( NS.GML3, TAG_LINEAR_RING, QNAME_LINEAR_RING, EMPTY_ATTRIBUTES );

      for( final GM_Position position : exteriorRing )
        marshallPosition( position );

      contentHandler.endElement( NS.GML3, TAG_LINEAR_RING, QNAME_LINEAR_RING );
    }

    contentHandler.endElement( NS.GML3, TAG_EXTERIOR, QNAME_EXTERIOR );
    contentHandler.endElement( NS.GML3, TAG_TRIANGLE, QNAME_TRIANGLE );
  }

  private void marshallPosition( final GM_Position position ) throws SAXException
  {
    final ContentHandler contentHandler = m_reader.getContentHandler();

    // a position has no crs, so we have not attributes
    final Attributes posListAtts = EMPTY_ATTRIBUTES;

    contentHandler.startElement( NS.GML3, TAG_POS, QNAME_POS, posListAtts );

    final double[] asArray = position.getAsArray();
    for( final double d : asArray )
    {
      final String dString = Double.toString( d );
      final char[] charArray = dString.toCharArray();
      contentHandler.characters( charArray, 0, charArray.length );
      contentHandler.characters( WHITESPACE, 0, 1 );
    }

    contentHandler.endElement( NS.GML3, TAG_POS, QNAME_POS );
  }

  private AttributesImpl createCrsAttributes( final String crs ) throws UnknownCRSException
  {
    final AttributesImpl atts = new AttributesImpl();
    atts.addAttribute( "", "srsName", "srsName", "CDATA", crs );
    atts.addAttribute( "", "srsDimension", "srsDimension", "CDATA", "" + CRSHelper.getDimension( crs ) );
    return atts;
  }

}
