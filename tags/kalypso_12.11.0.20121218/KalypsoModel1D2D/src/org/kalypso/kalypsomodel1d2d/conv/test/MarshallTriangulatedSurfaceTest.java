/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.conv.test;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.xml.serializer.ToXMLStream;
import org.junit.Test;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.commons.xml.NS;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.io.sax.marshaller.TriangulatedSurfaceMarshaller;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * @author Felipe Maximino
 */
public class MarshallTriangulatedSurfaceTest extends TestCase
{
  private ToXMLStream m_xmlStream;

  private static final double DELTA = 0.00001;

  private static final GM_Position CHECK_POS_1 = GeometryFactory.createGM_Position( 0.0, 0.0, 0.0 );

  private static final GM_Position CHECK_POS_2 = GeometryFactory.createGM_Position( 0.0, 1.0, 1.0 );

  private static final GM_Position CHECK_POS_3 = GeometryFactory.createGM_Position( 1.0, 0.0, 2.0 );

  @Test
  public void testWriteTinyTin( ) throws Exception
  {
    File tinFile = null;
    try
    {
      tinFile = File.createTempFile( "tinTest", ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$
      tinFile.deleteOnExit();

      final URL gmlLocation = getClass().getResource( "/etc/testdata/conv/tinyTin.gml" ); //$NON-NLS-1$
      assertNotNull( gmlLocation );

      loadAndMarshall( tinFile, gmlLocation );

      assertContentEquals( gmlLocation, tinFile );
    }
    finally
    {
      tinFile.delete();
    }
  }

  @Test
  public void testWriteTinyTin2( ) throws Exception
  {
    final File tinFile = new File( System.getProperty( "user.dir" ) + "/src/org/kalypso/kalypsomodel1d2d/conv/test/tinTest.gml" ); //$NON-NLS-1$ //$NON-NLS-2$
    tinFile.deleteOnExit();

    final URL gmlLocation = getClass().getResource( "/etc/testdata/conv/tinyTin2.gml" ); //$NON-NLS-1$
    assertNotNull( gmlLocation );

    loadAndMarshall( tinFile, gmlLocation );

    assertTinFirstTriangle( tinFile );

  }

  private void loadAndMarshall( final File tinFile, final URL gmlLocation ) throws Exception
  {
    /* Output: to stream */
    OutputStream os = null;

    try
    {
      os = new BufferedOutputStream( new FileOutputStream( tinFile ) );
      assertNotNull( os );

      final Feature rootFeature = getRootFeature( gmlLocation );

      final GM_TriangulatedSurface tin = (GM_TriangulatedSurface) rootFeature.getProperty( new QName( "org.kalypso.deegree.gmlparsertest", "triangularSurfaceMember" ) ); //$NON-NLS-1$ //$NON-NLS-2$

      final XMLReader reader = initTinyTinMarshalling( os );

      final TriangulatedSurfaceMarshaller marshaller = new TriangulatedSurfaceMarshaller( reader, tin.getCoordinateSystem() );
      marshaller.marshall( tin );

      endTinyTinMarshalling();

      os.close();
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }

  private Feature getRootFeature( final URL gmlLocation ) throws Exception
  {
    final GMLWorkspace tinWorkspace;
    tinWorkspace = GmlSerializer.createGMLWorkspace( gmlLocation, null );
    final Feature rootFeature = tinWorkspace.getRootFeature();

    return rootFeature;
  }

  private void assertTinFirstTriangle( final File file ) throws Exception
  {
    try
    {
      final Feature rootFeature = getRootFeature( file.toURI().toURL() );

      final GM_TriangulatedSurface tin = (GM_TriangulatedSurface) rootFeature.getProperty( new QName( "org.kalypso.deegree.gmlparsertest", "triangularSurfaceMember" ) ); //$NON-NLS-1$ //$NON-NLS-2$

      final GM_Triangle triangle = tin.get( 0 );

      final String srs = triangle.getCoordinateSystem();

      assertEquals( "EPSG:31467", srs ); //$NON-NLS-1$

      final GM_Position[] exteriorRing = triangle.getExteriorRing();
      assertTrue( exteriorRing[0].getDistance( CHECK_POS_1 ) < DELTA );
      assertTrue( exteriorRing[1].getDistance( CHECK_POS_2 ) < DELTA );
      assertTrue( exteriorRing[2].getDistance( CHECK_POS_3 ) < DELTA );
      assertTrue( exteriorRing[3].getDistance( CHECK_POS_1 ) < DELTA );
    }
    finally
    {
      file.delete();
    }
  }

  private XMLReader initTinyTinMarshalling( final OutputStream os ) throws SAXException
  {
    final XMLReader reader = initMarshalling( os );

    m_xmlStream.startPrefixMapping( "xlink", NS.XLINK ); // the attribute does not trigger the prefix mapping //$NON-NLS-1$
    m_xmlStream.startPrefixMapping( "gml", NS.GML3 ); //$NON-NLS-1$
    m_xmlStream.startPrefixMapping( "xs", NS.XSD ); //$NON-NLS-1$
    m_xmlStream.startPrefixMapping( "ns1", "org.kalypso.deegree.gmlparsertest" ); //$NON-NLS-1$ //$NON-NLS-2$

    m_xmlStream.startElement( "", "TinFeature", "ns1:TinFeature" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    m_xmlStream.addAttribute( NS.XSD, "schemaLocation", "xs:schemaLocation", "string", "org.kalypso.deegree.gmlparsertest test.xsd" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    m_xmlStream.addAttribute( NS.GML3, "id", "gml:id", "string", "idvalue0" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    m_xmlStream.startElement( "", "triangularSurfaceMember", "ns1:triangularSurfaceMember" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    return reader;
  }

  private void endTinyTinMarshalling( ) throws SAXException
  {
    m_xmlStream.endElement( "", "triangularSurfaceMember", "ns1:triangularSurfaceMember" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    m_xmlStream.endElement( "", "TinFeature", "ns1:TinFeature" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    endMarshalling();
  }

  private void endMarshalling( ) throws SAXException
  {
    m_xmlStream.endDocument();
  }

  private XMLReader initMarshalling( final OutputStream os ) throws SAXException
  {
    m_xmlStream = new ToXMLStream();
    m_xmlStream.setOutputStream( os );
    // Configure content handler. IMPORTANT: call after setOutputStream!
    m_xmlStream.setLineSepUse( true );
    m_xmlStream.setIndent( true );
    m_xmlStream.setIndentAmount( 1 );
    m_xmlStream.setEncoding( "UTF-8" ); //$NON-NLS-1$

    final XMLReader xmlReader = XMLReaderFactory.createXMLReader();
    xmlReader.setContentHandler( m_xmlStream );

    m_xmlStream.startDocument();

    return xmlReader;
  }

  private void assertContentEquals( final URL location, final File file ) throws IOException
  {
    final String fileContent = FileUtils.readFileToString( new File( file.getAbsolutePath() ), System.getProperty( "file.encoding" ) ); //$NON-NLS-1$
    final String urlContent = UrlUtilities.toString( location, System.getProperty( "file.encoding" ) ); //$NON-NLS-1$
    assertEquals( fileContent, urlContent );
  }
}
