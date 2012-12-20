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
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.xml.serializer.ToXMLStream;
import org.junit.Test;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.GM_PolyhedralSurface;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.io.sax.marshaller.PolyhedralSurfaceMarshaller;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * @author Felipe Maximino
 */
public class MarshallPolyhedralSurfaceTest extends TestCase
{
  private final static String crs = "EPSG:31467"; //$NON-NLS-1$

  private static final GM_Position pos1 = GeometryFactory.createGM_Position( 0.0, 0.0, 1.0 );

  private static final GM_Position pos2 = GeometryFactory.createGM_Position( 0.0, 1.0, 2.0 );

  private static final GM_Position pos3 = GeometryFactory.createGM_Position( 1.0, 0.0, 3.0 );

  private static final GM_Position pos4 = GeometryFactory.createGM_Position( 0.0, 1.0, 1.0 );

  private static final GM_Position pos5 = GeometryFactory.createGM_Position( 1.0, 1.0, 2.0 );

  private static final GM_Position pos6 = GeometryFactory.createGM_Position( 1.0, 1.0, 1.0 );

  @Test
  public void testWritePolyhedralSurface( ) throws Exception
  {
    final GM_PolygonPatch[] polygons = createPolygons();
    final GM_PolyhedralSurface<GM_PolygonPatch> surface = GeometryFactory.createGM_PolyhedralSurface( polygons, crs );

    File polyFile = null;
    OutputStream os = null;
    try
    {
      polyFile = File.createTempFile( "polyTest", ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$
      polyFile.deleteOnExit();

      /* Output: to stream */
      os = new BufferedOutputStream( new FileOutputStream( polyFile ) );
      assertNotNull( os );

      final ToXMLStream xmlStream = new ToXMLStream();
      xmlStream.setOutputStream( os );
      // Configure content handler. IMPORTANT: call after setOutputStream!
      xmlStream.setLineSepUse( true );
      xmlStream.setIndent( true );
      xmlStream.setIndentAmount( 1 );
      xmlStream.setEncoding( "UTF-8" ); //$NON-NLS-1$

      final XMLReader xmlReader = XMLReaderFactory.createXMLReader();
      xmlReader.setContentHandler( xmlStream );

      xmlStream.startDocument();

      final PolyhedralSurfaceMarshaller marshaller = new PolyhedralSurfaceMarshaller( xmlReader );
      marshaller.marshall( surface );

      xmlStream.endDocument();

      os.close();

      final String xmlString = FileUtils.readFileToString( polyFile, "UTF-8" ); //$NON-NLS-1$
      System.out.println( xmlString );
    }
    finally
    {
      polyFile.delete();
      IOUtils.closeQuietly( os );
    }
  }

  private GM_PolygonPatch[] createPolygons( ) throws Exception
  {
    final GM_Position[] ring1 = new GM_Position[] { pos1, pos2, pos3, pos1 };
    final GM_Position[] ring2 = new GM_Position[] { pos3, pos4, pos5, pos6, pos3 };

    final List<GM_PolygonPatch> list = new ArrayList<>( 2 );

    list.add( GeometryFactory.createGM_PolygonPatch( ring1, null, crs ) );
    list.add( GeometryFactory.createGM_PolygonPatch( ring2, null, crs ) );

    return list.toArray( new GM_PolygonPatch[list.size()] );
  }
}
