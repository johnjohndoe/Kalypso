/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.test.gml.serializer;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.io.StreamUtilities;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.Document;

/**
 * Test does a load & save & load & save on a gml file<br>
 * Test is intended to fila on differencess the two saved files
 * 
 * @author doemming
 */
public class GMLSerializerTest extends TestCase
{
  /**
   * @see junit.framework.TestCase#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    // KalypsoTest.init();
    KalypsoGisPlugin.getDefault();
  }

  /**
   * @see junit.framework.TestCase#tearDown()
   */
  @Override
  protected void tearDown( ) throws Exception
  {
    // KalypsoTest.release();
  }

  public void testNAModell( ) throws Exception
  {
    final URL resource = getClass().getResource( "resources/calcCase.gml" );
    multiLoadAndSaveGMLWorkspace( resource );
  }

  public void testNAHydros( ) throws Exception
  {
    final URL resource = getClass().getResource( "resources/calcHydrotop.gml" );
    multiLoadAndSaveGMLWorkspace( resource );
  }

  // not working at the moment as the schemacatalog is not available in test
  // TODO reactivate test soon
  public void testWspmTuhhModel( ) throws Exception
  {
    final URL resource = getClass().getResource( "resources/wspmTuhhModel.gml" );
    multiLoadAndSaveGMLWorkspace( resource );
  }

  /**
   * Class under test for GMLWorkspace createGMLWorkspace(URL)
   */
  public void multiLoadAndSaveGMLWorkspace( final URL resourceURL ) throws Exception
  {
    final File testFileOrg = File.createTempFile( "testOriginal", ".gml" );
    final File testFileParsed = File.createTempFile( "testParsed", ".gml" );
    final File testFileParsed2 = File.createTempFile( "testParsed2", ".gml" );

    OutputStreamWriter writerOrg = null;
    OutputStreamWriter writerParsed = null;
    OutputStreamWriter writerParsed2 = null;

    try
    {
      // Remark: why write it into a separate file? Load it directly for comparison
      // via URL.open();
      writerOrg = new OutputStreamWriter( new FileOutputStream( testFileOrg ), "UTF-8" );
      final Document documentOrg = XMLTools.parse( resourceURL );
      XMLHelper.writeDOM( documentOrg, "UTF-8", writerOrg );
      writerOrg.close();

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( resourceURL );
      writerParsed = new OutputStreamWriter( new FileOutputStream( testFileParsed ), "UTF-8" );
      GmlSerializer.serializeWorkspace( writerParsed, workspace, "UTF-8" );
      writerParsed.close();

      final GMLWorkspace workspace2 = GmlSerializer.createGMLWorkspace( testFileParsed.toURL() );
      writerParsed2 = new OutputStreamWriter( new FileOutputStream( testFileParsed2 ), "UTF-8" );
      GmlSerializer.serializeWorkspace( writerParsed2, workspace2, "UTF-8" );
      writerParsed.close();

      assertTrue( StreamUtilities.isEqual( new FileInputStream( testFileParsed2 ), new FileInputStream( testFileParsed ) ) );
    }
    finally
    {
      IOUtils.closeQuietly( writerOrg );
      IOUtils.closeQuietly( writerParsed );
      IOUtils.closeQuietly( writerParsed2 );

      testFileOrg.delete();
      testFileParsed.delete();
      testFileParsed2.delete();
    }
  }

}
