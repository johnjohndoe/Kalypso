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
import java.io.FileWriter;
import java.net.URL;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.KalypsoTest;
import org.kalypso.contribs.java.io.StreamUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.w3c.dom.Document;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author doemming
 */
public class GMLSerializerTest extends TestCase
{

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    super.setUp();
    KalypsoTest.init();
  }

  /*
   * Class under test for GMLWorkspace createGMLWorkspace(URL)
   */
  public void testCreateGMLWorkspaceURL() throws Exception
  {
    final File testFileOrg = new File( "C:\\TMP\\testOriginal.gml" );
    final File testFileParsed = new File( "C:\\TMP\\testParsed.gml" );
    final File testFileParsed2 = new File( "C:\\TMP\\testParsed2.gml" );

    final FileWriter writerOrg = new FileWriter( testFileOrg );
    final URL resource = getClass().getResource( "resources/calcCase.gml" );
    final Document documentOrg = XMLTools.parse( resource );
    XMLHelper.writeDOM( documentOrg, "UTF-8", writerOrg );
    IOUtils.closeQuietly( writerOrg );

    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( resource );

    final FileWriter writerParsed = new FileWriter( testFileParsed );
    GmlSerializer.serializeWorkspace( writerParsed, workspace, "UTF-8" );
    IOUtils.closeQuietly( writerParsed );

    final GMLWorkspace workspace2 = GmlSerializer.createGMLWorkspace( testFileParsed.toURL());
    final FileWriter writerParsed2 = new FileWriter( testFileParsed2 );
    GmlSerializer.serializeWorkspace( writerParsed2, workspace2, "UTF-8" );
    IOUtils.closeQuietly( writerParsed2 );
    
    assertTrue( StreamUtilities.isEqual( new FileInputStream( testFileOrg ), new FileInputStream( testFileParsed ) ) );
  }

}
