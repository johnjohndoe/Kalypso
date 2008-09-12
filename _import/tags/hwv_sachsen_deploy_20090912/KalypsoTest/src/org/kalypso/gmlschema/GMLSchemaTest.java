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
package org.kalypso.gmlschema;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URL;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.gmlschema.ui.GMLSchemaLabelProvider;
import org.kalypso.gmlschema.ui.GMLSchemaTreeContentProvider;
import org.kalypso.gmlschema.ui.GmlTreePrintVisitor;
import org.kalypso.gmlschema.ui.ITreeContentProviderVisitor;
import org.kalypso.test.TestUtilities;
import org.kalypso.ui.KalypsoGisPlugin;


/**
 * this test parses GML application schemas and produces a ASCII-Tree-output. this output is compared by validated
 * treeoutputs in the resources. This Test is intended to fail on changes in schema parsing.
 * 
 * @author doemming
 */
public class GMLSchemaTest extends TestCase
{
  /**
   * @see TestCase#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    KalypsoGisPlugin.getDefault();
    // REMARK: KalypsoTest is not needed any more as we run the tests as plug-in test.
    // Instead we must start the KalypsoGisPlugin to init the schema catalog

    // KalypsoTest.init();
    // // final Map<String, URL> map;
    // // map = new HashMap<String, URL>();
    //
    // // map.put( NS_GML2, getClass().getResource( "resources/feature.xsd" ) );
    // final IUrlCatalog defaultCatalog = GMLSchemaCatalog.getDefaultCatalog();
    // final Map<String, URL> map = defaultCatalog.getCatalog();
    // // map.put( "http://www.tuhh.de/kalypsoNA", getClass().getResource( "resources/namodell2.xsd" ) );
    // map.put( "http://www.xplanung.de/bplangml", getClass().getResource( "resources/xplanung/BPlanGML_2.xsd" ) );
    //
    // final IUrlCatalog newURLCatalog = new IUrlCatalog()
    // {
    //
    // public Map<String, URL> getCatalog( )
    // {
    // return map;
    // }
    //
    // public URL getURL( String namespace )
    // {
    // return map.get( namespace );
    // }
    //
    // public String getPreferedNamespacePrefix( String namespace )
    // {
    // return null;
    // }
    //
    // };
    //
    // m_tmpFileCache = FileUtilities.createNewTempDir( "kalypsoSchemaCache" );
    // m_tmpFileCache.deleteOnExit();
    // GMLSchemaCatalog.init( newURLCatalog, m_tmpFileCache );
    // // {
    // // public Map<String, URL> getCatalog( )
    // // {
    // // return map;
    // // }
    // //
    // // public URL getURL( String namespace )
    // // {
    // // return map.get( namespace );
    // // }
    // //
    // // }, tmpFileCache );
    //
    // // m_listToTest
  }

  /**
   * @see junit.framework.TestCase#tearDown()
   */
  @Override
  protected void tearDown( ) throws Exception
  {
    // KalypsoTest.release();
    //
    // FileUtilities.deleteRecursive( m_tmpFileCache );
  }

  public void testBPlan( ) throws Exception
  {
    try
    {
      loadAndTestSchema( // 
      // getClass().getResource( "resources/xplanung/BPlanGML_2.xsd" ),// schemalocationURL
      getClass().getResource( "resources/xplanung/BPlan-Operationen_2.xsd" ),// schemalocationURL
      getClass().getResource( "resources/xplanung/test_planGML2.txt" ) // testresource to compare
      , false );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }

  public void testNA( ) throws Exception
  {
    try
    {
      loadAndTestSchema( // 
      getClass().getResource( "resources/namodell.xsd" ),// schemalocationURL
      getClass().getResource( "resources/test_rrm.txt" ) // testresource to compare
      , false );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }

  // TODO: wspm Schema still changing at the moment, please
  // comment in if stable
  // public void testWspm( ) throws Exception
  // {
  // try
  // {
  // loadAndTestSchema( //
  // getClass().getResource( "resources/GML3_wspm/wspm.xsd" ), //
  // getClass().getResource( "resources/GML3_wspm/schematree.txt" )//
  // , false );
  // }
  // catch( Exception e )
  // {
  // e.printStackTrace();
  // throw e;
  // }
  // }

  public static void loadAndTestSchema( final URL schemaLocationURL, final URL testResource, final boolean writeCompareFile ) throws Exception
  {
    System.out.println( " test " + schemaLocationURL.toString() );
    final IGMLSchema gmlSchema = GMLSchemaFactory.createGMLSchema( null, schemaLocationURL );

    if( gmlSchema != null )
    {
      final StringBuffer buffer = new StringBuffer();

      final ITreeContentProviderVisitor visitor = new GmlTreePrintVisitor( new GMLSchemaLabelProvider(), buffer );
      final GMLSchemaTreeContentProvider provider = new GMLSchemaTreeContentProvider( gmlSchema, true );
      provider.accept( gmlSchema, visitor, 0 );
      if( writeCompareFile )
      {
        Writer writer = null;
        try
        {
          // Cannot use resource uri because it is probably no file
          // final File file = new File( testResource.toURI() ) ;
          // REMARK: In this case we do not clean up this file, because the user needs it.
          // Also, he has set the writeCompareFile to true manually, so he probably
          // knows what he is doing
          final String fileName = FileUtilities.nameFromPath( testResource.getPath() );
          final File file = new File( FileUtilities.TMP_DIR, fileName );

          // use UTF-8 because TestUtilities allways uses UTF-8
          writer = new OutputStreamWriter( new FileOutputStream( file ), "UTF-8" );
          writer.write( buffer.toString() );
          writer.close();
          System.out.println( "Wrote schema to " + file.toString() + "\n next run you can compare" );
        }
        finally
        {
          IOUtils.closeQuietly( writer );
        }
      }
      else
      {
        // System.out.println( buffer.toString() );
        TestUtilities.compare( "gmlschemaparser", testResource, buffer.toString() );
      }
    }
  }
}
