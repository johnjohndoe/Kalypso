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
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.viewers.ILabelProvider;
import org.kalypso.KalypsoTest;
import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.gmlschema.basics.GMLSchemaLabelProvider;
import org.kalypso.gmlschema.basics.GMLSchemaTreeContentProvider;
import org.kalypso.gmlschema.basics.ITreeContentProviderVisitor;
import org.kalypso.test.TestUtilities;

/**
 * @author doemming
 */
public class GMLSchemaTest extends TestCase
{
  public static final String NS_GML2 = "http://www.opengis.net/gml";

  /*
   * @see TestCase#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    KalypsoTest.init();
    final Map<String, URL> map;
    map = new HashMap<String, URL>();
    map.put( "http://www.xplanung.de/bplangml", getClass().getResource( "resources/xplanung/BPlanGML_2.xsd" ) );
    map.put( "http://www.tuhh.de/kalypsoNA", getClass().getResource( "resources/namodell.xsd" ) );
    map.put( "http://www.tuhh.de/kalypsoNA", getClass().getResource( "resources/namodell2.xsd" ) );
    map.put( "http://www.w3.org/1999/xlink", getClass().getResource( "resources/xlinks.xsd" ) );
    map.put( NS_GML2, getClass().getResource( "resources/feature.xsd" ) );

    GMLSchemaCatalog.init( new IUrlCatalog()
    {
      public Map<String, URL> getCatalog( )
      {
        return map;
      }

      public URL getURL( String namespace )
      {
        return map.get( namespace );
      }

    }, new File( "C:\\TMP" ) );
  }

  public void testSchemas( ) throws Exception
  {
    try
    {
      loadSchema( // 
          getClass().getResource( "resources/namodell.xsd" ),// schemalocationURL
          "http://www.tuhh.de/kalypsoNA",// namespace
          "resources/test_rrm.txt" // testresource to compare
      );

      loadSchema( // 
          getClass().getResource( "resources/xplanung/BPlanGML_2.xsd" ),// schemalocationURL
          "http://www.xplanung.de/bplangml",// namespace
          "resources/xplanung/test_planGML2.txt" // testresource to compare
      );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }

  public void loadSchema( URL schemaLocationURL, String namespace, String testResource ) throws Exception
  {
    final GMLSchema schema = GMLSchemaFactory.createGMLSchema( namespace, schemaLocationURL );

    if( schema != null )
    {
      final StringBuffer buffer = new StringBuffer();
      final ITreeContentProviderVisitor visitor = new PrintVisitor( new GMLSchemaLabelProvider(), buffer );
      final GMLSchemaTreeContentProvider provider = new GMLSchemaTreeContentProvider( schema, true );
      provider.accept( schema, visitor, 0 );
       System.out.println( buffer.toString() );
      TestUtilities.compare( "gmlschemaparser", getClass().getResource( testResource ), buffer.toString() );
    }
  }

  private class PrintVisitor implements ITreeContentProviderVisitor
  {
    private final ILabelProvider m_labelProvider;

    private final StringBuffer m_buffer;

    public PrintVisitor( final ILabelProvider labelProvider, StringBuffer buffer )
    {
      m_labelProvider = labelProvider;
      m_buffer = buffer;
    }

    public boolean visit( Object element, int indent )
    {
      final String space = StringUtils.repeat( ".  ", indent );
      final String text = m_labelProvider.getText( element );
      final String[] lines = text.split( "\n" );
      for( int i = 0; i < lines.length; i++ )
      {
        m_buffer.append( space );
        m_buffer.append( lines[i] );
        m_buffer.append( "\n" );
      }
      return true;
    }
  }
}
