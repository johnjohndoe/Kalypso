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
package org.kalypso.gml;

import java.io.File;
import java.io.FileOutputStream;
import java.net.URL;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.KalypsoTest;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author doemming
 */
public class ParserTest extends TestCase
{
  /**
   * @see junit.framework.TestCase#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    super.setUp();
    KalypsoTest.init();

  }

  // public void testBoth( ) throws Exception
  // {
  // final Calendar start = Calendar.getInstance();
  // for( int i = 100; i > 0; i-- )
  // {
  // System.out.print( i + ". ..." );
  // loadOld();
  // }
  // final Calendar end = Calendar.getInstance();
  // System.out.println( end.getTimeInMillis() - start.getTimeInMillis() + " millis" );
  // }

  public void testloadOld( ) throws Exception
  {
    try
    {
      final URL resource = getClass().getResource( "resources/modell.gml" );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( resource );

      final GMLParser parser = new GMLParser();
      final FileOutputStream stream = new FileOutputStream( new File( "C:/TMP/oldParserInAndNewOut.xml" ) );
      parser.writeGML( workspace, stream );
      IOUtils.closeQuietly( stream );
      System.out.println( " loaded" );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }

  }

  public void testloadNew( ) throws Exception
  {
    try
    {
      final URL resource = getClass().getResource( "resources/modell.gml" );
      final GMLParser parser = new GMLParser();
      final GMLWorkspace workspace = parser.parseGML( resource );
      final File file = new File( "C:/TMP/newParserInAndNewOut.xml" );
      final FileOutputStream stream = new FileOutputStream( file );
      parser.writeGML( workspace, stream );
      IOUtils.closeQuietly( stream );
      
      final GMLWorkspace workspace2 = parser.parseGML( file.toURL() );
      final File file2 = new File( "C:/TMP/newOut_newIn_newOut.xml" );
      final FileOutputStream stream2 = new FileOutputStream( file2 );
      parser.writeGML( workspace2, stream2 );
      IOUtils.closeQuietly( stream2 );
      
      
      
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }
}
