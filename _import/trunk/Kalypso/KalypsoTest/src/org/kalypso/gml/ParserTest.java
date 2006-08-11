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
package org.kalypso.gml;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.KalypsoTest;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
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
    KalypsoTest.init();
  }

  /**
   * @see junit.framework.TestCase#tearDown()
   */
  @Override
  protected void tearDown( ) throws Exception
  {
    KalypsoTest.release();
  }

  public void ftestload( ) throws Exception
  {
    try
    {
      final URL resource = getClass().getResource( "resources/modell.gml" );
      // final URL resource = getClass().getResource( "resources/dictionary.gml" );
      // final URL resource = getClass().getResource( "resources/_timeseriesNoDict.gml" );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( resource );
      final File file = new File( "C:/TMP/newParserInAndNewOut.gml" );
      final OutputStreamWriter writer = new OutputStreamWriter( new FileOutputStream( file ), "UTF-8" );
      GmlSerializer.serializeWorkspace( writer, workspace );
      IOUtils.closeQuietly( writer );

      // secound pass
      final GMLWorkspace workspace2 = GmlSerializer.createGMLWorkspace( file.toURL() );
      final File file2 = new File( "C:/TMP/newParserInAndNewOut2.gml" );
      final OutputStreamWriter writer2 = new OutputStreamWriter( new FileOutputStream( file2 ), "UTF-8" );
      GmlSerializer.serializeWorkspace( writer2, workspace2 );
      IOUtils.closeQuietly( writer2 );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }

  // TODO: no possibe to work at the moment, because
  // it is not possible to load the om schema without running
  // workbench
  // TODO: change to plugin unit test
  public void testObsToFeature( ) throws Exception
  {
    // final URL resource = getClass().getResource( "resources/_timeseriesNoDict.gml" );
    //
    // final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( resource );
    //
    // final Feature f = workspace.getFeature( "wasserstandsmessung" );
    //
    // final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( f );
    //
    // ObservationFeatureFactory.toFeature( obs, f );
    //
    // OutputStreamWriter writer = null;
    // try
    // {
    // writer = new OutputStreamWriter( new FileOutputStream( new File( "C:/TMP/obsSerialized.gml" ) ), "ISO-8859-1" );
    // GmlSerializer.serializeWorkspace( writer, workspace );
    // }
    // finally
    // {
    // IOUtils.closeQuietly( writer );
    // }
  }
}
