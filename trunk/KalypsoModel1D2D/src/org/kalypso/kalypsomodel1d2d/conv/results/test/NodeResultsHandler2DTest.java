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
package org.kalypso.kalypsomodel1d2d.conv.results.test;

import java.io.File;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.vfs2.FileObject;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.junit.Test;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.vfs.FileSystemManagerWrapper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.sim.ProcessResult2DOperation;
import org.kalypso.kalypsomodel1d2d.sim.ResultManager;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.shape.IShapeData;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.ShapeWriter;
import org.kalypso.shape.deegree.GM_Object2Shape;
import org.kalypso.shape.deegree.TriangulatedSurfaceSinglePartShapeDataProvider;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

/**
 * @author Thomas Jung
 */
public class NodeResultsHandler2DTest
{
  @Test
  public void testLoadResults( ) throws Exception
  {
    final File tempDir = FileUtilities.createNewTempDir( "result2dtest" ); //$NON-NLS-1$
    tempDir.mkdirs();

    final FileSystemManagerWrapper manager = VFSUtilities.getNewManager();
    try
    {
      final ILog log = KalypsoModel1D2DPlugin.getDefault().getLog();
      log.log( new Status( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "Start Result Processing Test (2D only)" ) ); //$NON-NLS-1$

      final URL zipLocation = getClass().getResource( "/etc/testdata/results/original.2d.zip" ); //$NON-NLS-1$
      ZipUtilities.unzip( zipLocation, tempDir );

      // get 2d-file from resources
      final File result2dFile = new File( tempDir, "A0001.2d" ); //$NON-NLS-1$
      final FileObject resultFileObject = manager.toFileObject( result2dFile );
      final File outputDir = new File( tempDir, "output" ); //$NON-NLS-1$
      outputDir.mkdir();

      log.log( new Status( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, "calling ProcessResultsJob" ) ); //$NON-NLS-1$
      final List<ResultType> parameters = new ArrayList<>();
      parameters.add( ResultType.DEPTH );
      parameters.add( ResultType.WATERLEVEL );

      final ProcessResult2DOperation job = new ProcessResult2DOperation( resultFileObject, outputDir, null, null, null, parameters, ResultManager.STEADY_DATE, null );
      final IStatus result = job.execute( new NullProgressMonitor() );
      log.log( result );

      if( result.isOK() )
        writeShape( outputDir );
    }
    finally
    {
      FileUtils.forceDelete( tempDir );

      manager.close();
      System.gc();
      System.gc();
      System.gc();
      System.out.println( "Total memory" + Runtime.getRuntime().totalMemory() ); //$NON-NLS-1$
    }
  }

  private void writeShape( final File outputDir ) throws Exception
  {
    final File tinFile = new File( outputDir, "Tin\\tin_WATERLEVEL.gz" ); //$NON-NLS-1$

    final File shapeFile = new File( outputDir, "Waterlevel" ); //$NON-NLS-1$

    final GMLWorkspace tinWorkspace = GmlSerializer.createGMLWorkspace( tinFile, null );
    final Feature rootFeature = tinWorkspace.getRootFeature();

    final Feature[] featureArray = new Feature[] { rootFeature };

    final ShapeType shapeType = ShapeType.POLYGONZ;
    final GMLXPath geometry = new GMLXPath( "triangulatedSurfaceMember", null ); //$NON-NLS-1$
    final Charset charset = ShapeSerializer.getShapeDefaultCharset();
    final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final GM_Object2Shape shapeConverter = new GM_Object2Shape( shapeType, coordinateSystem );
    final IShapeData dataProvider = new TriangulatedSurfaceSinglePartShapeDataProvider( featureArray, geometry, charset, shapeConverter );

    final ShapeWriter shapeWriter = new ShapeWriter( dataProvider );
    shapeWriter.write( shapeFile.getAbsolutePath(), new NullProgressMonitor() );
  }
}
