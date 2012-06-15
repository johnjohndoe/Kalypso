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
package org.kalypso.kalypsomodel1d2d.conv.results.differences;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemManagerWrapper;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.conv.DifferenceResultModel1d2dHandler;
import org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType.TYPE;
import org.kalypso.kalypsomodel1d2d.conv.results.test.NodeResultsHandler2DTest;
import org.kalypso.kalypsomodel1d2d.sim.ProcessResultsJob;
import org.kalypso.kalypsomodel1d2d.sim.ResultManager;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;

/**
 * This class processes the differences of the velocity vectors between two rma2 result files (.2d). At first the
 * surfaces of the calculated x- and y-velocities at the mesh nodes are being created. By defining a template 2d file
 * (must be also a result file!), the differences are being computed at the position of the nodes of the template file.
 * In the end the user gets a rma2/10 result file with the differences of vx and vy at the nodes. This file can then be
 * normally processed in order to show the difference vectors.<br>
 * <br>
 * necessary arguments are:<br>
 * <br>
 * result file 1, result file 2, template file, output file, parameter<br>
 * 
 * @author Thomas Jung
 */
public class VeloDiff2DTest extends TestCase
{
  private static void generateDifferences( final List<TYPE> parameters, final File outputDir1, final File outputDir2, final File templateFile, final File outputFile, final org.kalypso.kalypsomodel1d2d.conv.results.differences.ResultCalculatorType.TYPE differenceType ) throws Exception
  {
    final InputStream is = new FileInputStream( templateFile );
    try
    {
      /* generate differences */
      final TYPE[] resultTypes = parameters.toArray( new TYPE[parameters.size()] );

      final GM_TriangulatedSurface[] minuendSurfaces = new GM_TriangulatedSurface[resultTypes.length];
      final GM_TriangulatedSurface[] subtrahentSurfaces = new GM_TriangulatedSurface[resultTypes.length];

      for( int i = 0; i < resultTypes.length; i++ )
      {
        minuendSurfaces[i] = getSurfaces( outputDir1, resultTypes[i] );
        subtrahentSurfaces[i] = getSurfaces( outputDir2, resultTypes[i] );
      }

      final RMA10S2GmlConv converter = new RMA10S2GmlConv();
      IRMA10SModelElementHandler handler;

      handler = new DifferenceResultModel1d2dHandler( outputFile, minuendSurfaces, subtrahentSurfaces, resultTypes, differenceType );

      converter.setRMA10SModelElementHandler( handler );

      converter.parse( is );
      handler.end();
      is.close();
    }
    catch( final RuntimeException e )
    {
      e.printStackTrace();
    }
    finally
    {
      is.close();
    }
  }

  private static void processResults( final FileObject result2dFile1, final FileObject result2dFile2, final List<TYPE> parameters, final File outputDir1, final File outputDir2 )
  {
    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "calling ProcessResultsJob\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    final ProcessResultsJob job1 = new ProcessResultsJob( result2dFile1, outputDir1, null, null, null, parameters, ResultManager.STEADY_DATE, null );
    job1.run( new NullProgressMonitor() );

    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "calling ProcessResultsJob\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    final ProcessResultsJob job2 = new ProcessResultsJob( result2dFile2, outputDir2, null, null, null, parameters, ResultManager.STEADY_DATE, null );
    job2.run( new NullProgressMonitor() );
  }

  public void testLoadResults( ) throws Exception
  {
    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "Start Result Processing Test (2D only)\n" ); //$NON-NLS-1$ //$NON-NLS-2$

    // unzip test project into workspace
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IProject project = workspace.getRoot().getProject( "veloDiffTest" ); //$NON-NLS-1$
    project.create( new NullProgressMonitor() );

    final URL zipLocation = NodeResultsHandler2DTest.class.getResource( "resources/ergs.zip" ); //$NON-NLS-1$
    ZipUtilities.unzip( zipLocation, project, new NullProgressMonitor() );

    // run model
    final IFolder folder = project.getFolder( "ergs" ); //$NON-NLS-1$
    final IFile resultFile1 = folder.getFile( "erg1.2d" ); //$NON-NLS-1$
    final IFile resultFile2 = folder.getFile( "erg2.2d" ); //$NON-NLS-1$
    final IFile templateFile = folder.getFile( "template.2d" ); //$NON-NLS-1$

    final File outputDir1 = FileUtilities.createNewTempDir( "output1" ); //$NON-NLS-1$
    final File outputDir2 = FileUtilities.createNewTempDir( "output2" ); //$NON-NLS-1$

    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", "calling ProcessResultsJob\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    final List<TYPE> parameters = new ArrayList<TYPE>();

    parameters.add( TYPE.VELOCITY_X );
    parameters.add( TYPE.VELOCITY_Y );

    // ICalcUnitResultMeta resultMeta = new CalcUnitResultMeta(null);

    System.out.println( "processing input result files..." ); //$NON-NLS-1$
    File file1 = resultFile1.getLocation().toFile();
    File file2 = resultFile2.getLocation().toFile();

    file1 = new File( "P:\\emu0528409\\modell\\bce_2d\\calc_plan\\ohne_Hafen\\mhw_hq100\\mhw_hq100_plan_p16.2d" ); //$NON-NLS-1$
    file2 = new File( "P:\\emu0528409\\modell\\bce_2d\\calc_ref\\mhw_hq100\\mhw_ref_g18.2d" ); //$NON-NLS-1$

    final FileSystemManagerWrapper manager = VFSUtilities.getNewManager();
    processResults( manager.toFileObject( file1 ), manager.toFileObject( file2 ), parameters, outputDir1, outputDir2 );
    manager.close();

    final File outputFile = new File( "P:\\emu0528409\\modell\\bce_2d\\Auswertung_Querstroemung\\oH-Ref\\oH-Ref_mhw_hq100_quer.2d" ); //$NON-NLS-1$
    final File template = file2;
    // File template = templateFile.getLocation().toFile();

    final boolean parallel = true;
    if( parallel == true )
    {
      final File outputFileParallel = new File( outputDir2, "output_para.2d" ); //$NON-NLS-1$
      final File outputFileOrthogonal = new File( outputDir2, "output_ortho.2d" ); //$NON-NLS-1$

      generateDifferences( parameters, outputDir1, outputDir2, template, outputFileParallel, ResultCalculatorType.TYPE.VECTOR_DIFFERENCE_PARALLEL );
      generateDifferences( parameters, outputDir1, outputDir2, template, outputFileOrthogonal, ResultCalculatorType.TYPE.VECTOR_DIFFERENCE_ORTHOGONAL );
    }
    else
    {
      generateDifferences( parameters, outputDir1, outputDir2, template, outputFile, ResultCalculatorType.TYPE.VECTOR_DIFFERENCE );
    }
  }

  private static GM_TriangulatedSurface getSurfaces( final File outputDir, final TYPE resultType ) throws Exception
  {
    File gmlFile = null;

    final File tinFolder = new File( outputDir, "Tin" ); //$NON-NLS-1$
    if( resultType.equals( TYPE.VELOCITY_X ) )
      gmlFile = new File( tinFolder, "tin_VELOCITY_X.gml" ); //$NON-NLS-1$
    else
      gmlFile = new File( tinFolder, "tin_VELOCITY_Y.gml" ); //$NON-NLS-1$

    final URL url = gmlFile.toURL();

    // REMARK 1: loads the source tin directly into memory.... will bring performance problems...
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url, null );

    final Feature rootFeature = workspace.getRootFeature();
    final GM_TriangulatedSurface surface = (GM_TriangulatedSurface) rootFeature.getProperty( new QName( "http://www.tu-harburg.de/wb/kalypso/schemata/1d2dResults", "triangulatedSurfaceMember" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    return surface;
  }
}
