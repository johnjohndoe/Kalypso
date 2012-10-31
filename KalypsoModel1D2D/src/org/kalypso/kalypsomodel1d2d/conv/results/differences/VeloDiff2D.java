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

import org.apache.commons.vfs2.FileObject;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.vfs.FileSystemManagerWrapper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.conv.DifferenceResultModel1d2dHandler;
import org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.sim.ProcessResult2DOperation;
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
public class VeloDiff2D
{
  /**
   * @param args
   */
  public static void main( final String[] args )
  {
    final File resultFile1 = new File( args[0] );
    final File resultFile2 = new File( args[1] );
    final File templateFile = new File( args[2] );
    final File outputFile = new File( args[3] );

    final File outputDir1 = FileUtilities.createNewTempDir( "output1" ); //$NON-NLS-1$
    final File outputDir2 = FileUtilities.createNewTempDir( "output2" ); //$NON-NLS-1$

    try
    {
      outputFile.createNewFile();

      /* check files */
      if( !resultFile1.exists() || !resultFile2.exists() || !templateFile.exists() )
      {
        System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.differences.VeloDiff2D.2" ) ); //$NON-NLS-1$
        return;
      }

      /* process results and generate result tins */
      final List<ResultType> parameters = new ArrayList<>();

      if( args[4].equals( "velo" ) ) //$NON-NLS-1$
      {
        parameters.add( ResultType.VELOCITY_X );
        parameters.add( ResultType.VELOCITY_Y );
      }
      else
      {
        System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.differences.VeloDiff2D.4" ) ); //$NON-NLS-1$
        return;
      }

      System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.differences.VeloDiff2D.0" ) ); //$NON-NLS-1$

      final FileSystemManagerWrapper manager = VFSUtilities.getNewManager();
      processResults( manager.toFileObject( resultFile1 ), manager.toFileObject( resultFile2 ), parameters, outputDir1, outputDir2 );
      manager.close();

      System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.differences.VeloDiff2D.6" ) ); //$NON-NLS-1$

      if( args.length > 5 )
      {

        if( args[5].equals( "project" ) ) //$NON-NLS-1$
        {
          final CharSequence subSequence = args[3].subSequence( 0, args[3].length() - 3 );
          final File outputFileParallel = new File( subSequence + "_parallel.2d" ); //$NON-NLS-1$
          final File outputFileOrthogonal = new File( subSequence + "_orthogonal.2d" ); //$NON-NLS-1$

          generateDifferences( parameters, outputDir1, outputDir2, templateFile, outputFile, ResultCalculatorType.TYPE.VECTOR_DIFFERENCE );
          System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.differences.VeloDiff2D.10" ) ); //$NON-NLS-1$

          System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.differences.VeloDiff2D.11" ) ); //$NON-NLS-1$
          generateDifferences( parameters, outputDir1, outputDir2, templateFile, outputFileParallel, ResultCalculatorType.TYPE.VECTOR_DIFFERENCE_PARALLEL );

          System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.differences.VeloDiff2D.12" ) ); //$NON-NLS-1$
          generateDifferences( parameters, outputDir1, outputDir2, templateFile, outputFileOrthogonal, ResultCalculatorType.TYPE.VECTOR_DIFFERENCE_ORTHOGONAL );

        }
      }
      else
      {
        generateDifferences( parameters, outputDir1, outputDir2, templateFile, outputFile, ResultCalculatorType.TYPE.VECTOR_DIFFERENCE );
      }

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      FileUtilities.deleteRecursive( outputDir1 );
      FileUtilities.deleteRecursive( outputDir2 );
    }
  }

  private static void generateDifferences( final List<ResultType> parameters, final File outputDir1, final File outputDir2, final File templateFile, final File outputFile, final org.kalypso.kalypsomodel1d2d.conv.results.differences.ResultCalculatorType.TYPE resultDifferenceType ) throws Exception
  {
    final InputStream is = new FileInputStream( templateFile );
    try
    {
      /* generate differences */
      final ResultType[] types = parameters.toArray( new ResultType[parameters.size()] );

      final GM_TriangulatedSurface[] minuendSurfaces = new GM_TriangulatedSurface[types.length];
      final GM_TriangulatedSurface[] subtrahentSurfaces = new GM_TriangulatedSurface[types.length];

      for( int i = 0; i < types.length; i++ )
      {
        minuendSurfaces[i] = getSurfaces( outputDir1, types[i] );
        subtrahentSurfaces[i] = getSurfaces( outputDir2, types[i] );
      }

      final RMA10S2GmlConv converter = new RMA10S2GmlConv( null );

      final IRMA10SModelElementHandler handler = new DifferenceResultModel1d2dHandler( outputFile, minuendSurfaces, subtrahentSurfaces, types, resultDifferenceType );
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

  private static void processResults( final FileObject result2dFile1, final FileObject result2dFile2, final List<ResultType> parameters, final File outputDir1, final File outputDir2 )
  {
    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.differences.VeloDiff2D.14" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    final ProcessResult2DOperation job1 = new ProcessResult2DOperation( result2dFile1, outputDir1, null, null, null, parameters, ResultManager.STEADY_DATE, null );
    job1.execute( new NullProgressMonitor() );

    KalypsoModel1D2DDebug.SIMULATIONRESULT.printf( "%s", Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.results.differences.VeloDiff2D.16" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    final ProcessResult2DOperation job2 = new ProcessResult2DOperation( result2dFile2, outputDir2, null, null, null, parameters, ResultManager.STEADY_DATE, null );
    job2.execute( new NullProgressMonitor() );
  }

  private static GM_TriangulatedSurface getSurfaces( final File outputDir, final ResultType resultType ) throws Exception
  {
    File gmlFile = null;

    final File tinFolder = new File( outputDir, "Tin" ); //$NON-NLS-1$
    if( resultType.equals( ResultType.VELOCITY_X ) )
      gmlFile = new File( tinFolder, "tin_VELOCITY_X.gml" ); //$NON-NLS-1$
    else
      gmlFile = new File( tinFolder, "tin_VELOCITY_Y.gml" ); //$NON-NLS-1$

    final URL url = gmlFile.toURI().toURL();

    // REMARK 1: loads the source tin directly into memory.... will bring performance problems...
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url, null );

    final Feature rootFeature = workspace.getRootFeature();
    final GM_TriangulatedSurface surface = (GM_TriangulatedSurface)rootFeature.getProperty( new QName( "http://www.tu-harburg.de/wb/kalypso/schemata/1d2dResults", "triangulatedSurfaceMember" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    return surface;
  }
}
