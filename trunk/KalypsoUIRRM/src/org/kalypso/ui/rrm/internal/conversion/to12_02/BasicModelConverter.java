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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.module.conversion.AbstractLoggingOperation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.conversion.ITimeseriesVisitor;
import org.kalypso.ui.rrm.internal.conversion.TimeseriesWalker;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * @author Gernot Belger
 */
public class BasicModelConverter extends AbstractLoggingOperation
{
  /**
   * The directory of the project to be imported.
   */
  private final File m_sourceDir;

  /**
   * The directory of the new project.
   */
  private final File m_targetDir;

  /**
   * The global conversion data.
   */
  private final ConverterData m_data;

  /**
   * The timeseries index.
   */
  private TimeseriesIndex m_timeseriesIndex;

  public BasicModelConverter( final File sourceDir, final File targetDir )
  {
    super( Messages.getString( "BasicModelConverter_1" ) ); //$NON-NLS-1$

    m_sourceDir = sourceDir;
    m_targetDir = targetDir;
    m_data = new ConverterData( new File( m_targetDir, INaProjectConstants.FOLDER_BASIS ) );
    m_timeseriesIndex = null;
  }

  @Override
  protected void doExecute( final IProgressMonitor monitor ) throws Exception
  {
    monitor.beginTask( Messages.getString( "BasicModelConverter.0" ), 100 ); //$NON-NLS-1$

    try
    {
      /* Copy basic .gml files. */
      monitor.subTask( Messages.getString( "BasicModelConverter.1" ) ); //$NON-NLS-1$
      final IPath basisPath = new Path( INaProjectConstants.FOLDER_BASIS );

      copyFile( new Path( INaProjectConstants.GML_MODELL_FILE ), basisPath.append( INaProjectConstants.GML_MODELL_PATH ) );
      copyFile( new Path( INaProjectConstants.GML_HYDROTOP_FILE ), basisPath.append( INaProjectConstants.GML_HYDROTOP_PATH ) );
      copyFile( new Path( INaProjectConstants.GML_PARAMETER_FILE ), basisPath.append( INaProjectConstants.GML_PARAMETER_PATH ) );
      copyFile( new Path( "calcSynthN.gml" ), basisPath.append( INaProjectConstants.GML_SYNTH_N_PATH ) ); //$NON-NLS-1$
      copyFile( new Path( INaProjectConstants.GML_LANDUSE_FILE ), basisPath.append( INaProjectConstants.GML_LANDUSE_PATH ) );
      copyFile( new Path( INaProjectConstants.GML_GEOLOGIE_FILE ), basisPath.append( INaProjectConstants.GML_GEOLOGIE_PATH ) );
      copyFile( new Path( INaProjectConstants.GML_PEDOLOGIE_FILE ), basisPath.append( INaProjectConstants.GML_PEDOLOGIE_PATH ) );

      monitor.worked( 5 );

      final IParameterTypeIndex parameterIndex = fixTimeseries();

      /* Copy timeseries. */
      monitor.subTask( Messages.getString( "BasicModelConverter.3" ) ); //$NON-NLS-1$
      m_timeseriesIndex = copyBasicTimeseries( parameterIndex, new SubProgressMonitor( monitor, 95 ) );
    }
    finally
    {
      monitor.done();
    }
  }

  private TimeseriesIndex copyBasicTimeseries( final IParameterTypeIndex parameterIndex, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( Messages.getString( "BasicModelConverter.5" ), 100 ); //$NON-NLS-1$

    monitor.subTask( Messages.getString( "BasicModelConverter.6" ) ); //$NON-NLS-1$
    final TimeseriesImporter importer = new TimeseriesImporter( m_sourceDir, m_targetDir, getLog(), parameterIndex );
    importer.readStations();
    monitor.worked( 5 );

    /* Copy known folders. */
    copyTimeseries( importer, monitor );

    monitor.subTask( Messages.getString( "BasicModelConverter.7" ) ); //$NON-NLS-1$
    importer.saveStations();
    monitor.worked( 5 );

    return importer.getIndex();
  }

  private void copyTimeseries( final TimeseriesImporter importer, final IProgressMonitor monitor )
  {
    monitor.subTask( Messages.getString( "BasicModelConverter.8", INaProjectConstants.FOLDER_ZEITREIHEN ) ); //$NON-NLS-1$
    importer.copyTimeseries( new SubProgressMonitor( monitor, 90 / 7 ) ); //$NON-NLS-1$
  }

  private void copyFile( final IPath sourcePath, final IPath targetPath ) throws IOException
  {
    final File modelSourceFile = new File( m_sourceDir, sourcePath.toOSString() );
    final File modelTargetFile = new File( m_targetDir, targetPath.toOSString() );

    FileUtils.copyFile( modelSourceFile, modelTargetFile, true );
  }

  private IParameterTypeIndex fixTimeseries( ) throws Exception
  {
    final NaModell naModel = m_data.loadModel( INaProjectConstants.GML_MODELL_PATH );

    /* IMPORTANT: index parameter types before the links have been fixed, so file paths are correct. */
    final IParameterTypeIndex parameterIndex = collectTimeseriesParameterTypes( naModel, m_sourceDir );
    fixTimeseriesLinks( naModel, getLog() );

    naModel.getNodes().accept( new UpdateResultCategoriesVisitor() );

    m_data.saveModel( INaProjectConstants.GML_MODELL_PATH, naModel );

    return parameterIndex;
  }

  /**
   * Add an additional '../' to every timeseries path.
   */
  public static void fixTimeseriesLinks( final NaModell naModel, final IStatusCollector log ) throws Exception
  {
    final IStatusCollector localLog = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    visitModel( naModel, new FixDotDotTimeseriesVisitor(), localLog );
    naModel.getNodes().accept( new UpdateResultCategoriesVisitor() );

    final IStatus status = localLog.asMultiStatus( "Anpassen der Zeitreihenreferenzen" );
    log.add( status );
  }

  private static IParameterTypeIndex collectTimeseriesParameterTypes( final NaModell naModel, final File sourceModelDir )
  {
    final IStatusCollector localLog = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final ParameterTypeIndexVisitor visitor = new ParameterTypeIndexVisitor( sourceModelDir );
    visitModel( naModel, visitor, localLog );

    return visitor;
  }

  private static void visitModel( final NaModell naModel, final ITimeseriesVisitor visitor, final IStatusCollector log )
  {
    final TimeseriesWalker walker = new TimeseriesWalker( visitor, log );
    naModel.getWorkspace().accept( walker, naModel, FeatureVisitor.DEPTH_INFINITE );
  }

  public TimeseriesIndex getTimeseriesIndex( )
  {
    return m_timeseriesIndex;
  }
}