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
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.module.conversion.AbstractLoggingOperation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.conversion.TimeseriesWalker;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * @author Gernot Belger
 * @author Holger Albert
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
    m_data = new ConverterData( new File( m_targetDir, RrmProject.FOLDER_BASIS ) );
    m_timeseriesIndex = null;
  }

  @Override
  protected void doExecute( final IProgressMonitor monitor ) throws Exception
  {
    /* Monitor. */
    monitor.beginTask( Messages.getString( "BasicModelConverter.0" ), 100 ); //$NON-NLS-1$

    try
    {
      /* Monitor. */
      monitor.subTask( Messages.getString( "BasicModelConverter.1" ) ); //$NON-NLS-1$

      /* Copy the basic gml files. */
      copyBasicFiles();

      /* Monitor. */
      monitor.worked( 5 );

      /* Monitor. */
      monitor.subTask( Messages.getString( "BasicModelConverter.3" ) ); //$NON-NLS-1$

      /* Fix the timeseries. */
      final IParameterTypeIndex parameterIndex = fixTimeseries();

      /* Copy timeseries. */
      m_timeseriesIndex = copyBasicTimeseries( parameterIndex, new SubProgressMonitor( monitor, 95 ) );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  private void copyBasicFiles( ) throws IOException
  {
    final IPath basisPath = new Path( RrmProject.FOLDER_BASIS );

    final IPath modelsPath = basisPath.append( RrmScenario.FOLDER_MODELS );

    copyFile( new Path( INaProjectConstants.GML_MODELL_FILE ), modelsPath.append( RrmScenario.FILE_MODELL_GML ) );
    final File hydrotope = copyFile( new Path( INaProjectConstants.GML_HYDROTOP_FILE ), modelsPath.append( RrmScenario.FILE_HYDROTOP_GML ) );
    copyFile( new Path( INaProjectConstants.GML_PARAMETER_FILE ), modelsPath.append( RrmScenario.FILE_PARAMETER_GML ) );
    copyFile( new Path( "calcSynthN.gml" ), modelsPath.append( RrmScenario.FILE_SYNTHN_GML ) ); //$NON-NLS-1$
    final File landuse = copyFile( new Path( INaProjectConstants.GML_LANDUSE_FILE ), modelsPath.append( RrmScenario.FILE_LANDUSE ) );
    final File geology = copyFile( new Path( INaProjectConstants.GML_GEOLOGIE_FILE ), modelsPath.append( RrmScenario.FILE_GEOLOGIE ) );
    copyFile( new Path( INaProjectConstants.GML_PEDOLOGIE_FILE ), modelsPath.append( RrmScenario.FILE_PEDOLOGIE ) );

    final IStatus convertHydrotopesStatus = new ConvertHydrotopesOperation( hydrotope ).execute( new NullProgressMonitor() );
    getLog().add( convertHydrotopesStatus );

    final IStatus convertLanduseStatus = new ConvertLanduseOperation( landuse ).execute( new NullProgressMonitor() );
    getLog().add( convertLanduseStatus );

    final IStatus convertGeologyStatus = new ConvertGeologyOperation( geology ).execute( new NullProgressMonitor() );
    getLog().add( convertGeologyStatus );
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

  /**
   * @return model target file
   */
  private File copyFile( final IPath sourcePath, final IPath targetPath ) throws IOException
  {
    final File modelSourceFile = new File( m_sourceDir, sourcePath.toOSString() );
    final File modelTargetFile = new File( m_targetDir, targetPath.toOSString() );

    FileUtils.copyFile( modelSourceFile, modelTargetFile, true );

    return modelTargetFile;
  }

  private IParameterTypeIndex fixTimeseries( ) throws Exception
  {
    /* Load the na model. */
    final String modelFilePath = RrmScenario.FOLDER_MODELS + '/' + RrmScenario.FILE_MODELL_GML;
    final NaModell naModel = m_data.loadModel( modelFilePath );

    /* IMPORTANT: Index parameter types before the links have been fixed, so file paths are correct. */
    final IParameterTypeIndex parameterIndex = collectTimeseriesParameterTypes( naModel, m_sourceDir, getLog() );

    /* IMPORTANT: Update the categories before the links have been fixed. */
    naModel.getNodes().accept( new UpdateResultCategoriesVisitor() );

    /* Empty the timeseries links. */
    emptyTimeseriesLinks( naModel, getLog() );

    /* Save the na model. */
    m_data.saveModel( modelFilePath, naModel );

    return parameterIndex;
  }

  private IParameterTypeIndex collectTimeseriesParameterTypes( final NaModell naModel, final File sourceModelDir, final IStatusCollector log )
  {
    final IStatusCollector localLog = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final TimeseriesWalker walker = new TimeseriesWalker( new ParameterTypeIndexVisitor( sourceModelDir ), localLog );
    naModel.getWorkspace().accept( walker, naModel, FeatureVisitor.DEPTH_INFINITE );

    final IStatus status = localLog.asMultiStatus( Messages.getString( "BasicModelConverter.2" ) ); //$NON-NLS-1$
    log.add( status );

    return new ParameterTypeIndexVisitor( sourceModelDir );
  }

  public static void emptyTimeseriesLinks( final NaModell naModel, final IStatusCollector log ) throws Exception
  {
    final IStatusCollector localLog = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final TimeseriesWalker walker = new TimeseriesWalker( new EmptyTimeseriesVisitor(), localLog );
    naModel.getWorkspace().accept( walker, naModel, FeatureVisitor.DEPTH_INFINITE );

    final IStatus status = localLog.asMultiStatus( Messages.getString( "BasicModelConverter.9" ) ); //$NON-NLS-1$
    log.add( status );
  }

  public TimeseriesIndex getTimeseriesIndex( )
  {
    return m_timeseriesIndex;
  }
}