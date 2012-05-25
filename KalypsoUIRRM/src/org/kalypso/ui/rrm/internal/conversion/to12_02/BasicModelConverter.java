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
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.module.conversion.AbstractLoggingOperation;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class BasicModelConverter extends AbstractLoggingOperation
{
  private final File m_sourceDir;

  private final File m_targetDir;

  private final ConverterData m_data;

  private TimeseriesIndex m_timeseriesIndex;

  public BasicModelConverter( final File sourceDir, final File targetDir )
  {
    super( Messages.getString( "BasicModelConverter_1" ) ); //$NON-NLS-1$

    m_sourceDir = sourceDir;
    m_targetDir = targetDir;

    final File basisDir = new File( m_targetDir, INaProjectConstants.FOLDER_BASIS );
    m_data = new ConverterData( basisDir );
  }

  @Override
  protected void doExecute( final IProgressMonitor monitor ) throws Exception
  {
    monitor.beginTask( Messages.getString( "BasicModelConverter.0" ), 100 ); //$NON-NLS-1$

    try
    {
      /* Copy basic .gml files */
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

      /* Copy timeseries */
      monitor.subTask( Messages.getString( "BasicModelConverter.3" ) ); //$NON-NLS-1$
      m_timeseriesIndex = copyBasicTimeseries( parameterIndex, new SubProgressMonitor( monitor, 90 ) );

      /* timeseries links */
      monitor.subTask( Messages.getString( "BasicModelConverter.4" ) ); //$NON-NLS-1$

      copyObservationConf( m_timeseriesIndex );

      monitor.worked( 5 );
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

    /* Copy known folders */
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
   * Copy observationConf.
   */
  private void copyObservationConf( final TimeseriesIndex timeseriesIndex )
  {
    final ObservationconfConverter converter = new ObservationconfConverter( timeseriesIndex, m_sourceDir, m_targetDir );

    getLog().add( converter.execute( "ObsQZuMapping.gml" ) ); //$NON-NLS-1$
    getLog().add( converter.execute( "ObsQMapping.gml" ) ); //$NON-NLS-1$
    // FIXME: remove everything from the mapping except sea evaporation timeseries
    getLog().add( converter.execute( "ObsEMapping.gml" ) ); //$NON-NLS-1$
  }

  private void copyFile( final IPath sourcePath, final IPath targetPath ) throws IOException
  {
    final File modelSourceFile = new File( m_sourceDir, sourcePath.toOSString() );
    final File modelTargetFile = new File( m_targetDir, targetPath.toOSString() );

    FileUtils.copyFile( modelSourceFile, modelTargetFile, true );
  }

  private IParameterTypeIndex fixTimeseries( ) throws Exception
  {
    final NaModell naModel = m_data.loadNaModel();

    // IMPORTANT: index parameter types before the links have been fixed, so file pathes are correct
    final IParameterTypeIndex parameterIndex = CalcCaseConverter.collectTimeseriesParameterTypes( naModel, m_sourceDir );
    CalcCaseConverter.fixTimeseriesLinks( naModel, getLog() );

    naModel.getNodes().accept( new UpdateResultCategoriesVisitor() );

    m_data.saveModel( naModel, INaProjectConstants.GML_MODELL_PATH );

    return parameterIndex;
  }

  public TimeseriesIndex getTimeseriesIndex( )
  {
    return m_timeseriesIndex;
  }
}