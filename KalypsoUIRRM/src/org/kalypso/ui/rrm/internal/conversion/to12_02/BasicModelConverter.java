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
    try
    {
      final IPath basisPath = new Path( INaProjectConstants.FOLDER_BASIS );

      /* Copy basic .gml files */
      copyFile( new Path( INaProjectConstants.GML_MODELL_FILE ), basisPath.append( INaProjectConstants.GML_MODELL_PATH ) );
      copyFile( new Path( INaProjectConstants.GML_HYDROTOP_FILE ), basisPath.append( INaProjectConstants.GML_HYDROTOP_PATH ) );
      copyFile( new Path( INaProjectConstants.GML_PARAMETER_FILE ), basisPath.append( INaProjectConstants.GML_PARAMETER_PATH ) );
      copyFile( new Path( "calcSynthN.gml" ), basisPath.append( INaProjectConstants.GML_SYNTH_N_PATH ) );
      copyFile( new Path( INaProjectConstants.GML_LANDUSE_FILE ), basisPath.append( INaProjectConstants.GML_LANDUSE_PATH ) );
      copyFile( new Path( INaProjectConstants.GML_GEOLOGIE_FILE ), basisPath.append( INaProjectConstants.GML_GEOLOGIE_PATH ) );
      copyFile( new Path( INaProjectConstants.GML_PEDOLOGIE_FILE ), basisPath.append( INaProjectConstants.GML_PEDOLOGIE_PATH ) );

      final TimeseriesImporter importer = copyBasicTimeseries();

      copyObservationConf();

      fixTimeseries();
    }
    finally
    {
      monitor.done();
    }
  }

  private TimeseriesImporter copyBasicTimeseries( ) throws CoreException
  {
    final TimeseriesImporter importer = new TimeseriesImporter( m_sourceDir, m_targetDir, getLog() );
    importer.readStations();

    /* Copy known folders */
    importer.copyTimeseries( "Klima" ); //$NON-NLS-1$
    importer.copyTimeseries( "Climate" ); //$NON-NLS-1$

    importer.copyTimeseries( "Ombrometer" ); //$NON-NLS-1$

    importer.copyTimeseries( "Pegel" ); //$NON-NLS-1$
    importer.copyTimeseries( "Gauge" ); //$NON-NLS-1$

    importer.copyTimeseries( "Zufluss" ); //$NON-NLS-1$
    importer.copyTimeseries( "Tributary" ); //$NON-NLS-1$

    importer.saveStations();

    return importer;
  }

  /**
   * Copy observationConf.
   */
  private void copyObservationConf( ) throws IOException
  {
    /* copy observationConfig */
    final File sourceDir = new File( m_sourceDir, INaProjectConstants.FOLDER_OBSERVATION_CONF );
    final File targetDir = new File( m_targetDir, INaProjectConstants.FOLDER_OBSERVATION_CONF );
    FileUtils.copyDirectory( sourceDir, targetDir, true );
  }

  private void copyFile( final IPath sourcePath, final IPath targetPath ) throws IOException
  {
    final File modelSourceFile = new File( m_sourceDir, sourcePath.toOSString() );
    final File modelTargetFile = new File( m_targetDir, targetPath.toOSString() );

    FileUtils.copyFile( modelSourceFile, modelTargetFile, true );
  }

  private void fixTimeseries( ) throws Exception
  {
    final NaModell naModel = m_data.loadNaModel();

    final IStatus log = CalcCaseConverter.fixTimeseriesLinks( naModel );
    getLog().add( log );

    m_data.saveModel( naModel, INaProjectConstants.GML_MODELL_PATH );
    getLog().add( IStatus.INFO, "Timeseries links have been updated." );
  }
}