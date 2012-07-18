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
package org.kalypso.ui.rrm.internal.timeseries.operations;

import java.io.File;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.timeseries.HydrologyTimeseriesImportWorker;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.util.FindTimeStepOperation;
import org.kalypso.ogc.sensor.util.FindTimestampOperation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.IImportTimeseriesOperation;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.ImportObservationWorker;
import org.kalypso.zml.ui.imports.ImportObservationData;

/**
 * @author Gernot Belger
 * @author Dirk Kuch
 */
public class ImportTimeseriesOperation implements ICoreRunnableWithProgress, IImportTimeseriesOperation
{
  private final ImportObservationData m_data;

  private IObservation m_observation;

  private Period m_timestep;

  private LocalTime m_timestamp;

  private final IImportTimeseriesOperationValidator m_validator;

  private DateRange m_daterange;

  private String m_quality;

  private String m_description;

  public ImportTimeseriesOperation( final ImportObservationData data, final IImportTimeseriesOperationValidator validator )
  {
    m_data = data;
    m_validator = validator;
    m_observation = null;
    m_timestep = null;
    m_timestamp = null;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final IStatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );
    final File fileSource = m_data.getSourceFileData().getFile();

    try
    {
      final ImportObservationWorker opObservationImport = new ImportObservationWorker( m_data, fileSource );
      doExecute( opObservationImport, stati, monitor, Messages.getString( "ImportTimeseriesOperation_0" ) );//$NON-NLS-1$
      m_observation = opObservationImport.getObservation();

      /* Timestep. */
      final FindTimeStepOperation opTimeStep = new FindTimeStepOperation( m_observation );
      doExecute( opTimeStep, stati, monitor, Messages.getString( "ImportTimeseriesOperation_2" ) ); //$NON-NLS-1$

      /* Set the timestep. */
      m_timestep = opTimeStep.getTimestep();
      m_daterange = opTimeStep.getDateRange();

      /* Set the timestamp. */
      final FindTimestampOperation opTimestamp = new FindTimestampOperation( m_observation, m_timestep );
      doExecute( opTimestamp, stati, monitor, Messages.getString( "ImportTimeseriesOperation_3" ) );//$NON-NLS-1$
      m_timestamp = opTimestamp.getTimestamp();

      updateMetadata( m_observation );

      if( m_validator != null ) // only in case of time series import
      {
        m_validator.setTimestep( m_timestep );
        doExecute( m_validator, stati, monitor, Messages.getString( "ImportTimeseriesOperation_0" ) );//$NON-NLS-1$
      }

      /* Check timestamp of "Tageszeitreihen" */
      final ValidateTageszeitreihenOperation opTageszeitreihe = new ValidateTageszeitreihenOperation( m_observation, m_timestep, m_timestamp );
      doExecute( opTageszeitreihe, stati, monitor, Messages.getString( "ImportTimeseriesOperation.0" ) ); //$NON-NLS-1$

      /* Rücksprung in Daten?!? */
      final ValidateRuecksprungOperation opRuecksprung = new ValidateRuecksprungOperation( m_observation );
      doExecute( opRuecksprung, stati, monitor, Messages.getString( "ImportTimeseriesOperation.1" ) ); //$NON-NLS-1$
      m_observation = opRuecksprung.getObservation();

      /* Validate the timestep. */
      final ValidateMissingTimestepsOperation opMissingValues = new ValidateMissingTimestepsOperation( m_observation, m_timestep );
      doExecute( opMissingValues, stati, monitor, Messages.getString( "ImportTimeseriesOperation.2" ) ); //$NON-NLS-1$
      m_observation = opMissingValues.getObservation();

      final HydrologyTimeseriesImportWorker cleanupWorker = new HydrologyTimeseriesImportWorker( m_observation, m_daterange );
      m_observation = cleanupWorker.convert( m_timestep, m_timestamp );
    }
    catch( final CancelProcessingException e )
    {
      return stati.asMultiStatus( e.getMessage() );
    }

    return stati.asMultiStatus( Messages.getString( "ImportTimeseriesOperation_5" ) ); //$NON-NLS-1$
  }

  private void doExecute( final ICoreRunnableWithProgress runnable, final IStatusCollector stati, final IProgressMonitor monitor, final String errorMessage ) throws CancelProcessingException
  {
    try
    {
      final IStatus status = runnable.execute( monitor );
      stati.add( status );

      if( IStatus.ERROR == status.getSeverity() )
        throw new CancelProcessingException( errorMessage );
    }
    catch( final CancelProcessingException e )
    {
      throw e;
    }
    catch( final Exception ex )
    {
      // FIXME: really, really ugly: CoreException etc. catched like that: all information is lost!

      throw new CancelProcessingException( ex.getMessage() );
    }
  }

  private void updateMetadata( final IObservation observation )
  {
    /* Timestep */
    final MetadataList metadataList = observation.getMetadataList();
    MetadataHelper.setTimestep( metadataList, m_timestep );
    MetadataHelper.setTargetDateRange( metadataList, getDateRange() );

    if( m_timestamp != null )
      MetadataHelper.setTimestamp( metadataList, m_timestamp );
  }

  @Override
  public ImportObservationData getData( )
  {
    return m_data;
  }

  @Override
  public IObservation getObservation( )
  {
    return m_observation;
  }

  @Override
  public Period getTimestep( )
  {
    return m_timestep;
  }

  public LocalTime getTimestamp( )
  {
    return m_timestamp;
  }

  @Override
  public DateRange getDateRange( )
  {
    return m_daterange;
  }

  @Override
  public String getQuality( )
  {
    return m_quality;
  }

  public void setQuality( final String quality )
  {
    m_quality = quality;
  }

  @Override
  public String getDescription( )
  {
    return m_description;
  }

  public void setDescription( final String description )
  {
    m_description = description;
  }
}