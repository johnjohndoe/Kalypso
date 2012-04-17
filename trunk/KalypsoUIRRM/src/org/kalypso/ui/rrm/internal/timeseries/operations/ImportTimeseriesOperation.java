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
import java.text.SimpleDateFormat;
import java.util.TimeZone;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.hydrology.timeseries.TimeseriesImportWorker;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.util.FindTimeStepOperation;
import org.kalypso.ogc.sensor.util.FindTimestampOperation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.IImportTimeseriesOperation;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.ImportObservationWorker;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.ValidateRuecksprungVisitor;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.ValidateTimestepsVisitor;
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

  public ImportTimeseriesOperation( final ImportObservationData data )
  {
    m_data = data;
    m_observation = null;
    m_timestep = null;
    m_timestamp = null;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final IStatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );
    final File fileSource = m_data.getSourceFileData().getFile();

    final ImportObservationWorker observationWorker = new ImportObservationWorker( m_data, fileSource );
    final IStatus status = observationWorker.execute( monitor );
    stati.add( status );
    if( IStatus.ERROR == status.getSeverity() )
      return stati.asMultiStatus( Messages.getString( "ImportTimeseriesOperation_0" ) ); //$NON-NLS-1$

    final IObservation observation = observationWorker.getObservation();

    /* Rücksprung in Daten?!? */
    final IStatus ruecksprung = validateRuecksprung( observation );
    stati.add( ruecksprung );
    if( IStatus.ERROR == ruecksprung.getSeverity() )
      return stati.asMultiStatus( Messages.getString( "ImportTimeseriesOperation_1" ) ); //$NON-NLS-1$

    /* Timestep. */
    final FindTimeStepOperation timeStepOperation = new FindTimeStepOperation( observation );
    final IStatus timestepStatus = timeStepOperation.execute( monitor );
    stati.add( timestepStatus );
    if( IStatus.ERROR == timestepStatus.getSeverity() )
      return stati.asMultiStatus( Messages.getString( "ImportTimeseriesOperation_2" ) ); //$NON-NLS-1$

    /* Set the timestep. */
    m_timestep = timeStepOperation.getTimestep();

    /* Timestamp. */
    final FindTimestampOperation timestampOperation = new FindTimestampOperation( observation, m_timestep );
    final IStatus timestampStatus = timestampOperation.execute( monitor );
    stati.add( timestampStatus );
    if( IStatus.ERROR == timestampStatus.getSeverity() )
      return stati.asMultiStatus( Messages.getString( "ImportTimeseriesOperation_3" ) ); //$NON-NLS-1$    

    /* Set the timestamp. */
    m_timestamp = timestampOperation.getTimestamp();

    /* Validate the timestep. */
    final IStatus validTimestep = validateTimesteps( observation, m_timestep );
    stati.add( validTimestep );
    if( IStatus.ERROR == validTimestep.getSeverity() )
      return stati.asMultiStatus( Messages.getString( "ImportTimeseriesOperation_4" ) ); //$NON-NLS-1$

    final TimeseriesImportWorker cleanupWorker = new TimeseriesImportWorker( observation );
    m_observation = cleanupWorker.convert( m_timestep, m_timestamp );

    updateMetadata( m_observation );

    return stati.asMultiStatus( Messages.getString( "ImportTimeseriesOperation_5" ) ); //$NON-NLS-1$

  }

  private void updateMetadata( final IObservation observation )
  {
    /* Timestep */
    final MetadataList metadataList = observation.getMetadataList();
    MetadataHelper.setTimestep( metadataList, m_timestep );
  }

  private IStatus validateTimesteps( final IObservation observation, final Period timestep )
  {
    try
    {
      final ValidateTimestepsVisitor visitor = new ValidateTimestepsVisitor( timestep );
      observation.accept( visitor, null, 1 );

      return visitor.getStatus();
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, KalypsoCorePlugin.getID(), Messages.getString( "ImportTimeseriesOperation_6" ), e ); //$NON-NLS-1$
    }
  }

  private IStatus validateRuecksprung( final IObservation observation )
  {
    try
    {
      final ValidateRuecksprungVisitor ruecksprung = new ValidateRuecksprungVisitor();
      observation.accept( ruecksprung, null, 1 );

      if( ruecksprung.hasRuecksprung() )
      {

        final SimpleDateFormat sdf = new SimpleDateFormat( Messages.getString( "dd.MM.yyyy HH:mm:ss" ) );
        final TimeZone timezone = MetadataHelper.getTimeZone( observation.getMetadataList(), KalypsoCorePlugin.getDefault().getTimeZone().getID() );
        sdf.setTimeZone( timezone );

        return new Status( IStatus.ERROR, KalypsoCorePlugin.getID(), Messages.getString( "ImportTimeseriesOperation_7", sdf.format( ruecksprung.getLastDate() ) ) ); //$NON-NLS-1$
      }

      return new Status( IStatus.OK, KalypsoCorePlugin.getID(), String.format( Messages.getString( "ImportTimeseriesOperation_8" ) ) ); //$NON-NLS-1$
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, KalypsoCorePlugin.getID(), Messages.getString( "ImportTimeseriesOperation_9" ), e ); //$NON-NLS-1$
    }
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
}