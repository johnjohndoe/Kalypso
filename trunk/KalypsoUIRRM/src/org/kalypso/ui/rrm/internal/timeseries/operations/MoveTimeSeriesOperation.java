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
package org.kalypso.ui.rrm.internal.timeseries.operations;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.timeseries.StationClassesCatalog;
import org.kalypso.model.hydrology.timeseries.Timeserieses;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

import de.renew.workflow.connector.cases.IScenario;
import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * moves a timeseries from one station to the given target station
 *
 * @author Dirk Kuch
 */
public class MoveTimeSeriesOperation implements ICoreRunnableWithProgress
{
  private final Collection<ITimeseries> m_movedTimeseries = new ArrayList<>();

  private final IStation m_target;

  private final ITimeseries[] m_timeseries;

  public MoveTimeSeriesOperation( final IStation target, final ITimeseries... timeseries )
  {
    m_target = target;
    m_timeseries = timeseries;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString("MoveTimeSeriesOperation.1"), m_timeseries.length ); //$NON-NLS-1$

    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    for( final ITimeseries timeseries : m_timeseries )
    {
      try
      {
        final IStatus status = moveTimeseries( timeseries, new SubProgressMonitor( monitor, 1 ) );
        log.add( status );
      }
      catch( final CoreException e )
      {
        log.add( e.getStatus() );
      }
    }

    /* Save stations workspace, we cannot revert this operation */
    try
    {
      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      dataProvider.saveModel( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS, new NullProgressMonitor() );
    }
    catch( final CoreException e )
    {
      log.add( e.getStatus() );
    }

    return log.asMultiStatus( Messages.getString("MoveTimeSeriesOperation.1") ); //$NON-NLS-1$
  }

  private IStatus moveTimeseries( final ITimeseries timeseries, final IProgressMonitor monitor ) throws CoreException
  {
    final IStatusCollector log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final String timeseriesLabel = Timeserieses.toLinkLabel( timeseries );

    /* If already in same station, inform use but do nothing */
    if( timeseries.getStation() == m_target )
    {
      m_movedTimeseries.add( timeseries );
      final String message = String.format( Messages.getString("MoveTimeSeriesOperation.3"), timeseriesLabel ); //$NON-NLS-1$
      return new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), message );
    }

    /* If target station is not allowed to holde the parameter type, inform use but do nothing */
    final String parameterType = timeseries.getParameterType();
    final String[] allowedTypes = StationClassesCatalog.findAllowedParameterTypes( m_target );
    if( !ArrayUtils.contains( allowedTypes, parameterType ) )
    {
      m_movedTimeseries.add( timeseries );
      final String message = String.format( Messages.getString("MoveTimeSeriesOperation.4"), timeseriesLabel, m_target.getDescription(), parameterType ); //$NON-NLS-1$
      return new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), message );
    }

    final ZmlLink oldLink = timeseries.getDataLink();
    final IObservation observation = oldLink.getObservationFromPool();
    final ObservationImportOperation importOperation = new ObservationImportOperation( observation, parameterType, timeseries.getQuality(), timeseries.getDescription() );

    final StoreTimeseriesOperation storeOperation = new StoreTimeseriesOperation( m_target, importOperation );
    log.add( storeOperation.execute( monitor ) );

    final ITimeseries moved = storeOperation.getTimeseries();
    if( moved == null )
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "MoveTimeSeriesOperation.0", timeseriesLabel ) ); //$NON-NLS-1$

    m_movedTimeseries.add( moved );

    final ZmlLink newLink = moved.getDataLink();
    final IStatus updateStatus = doUpdateTimeseriesLinks( oldLink.getLocation(), newLink.getHref() );
    log.add( updateStatus );

    /* Delete the old timeseries and its status. */
    final DeleteTimeseriesOperation deleteOperation = new DeleteTimeseriesOperation( timeseries );
    log.add( deleteOperation.execute( monitor ) );

    /* Store the status for the new timeseries. */
    final IStatus status = log.asMultiStatus( timeseriesLabel ); //$NON-NLS-1$

    final StoreTimeseriesStatusOperation storeStatusOperation = new StoreTimeseriesStatusOperation( moved, status );
    log.add( storeStatusOperation.execute( monitor ) );

    return status;
  }

  private IStatus doUpdateTimeseriesLinks( final URL oldTimeseries, final String href )
  {
    final IScenario scenario = KalypsoAFGUIFrameworkPlugin.getActiveWorkContext().getCurrentCase();
    final TimeseriesReferencesUpdater updater = new TimeseriesReferencesUpdater( scenario, oldTimeseries, href );
    return updater.execute( new NullProgressMonitor() );
  }

  public ITimeseries[] getMovedTimeseries( )
  {
    return m_movedTimeseries.toArray( new ITimeseries[m_movedTimeseries.size()] );
  }
}