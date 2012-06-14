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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMappingCollection;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesBean;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * moves a timeseries from one station to the given target station
 * 
 * @author Dirk Kuch
 */
public class MoveTimeSeriesOperation implements ICoreRunnableWithProgress
{
  private final IStation m_target;

  private final ITimeseries m_timeseries;

  private ITimeseries m_moved;

  public MoveTimeSeriesOperation( final IStation target, final ITimeseries timeseries )
  {
    m_target = target;
    m_timeseries = timeseries;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final StatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final ZmlLink link = m_timeseries.getDataLink();
    final IFile oldFile = link.getFile();

    final IObservation observation = link.getObservationFromPool();

    final ObservationImportOperation importOperation = new ObservationImportOperation( observation, m_timeseries.getParameterType() );

    final StoreTimeseriesOperation storeOperation = new StoreTimeseriesOperation( new TimeseriesBean(), m_target, importOperation );
    storeOperation.updateDataAfterFinish();
    stati.add( storeOperation.execute( monitor ) );

    m_moved = storeOperation.getTimeseries();
    if( m_moved == null )
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString("MoveTimeSeriesOperation.0") ); //$NON-NLS-1$

    stati.add( UpdateTimeseriesLinks.doUpdateTimeseriesLinks( m_timeseries, m_moved ) );

    UpdateTimeseriesLinks.doUpdateTimeseriesLinks( m_timeseries, m_moved );

    final DeleteTimeseriesOperation deleteOperation = new DeleteTimeseriesOperation( m_timeseries );
    stati.add( deleteOperation.execute( monitor ) );

    final IStatus status = stati.asMultiStatusOrOK( String.format( Messages.getString( "MoveTimeSeriesOperation_0" ), m_timeseries.getName() ) ); //$NON-NLS-1$

    final StoreTimeseriesStatusOperation storeStatusOperation = new StoreTimeseriesStatusOperation( m_moved, status );
    stati.add( storeStatusOperation.execute( monitor ) );

    doUpdateMappings( oldFile, m_moved );

    return status; //$NON-NLS-1$
  }

  private void doUpdateMappings( final IFile oldFile, final ITimeseries target ) throws IllegalArgumentException, CoreException
  {
    final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    final CommandableWorkspace timeseriesMappingsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_TIMESERIES_MAPPINGS );
    final ITimeseriesMappingCollection mappings = (ITimeseriesMappingCollection) timeseriesMappingsWorkspace.getRootFeature();

    if( mappings != null )
    {
      final IFeatureBindingCollection<ITimeseriesMapping> collection = mappings.getTimeseriesMappings();
      collection.accept( new UpdateTimeseriesMappingsVisitor( oldFile, target.getDataLink().getHref() ) );
    }
  }

  public ITimeseries getMovedTimeseries( )
  {
    return m_moved;
  }

}
