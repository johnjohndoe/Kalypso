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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesBean;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * moves a timeseries from one station to the given target station
 * 
 * @author Dirk Kuch
 */
public class MoveTimeSeriesOperation implements ICoreRunnableWithProgress
{
  private final IStation m_target;

  private final ITimeseries m_timeseries;

  private final ITreeNodeModel m_model;

  private ITimeseries m_moved;

  public MoveTimeSeriesOperation( final ITreeNodeModel model, final IStation target, final ITimeseries timeseries )
  {
    m_model = model;
    m_target = target;
    m_timeseries = timeseries;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final StatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final ZmlLink link = m_timeseries.getDataLink();
    final IObservation observation = link.getObservationFromPool();

    final ObservationImportOperation importOperation = new ObservationImportOperation( observation, m_timeseries.getParameterType() );

    final StoreTimeseriesOperation storeOperation = new StoreTimeseriesOperation( new TimeseriesBean(), m_model.getWorkspace(), m_target, importOperation );
    storeOperation.updateDataAfterFinish();
    stati.add( storeOperation.execute( monitor ) );

    m_moved = storeOperation.getTimeseries();
    stati.add( UpdateTimeseriesLinks.doUpdateTimeseriesLinks( m_timeseries, m_moved ) );

    m_model.getWorkspace().getContext();

    new UpdateTimeseriesLinksVisitor( m_timeseries, m_moved );

    final DeleteTimeseriesOperation deleteOperation = new DeleteTimeseriesOperation( m_model, m_timeseries );
    stati.add( deleteOperation.execute( monitor ) );

    return stati.asMultiStatusOrOK( String.format( Messages.getString("MoveTimeSeriesOperation_0"), m_timeseries.getName() ) ); //$NON-NLS-1$
  }

  public ITimeseries getMovedTimeseries( )
  {
    return m_moved;
  }

}
