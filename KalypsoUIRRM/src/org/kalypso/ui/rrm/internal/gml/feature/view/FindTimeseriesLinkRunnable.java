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
package org.kalypso.ui.rrm.internal.gml.feature.view;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.IStationCollection;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Dirk Kuch
 */
public class FindTimeseriesLinkRunnable implements ICoreRunnableWithProgress
{
  public static final ITimeseries findTimeseries( final String href )
  {
    if( StringUtils.isBlank( href ) )
      return null;

    final IStationCollection collection = getStationCollection();

    final FindTimeseriesLinkRunnable runnable = new FindTimeseriesLinkRunnable( collection, href );
    runnable.execute( new NullProgressMonitor() );

    return runnable.getTimeseries();
  }

  public static IStationCollection getStationCollection( )
  {
    try
    {
      final IScenarioDataProvider modelProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

      return modelProvider.<IStationCollection>getModel( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS );
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }

    return null;
  }

  public static CommandableWorkspace getStationsWorkspace( )
  {
    try
    {
      final IScenarioDataProvider modelProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

      return modelProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS );
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }

    return null;
  }

  private final IStationCollection m_collection;

  private final String m_href;

  private ITimeseries m_timeseries;

  public FindTimeseriesLinkRunnable( final IStationCollection collection, final String href )
  {
    m_collection = collection;

    m_href = href;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final IFeatureBindingCollection<IStation> stations = m_collection.getStations();
    for( final IStation station : stations )
    {
      if( doInspect( station ) )
        return Status.OK_STATUS;
    }

    return Status.CANCEL_STATUS;
  }

  private boolean doInspect( final IStation station )
  {
    final IFeatureBindingCollection<ITimeseries> timeserieses = station.getTimeseries();
    for( final ITimeseries timeseries : timeserieses )
    {
      if( doInspect( timeseries ) )
      {
        m_timeseries = timeseries;

        return true;
      }
    }

    return false;
  }

  private boolean doInspect( final ITimeseries timeseries )
  {
    final ZmlLink zml = timeseries.getDataLink();
    final TimeseriesLinkType link = zml.getTimeseriesLink();

    return m_href.equals( link.getHref() );
  }

  public ITimeseries getTimeseries( )
  {
    return m_timeseries;
  }
}
