/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Dirk Kuch
 */
public class DeleteTimeseriesOperation implements ICoreRunnableWithProgress
{
  private final ITimeseries[] m_timeserieses;

  public DeleteTimeseriesOperation( final ITimeseries... timeserieses )
  {
    m_timeserieses = timeserieses;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final StatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* Delete data files */
    for( final ITimeseries timeseries : m_timeserieses )
    {
      try
      {
        timeseries.deleteDataFile();
      }
      catch( final Exception ex )
      {
        final String msg = String.format( Messages.getString("DeleteTimeseriesOperation_0"), timeseries.getName() ); //$NON-NLS-1$
        stati.add( IStatus.ERROR, msg, ex );
      }
    }

    try
    {
      /* Delete feature */
      final DeleteFeatureCommand deleteCommand = new DeleteFeatureCommand( m_timeserieses );

      final IScenarioDataProvider dataProvider = ScenarioHelper.getScenarioDataProvider();
      final CommandableWorkspace stationsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS );
      stationsWorkspace.postCommand( deleteCommand );
    }
    catch( final Exception ex )
    {
      final String msg = String.format( Messages.getString("DeleteTimeseriesOperation_1") ); //$NON-NLS-1$
      stati.add( IStatus.ERROR, msg, ex );
    }

    return stati.asMultiStatusOrOK( Messages.getString("DeleteTimeseriesOperation_2") ); //$NON-NLS-1$
  }
}
