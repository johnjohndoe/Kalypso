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
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.util.Observations;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.IMergeTimeseriesOperation;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Dirk Kuch
 */
public class ReplaceTimeseriesOperation implements IMergeTimeseriesOperation
{
  private final ITimeseries m_timeseries;

  private IObservation m_observation;

  public ReplaceTimeseriesOperation( final ITimeseries timeseries )
  {
    m_timeseries = timeseries;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    try
    {
      final ZmlLink link = m_timeseries.getDataLink();
      final IFile targetFile = link.getFile();

      final DateRange dateRange = Observations.findDateRange( m_observation );
      MetadataHelper.setTargetDateRange( m_observation.getMetadataList(), dateRange );

      ZmlFactory.writeToFile( m_observation, targetFile );

      final FeatureChange changeStart = new FeatureChange( m_timeseries, ITimeseries.PROPERTY_MEASUREMENT_START, DateUtilities.toXMLGregorianCalendar( dateRange.getFrom() ) );
      final FeatureChange changeEnd = new FeatureChange( m_timeseries, ITimeseries.PROPERTY_MEASUREMENT_END, DateUtilities.toXMLGregorianCalendar( dateRange.getTo() ) );

      final FeatureChange[] changes = new FeatureChange[] { changeStart, changeEnd };

      final ICommand command = new ChangeFeaturesCommand( m_timeseries.getWorkspace(), changes );

      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final CommandableWorkspace stationsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS );

      stationsWorkspace.postCommand( command );
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();

      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "ReplaceTimeseriesObservation_0" ), ex ); //$NON-NLS-1$
    }

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "ReplaceTimeseriesObservation_1" ) ); //$NON-NLS-1$
  }

  @Override
  public void setObservation( final IObservation observation )
  {
    m_observation = observation;
  }

}
