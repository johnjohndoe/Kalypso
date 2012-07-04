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
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import de.renew.workflow.connector.cases.IScenario;
import de.renew.workflow.connector.cases.IScenarioList;

/**
 * @author Dirk Kuch
 */
public class TimeseriesReferencesUpdater implements ICoreRunnableWithProgress
{
  private final URL m_oldTimeseries;

  private final String m_href;

  private final IScenario m_base;

  public TimeseriesReferencesUpdater( final IScenario scenario, final URL oldTimeseries, final String href )
  {
    m_base = ScenarioHelper.findRootScenario( scenario );
    m_oldTimeseries = oldTimeseries;
    m_href = href;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final StatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final RrmScenario base = new RrmScenario( m_base.getFolder() );
    doUpdate( base, stati );

    final IScenarioList children = m_base.getDerivedScenarios();
    doIterate( children, stati );

    return stati.asMultiStatus( Messages.getString( "TimeseriesReferencesUpdater_0" ) ); //$NON-NLS-1$
  }

  private void doIterate( final IScenarioList list, final StatusCollector stati )
  {
    if( list == null )
      return;

    final List<IScenario> scenarios = list.getScenarios();
    for( final IScenario scenario : scenarios )
    {
      doUpdate( new RrmScenario( scenario.getFolder() ), stati );

      final IScenarioList children = scenario.getDerivedScenarios();
      doIterate( children, stati );
    }
  }

  private void doUpdate( final RrmScenario scenario, final StatusCollector stati )
  {
    final IFile timeseriesMappingsGml = scenario.getTimeseriesMappingsGml();
    stati.add( doUpdateTimeseriesLinks( timeseriesMappingsGml ) );

    final IFile catchmentModelsGml = scenario.getCatchmentModelsGml();
    stati.add( doUpdateTimeseriesLinks( catchmentModelsGml ) );
  }

  private IStatus doUpdateTimeseriesLinks( final IFile file )
  {
    if( !file.exists() )
      return new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "TimeseriesReferencesUpdater_1" ), file.getLocation().toOSString() ) ); //$NON-NLS-1$

    try
    {
      final GMLWorkspaceChangedListener listener = new GMLWorkspaceChangedListener();
      final UpdateTimeseriesLinksVisitor visitor = new UpdateTimeseriesLinksVisitor( m_oldTimeseries, m_href );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( file );
      workspace.addModellListener( listener );
      workspace.accept( visitor, FeatureVisitor.DEPTH_INFINITE );

      if( listener.isWorkspaceChanged() )
        GmlSerializer.saveWorkspace( workspace, file );

      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "TimeseriesReferencesUpdater_3" ), file.getLocation().toOSString() ) ); //$NON-NLS-1$;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "TimeseriesReferencesUpdater_2" ), file.getLocation().toOSString() ), e ); //$NON-NLS-1$
    }
  }
}