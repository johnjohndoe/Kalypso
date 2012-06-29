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

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.cm.ICatchmentModel;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMappingCollection;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.connector.cases.IScenario;
import de.renew.workflow.connector.cases.IScenarioList;

/**
 * @author Dirk Kuch
 */
public class TimeseriesReferencesUpdater implements ICoreRunnableWithProgress
{
  private final IFile m_oldFile;

  private final String m_href;

  private final IScenario m_base;

  public TimeseriesReferencesUpdater( final IScenario scenario, final IFile oldFile, final String href )
  {
    m_base = ScenarioHelper.findRootScenario( scenario );
    m_oldFile = oldFile;
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
    final IResource timeseriesMappingsGml = scenario.getTimeseriesMappingsGml();
    stati.add( doUpdateTimeseriesMappingModel( timeseriesMappingsGml ) );

    final IResource catchmentModelsGml = scenario.getCatchmentModelsGml();
    stati.add( doUpdateCatchmentModel( catchmentModelsGml ) );
  }

  private IStatus doUpdateCatchmentModel( final IResource resource )
  {
    if( !resource.exists() )
      return new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "TimeseriesReferencesUpdater_1" ), resource.getLocation().toOSString() ) ); //$NON-NLS-1$

    final IFile file = (IFile) resource;
    try
    {
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( file );
      final ICatchmentModel model = (ICatchmentModel) workspace.getRootFeature();

      if( model != null )
      {
        final IFeatureBindingCollection<IRainfallGenerator> generators = model.getGenerators();
        final UpdateCatchmentTimeseriesReferencesVisitor visitor = new UpdateCatchmentTimeseriesReferencesVisitor( m_oldFile, m_href );
        generators.accept( visitor );

        if( visitor.wasChanged() )
          GmlSerializer.saveWorkspace( workspace, file );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "TimeseriesReferencesUpdater_2" ), resource.getLocation().toOSString() ), e ); //$NON-NLS-1$
    }

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "TimeseriesReferencesUpdater_3" ), resource.getLocation().toOSString() ) ); //$NON-NLS-1$

  }

  private IStatus doUpdateTimeseriesMappingModel( final IResource resource )
  {
    if( !resource.exists() )
      return new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "TimeseriesReferencesUpdater_4" ), resource.getLocation().toOSString() ) ); //$NON-NLS-1$

    final IFile file = (IFile) resource;
    try
    {
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( file );
      final ITimeseriesMappingCollection collection = (ITimeseriesMappingCollection) workspace.getRootFeature();
      if( collection != null )
      {
        final UpdateTimeseriesMappingsVisitor visitor = new UpdateTimeseriesMappingsVisitor( m_oldFile, m_href );

        final IFeatureBindingCollection<ITimeseriesMapping> mappings = collection.getTimeseriesMappings();
        mappings.accept( visitor );

        if( visitor.wasChanged() )
          GmlSerializer.saveWorkspace( workspace, file );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "TimeseriesReferencesUpdater_5" ), resource.getLocation().toOSString() ), e ); //$NON-NLS-1$
    }

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "TimeseriesReferencesUpdater_6" ), resource.getLocation().toOSString() ) ); //$NON-NLS-1$
  }
}