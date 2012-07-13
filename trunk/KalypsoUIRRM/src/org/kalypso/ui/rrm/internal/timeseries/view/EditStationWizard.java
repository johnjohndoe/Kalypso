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
package org.kalypso.ui.rrm.internal.timeseries.view;

import java.net.URL;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.timeseries.operations.RenameStationOperation;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanWizardPage;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class EditStationWizard extends Wizard
{
  private final FeatureBean<IStation> m_stationBean;

  private String m_oldFolder;

  private URL[] m_oldTimeseriesLinks;

  public EditStationWizard( final IStation station )
  {
    m_stationBean = new FeatureBean<>( station );

    init( station );
  }

  private void init( final IStation station )
  {
    m_oldFolder = station.getTimeseriesFoldername();

    final Set<URL> urls = new LinkedHashSet<>();

    final IFeatureBindingCollection<ITimeseries> timeserieses = station.getTimeseries();
    for( final ITimeseries timeseries : timeserieses )
    {
      urls.add( timeseries.getDataLink().getLocation() );
    }

    m_oldTimeseriesLinks = urls.toArray( new URL[] {} );
  }

  @Override
  public void addPages( )
  {
    final FeatureBean<IStation> stationBean = m_stationBean;

    final WizardPage page = new FeatureBeanWizardPage( "feature" ) //$NON-NLS-1$
    {
      @Override
      protected Control createFeatureBeanControl( final Composite parent, final IDataBinding binding )
      {
        return new StationComposite( parent, stationBean, binding, true );
      }
    };

    addPage( page );
  }

  @Override
  public boolean performFinish( )
  {
    final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    try
    {
      final ICommand command = m_stationBean.applyChanges();

      final CommandableWorkspace stationsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS );
      stationsWorkspace.postCommand( command );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    final IStation station = m_stationBean.getFeature();
    final String folder = station.getTimeseriesFoldername();
    if( !StringUtils.equals( m_oldFolder, folder ) )
    {
      final RenameStationOperation operation = new RenameStationOperation( station, m_oldFolder, folder, m_oldTimeseriesLinks );
      operation.execute( new NullProgressMonitor() );

      try
      {
        /* Immediately save model, we cannot revert the operation if the folder changed */
        dataProvider.saveModel( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS, new NullProgressMonitor() );
      }
      catch( final CoreException e )
      {
        StatusDialog.open( getShell(), e.getStatus(), getWindowTitle() );
      }
    }

    return true;
  }
}