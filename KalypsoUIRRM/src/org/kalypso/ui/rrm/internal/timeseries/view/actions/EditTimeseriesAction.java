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
package org.kalypso.ui.rrm.internal.timeseries.view.actions;

import java.net.URL;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.operations.TimeseriesReferencesUpdater;
import org.kalypso.ui.rrm.internal.timeseries.view.edit.EditTimeseriesDialog;
import org.kalypso.ui.rrm.internal.timeseries.view.edit.TimeseriesDialogSource;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Dirk Kuch
 */
public class EditTimeseriesAction extends Action
{
  private final IDataBinding m_binding;

  private final FeatureBean<ITimeseries> m_timeseries;

  public EditTimeseriesAction( final FeatureBean<ITimeseries> timeseries, final IDataBinding binding )
  {
    m_timeseries = timeseries;
    m_binding = binding;

    setText( Messages.getString( "EditTimeseriesAction_0" ) ); //$NON-NLS-1$
    setToolTipText( Messages.getString( "EditTimeseriesAction_1" ) ); //$NON-NLS-1$
    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.EDIT_STATION ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    /* Get the shell. */
    final Shell shell = event.widget.getDisplay().getActiveShell();

    try
    {
      /* Get the feature of the timeseries. */
      final ITimeseries timeseries = m_timeseries.getFeature();

      /* Store some values, before the editing starts. */
      final String oldQuality = timeseries.getQuality();
      final String oldHref = timeseries.getDataLink().getHref();
      final IFile oldFile = timeseries.getDataLink().getFile();

      /* Create the timeseries dialog source. */
      final TimeseriesDialogSource source = new TimeseriesDialogSource( timeseries );

      /* Create and open the dialog. */
      final EditTimeseriesDialog dialog = new EditTimeseriesDialog( shell, m_timeseries, source, m_binding, PlatformUI.getWorkbench() );
      final int open = dialog.open();
      if( Window.OK == open )
      {
        /* Save everything. */
        save( source );

        /* If the quality has changed, rename the zml file. */
        if( isQualityChanged( oldQuality ) )
        {
          /* Get the data provider. */
          final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

          /* Get the new href. */
          final String newHref = timeseries.getDataLink().getHref();

          /* Create the old url. */
          final URL oldUrl = UrlResolverSingleton.resolveUrl( timeseries.getWorkspace().getContext(), oldHref );

          final TimeseriesReferencesUpdater updater = new TimeseriesReferencesUpdater( dataProvider.getScenario(), oldUrl, newHref );
          final IStatus status = updater.execute( new NullProgressMonitor() );
          if( !status.isOK() )
            StatusDialog.open( shell, status, Messages.getString( "EditTimeseriesAction.0" ) ); //$NON-NLS-1$

          /* Get the new file. */
          final IFile newFile = timeseries.getDataLink().getFile();
          oldFile.move( newFile.getFullPath(), true, new NullProgressMonitor() );

          /* Move the status file, too. */
          final IContainer oldParent = oldFile.getParent();
          final IFile oldStatusFile = oldParent.getFile( new Path( oldFile.getName() + ".status" ) ); //$NON-NLS-1$
          if( oldStatusFile.exists() )
          {
            final IContainer newParent = newFile.getParent();
            final IFile newStatusFile = newParent.getFile( new Path( newFile.getName() + ".status" ) ); //$NON-NLS-1$
            oldStatusFile.move( newStatusFile.getFullPath(), false, new NullProgressMonitor() );
          }
        }
      }

      /* Dispose the source. */
      source.dispose();
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }
  }

  private void save( final TimeseriesDialogSource source ) throws CoreException, Exception
  {
    /* Save the timeseries to the zml. */
    source.save();

    /* Get the data provider. */
    final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    /* Update the gml. */
    final ICommand command = m_timeseries.applyChanges();
    if( Objects.isNotNull( command ) )
    {
      final CommandableWorkspace stationsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS );
      stationsWorkspace.postCommand( command );
    }

    /* Save the model. */
    dataProvider.saveModel( new NullProgressMonitor() );
  }

  private boolean isQualityChanged( final String oldQuality )
  {
    /* Get the quality. After editing, this property may have changed. */
    final String quality = m_timeseries.getFeature().getQuality();
    if( Objects.equal( oldQuality, quality ) )
      return false;

    return true;
  }
}