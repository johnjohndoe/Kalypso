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

import org.apache.commons.io.IOCase;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.edit.EditTimeseriesDialog;
import org.kalypso.ui.rrm.internal.timeseries.view.edit.TimeseriesDialogSource;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;

/**
 * @author Dirk Kuch
 */
public class EditTimeseriesAction extends Action
{
  private final IDataBinding m_binding;

  private final FeatureBean<ITimeseries> m_timeseries;

  private final CommandableWorkspace m_workspace;

  public EditTimeseriesAction( final CommandableWorkspace workspace, final FeatureBean<ITimeseries> timeseries, final IDataBinding binding )
  {
    m_workspace = workspace;
    m_timeseries = timeseries;
    m_binding = binding;

    setText( Messages.getString( "EditTimeseriesAction_0" ) ); //$NON-NLS-1$
    setToolTipText( Messages.getString( "EditTimeseriesAction_1" ) ); //$NON-NLS-1$

    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.EDIT_STATION ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();
    final IWorkbench context = PlatformUI.getWorkbench();

    try
    {
      final ITimeseries timeseries = m_timeseries.getFeature();
      final ZmlLink link = timeseries.getDataLink();
      final IFile oldFile = link.getFile();
      final String oldQuality = timeseries.getQuality();

      final TimeseriesDialogSource source = new TimeseriesDialogSource( timeseries );
      final EditTimeseriesDialog dialog = new EditTimeseriesDialog( shell, m_timeseries, source, m_binding, context );
      final int open = dialog.open();
      if( Window.OK == open )
      {
        source.save();

        final ICommand command = m_timeseries.applyChanges();
        if( Objects.isNotNull( command ) )
          m_workspace.postCommand( command );

        // quality changed? so rename zml file!
        if( isQualityChanged( oldQuality ) )
        {
          final IFile target = link.getFile();
          oldFile.move( target.getFullPath(), true, new NullProgressMonitor() );
        }
      }

      source.dispose();

    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }

  }

  private boolean isQualityChanged( final String oldQuality )
  {
    final String quality = m_timeseries.getFeature().getQuality();

    if( Objects.allNull( quality, oldQuality ) )
      return false;
    else if( Objects.equal( oldQuality, quality ) )
    {
      return !IOCase.SYSTEM.checkEquals( oldQuality, quality );
    }

    return true;

  }
}
