/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.timeseries.view.edit.EditTimeseriesDialog;
import org.kalypso.ui.rrm.internal.timeseries.view.edit.EditTimeseriesDialogSource;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;

/**
 * @author Gernot Belger
 */
public class EditTimeseriesAction extends Action
{

  private final IDataBinding m_binding;

  private final FeatureBean<ITimeseries> m_timeseries;

  public EditTimeseriesAction( final FeatureBean<ITimeseries> timeseries, final IDataBinding binding )
  {
    m_timeseries = timeseries;
    m_binding = binding;

    setText( "Edit Timeseries" ); //$NON-NLS-1$
    setToolTipText( "Edit selected Timeseries" ); //$NON-NLS-1$

    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.EDIT_STATION ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();
    final IWorkbench context = PlatformUI.getWorkbench();

    try
    {
      final EditTimeseriesDialogSource source = new EditTimeseriesDialogSource( m_timeseries.getFeature() );
      final EditTimeseriesDialog dialog = new EditTimeseriesDialog( shell, m_timeseries, source, m_binding, context );
      final int open = dialog.open();

      if( Window.OK == open )
        source.save();

      source.dispose();

    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }

  }
}
