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
package org.kalypso.ui.rrm.internal.timeseries.view.actions;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Gernot Belger
 */
public class DeleteStationAction extends Action
{
  private final IStation[] m_stations;

  private final ITreeNodeModel m_model;

  public DeleteStationAction( final ITreeNodeModel model, final IStation... station )
  {
    m_model = model;
    m_stations = station;

    setText( Messages.getString("DeleteStationAction_0") ); //$NON-NLS-1$
    setToolTipText( Messages.getString("DeleteStationAction_1") ); //$NON-NLS-1$

    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.DELETE ) );

    if( station.length == 0 )
    {
      setEnabled( false );
      setToolTipText( Messages.getString("DeleteStationAction_2") ); //$NON-NLS-1$
    }
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final String deleteMessage = getDeleteMessage();

    if( !MessageDialog.openConfirm( shell, getText(), deleteMessage ) )
      return;

    try
    {
      /* Delete data files */
      for( final IStation station : m_stations )
      {
        for( final ITimeseries timeseries : station.getTimeseries() )
          timeseries.deleteDataFile();
      }

      /* Delete feature */
      final DeleteFeatureCommand deleteCommand = new DeleteFeatureCommand( m_stations );
      m_model.postCommand( deleteCommand );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString("DeleteStationAction_3"), e ); //$NON-NLS-1$
      StatusDialog.open( shell, status, getText() );
    }
  }

  private String getDeleteMessage( )
  {
    if( m_stations.length > 1 )
      return Messages.getString("DeleteStationAction_4"); //$NON-NLS-1$

    return String.format( Messages.getString("DeleteStationAction_5"), m_stations[0].getDescription() ); //$NON-NLS-1$
  }
}
