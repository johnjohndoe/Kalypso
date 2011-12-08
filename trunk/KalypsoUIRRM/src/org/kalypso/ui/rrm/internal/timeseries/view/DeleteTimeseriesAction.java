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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.timeseries.binding.Timeseries;

/**
 * @author Gernot Belger
 */
public class DeleteTimeseriesAction extends Action
{
  private final Timeseries[] m_timeseries;

  private final String m_deleteMessage;

  private final TimeseriesNode m_parentNode;

  public DeleteTimeseriesAction( final TimeseriesNode parentNode, final String deleteMessage, final Timeseries... timeseries )
  {
    m_parentNode = parentNode;
    m_timeseries = timeseries;
    m_deleteMessage = deleteMessage;

    setText( "Delete Timeseries" );
    setToolTipText( "Delete selected timeseries" );

    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.DELETE ) );

    if( timeseries.length == 0 )
    {
      setEnabled( false );
      setToolTipText( "Element contains no timeseries" );
    }
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    if( !MessageDialog.openConfirm( shell, getText(), m_deleteMessage ) )
      return;

    try
    {
      /* Delete data files */
      for( final Timeseries timeseries : m_timeseries )
      {
        final ZmlLink dataLink = timeseries.getDataLink();
      final IFile file = dataLink.getFile();
      if( file != null && file.exists() )
        file.delete( false, true, null );
      }

      final TimeseriesTreeContext context = m_parentNode.getContext();

      /* Select parent node */
      context.setSelection( m_parentNode );

      /* Delete feature */
      final DeleteFeatureCommand deleteCommand = new DeleteFeatureCommand( m_timeseries );
      context.postCommand( deleteCommand );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to delete timeseries", e );
      StatusDialog.open( shell, status, getText() );
    }
  }
}
