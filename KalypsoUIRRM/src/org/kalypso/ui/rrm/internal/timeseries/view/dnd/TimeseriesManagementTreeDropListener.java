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
package org.kalypso.ui.rrm.internal.timeseries.view.dnd;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerDropAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.timeseries.Timeserieses;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.operations.MoveTimeSeriesOperation;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;

/**
 * @author Dirk Kuch
 */
public class TimeseriesManagementTreeDropListener extends ViewerDropAdapter
{
  public TimeseriesManagementTreeDropListener( final TreeViewer treeViewer )
  {
    super( treeViewer );
  }

  @Override
  public boolean performDrop( final Object data )
  {
    final ITimeseries[] timeserieses = (ITimeseries[]) data;
    if( ArrayUtils.isEmpty( timeserieses ) )
      return false;

    final DropTargetEvent event = getCurrentEvent();
    final Shell shell = event.widget.getDisplay().getActiveShell();
    final String windowTitle = Messages.getString("TimeseriesManagementTreeDropListener.0"); //$NON-NLS-1$

    final IStation station = findStation( getCurrentTarget() );
    if( station == null )
      return false;

    if( isSameStation( station, timeserieses ) )
    {
      // TODO: can't we check this beforehand?
      final IStatus status = new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), Messages.getString("TimeseriesManagementTreeDropListener.1") ); //$NON-NLS-1$
      StatusDialog.open( shell, status, windowTitle );
      return false;
    }

    final ITreeNodeModel model = getModel();
    if( Objects.isNull( model ) )
      return false;

    final StringBuilder message = new StringBuilder();
    message.append( String.format( Messages.getString("TimeseriesManagementTreeDropListener.2"), station.getDescription() ) ); //$NON-NLS-1$
    for( final ITimeseries timeseries : timeserieses )
    {
      message.append( " - " ); //$NON-NLS-1$
      message.append( Timeserieses.toLinkLabel( timeseries ) );
      message.append( '\n' );
    }

    if( !MessageDialog.openConfirm( shell, windowTitle, message.toString() ) )
      return false;

    final MoveTimeSeriesOperation operation = new MoveTimeSeriesOperation( station, timeserieses );
    final IStatus execute = ProgressUtilities.busyCursorWhile( operation );

    final ITimeseries[] movedTimeseries = operation.getMovedTimeseries();
    for( final ITimeseries moved : movedTimeseries )
      model.refreshTree( moved );

    StatusDialog.open( shell, execute, windowTitle );

    return false;
  }

  private boolean isSameStation( final IStation station, final ITimeseries[] timeserieses )
  {
    for( final ITimeseries timeseries : timeserieses )
    {
      final IStation tsStation = timeseries.getStation();
      if( tsStation != station )
        return false;
    }

    return true;
  }

  private ITreeNodeModel getModel( )
  {
    final Object input = getViewer().getInput();
    if( input instanceof ITreeNodeModel )
      return (ITreeNodeModel) input;

    return null;
  }

  @Override
  public boolean validateDrop( final Object target, final int operation, final TransferData transferType )
  {
    final IStation station = findStation( target );
    if( station == null )
      return false;

    // TODO: does not work, how to we get the current drag data?
    // final Object data = getCurrentEvent().data;
    // final ITimeseries[] timeserieses = (ITimeseries[]) data;
    // if( timeserieses == null )
    // return false;
    //
    // if( isSameStation( station, timeserieses ) )
    // return false;

    return true;
  }

  private IStation findStation( final Object target )
  {
    if( !(target instanceof TreeNode) )
      return null;

    final TreeNode node = (TreeNode) target;
    final Object objStation = node.getAdapter( IStation.class );
    if( objStation instanceof IStation )
      return (IStation) objStation;

    return null;
  }
}