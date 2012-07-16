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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Dirk Kuch
 */
public class RepairObservationJob extends UIJob
{
  private boolean m_done = false;

  private final StatusCollector m_stati;

  private final IStatus m_status;

  private final IRepairObservationWorker m_worker;

  public RepairObservationJob( final StatusCollector stati, final IStatus status, final IRepairObservationWorker worker )
  {
    super( Messages.getString( "RepairObservationJob_0" ) ); //$NON-NLS-1$
    m_stati = stati;
    m_status = status;
    m_worker = worker;

    setSystem( true );
    setUser( false );
  }

  @Override
  public IStatus runInUIThread( final IProgressMonitor monitor )
  {
    final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();

    final StatusDialog dialog = new StatusDialog( shell, m_status, m_status.getMessage() );
    dialog.open();

    final boolean repair = MessageDialog.openConfirm( shell, m_worker.getDialogTitle(), m_worker.getDialogMessage() );

    if( repair )
    {
      m_stati.add( m_worker.execute( monitor ) );
    }
    else
      m_stati.add( IStatus.ERROR, Messages.getString( "RepairObservationJob_1" ) ); //$NON-NLS-1$

    m_done = true;

    return Status.OK_STATUS;
  }

  public boolean isDone( )
  {
    return m_done;
  }
}