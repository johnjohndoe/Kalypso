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
package de.renew.workflow.connector.worklist;

import org.eclipse.core.runtime.IStatus;

import de.renew.workflow.base.ITask;

/**
 * @author Stefan Kurzbach
 */
public interface ITaskExecutor
{
  /**
   * Called when a task needs to be executed<br>
   * This method MUST be called in the swt-thread.
   *
   */
  public IStatus execute( final ITask task );

  /**
   * Returns the active task or <code>null</code> if there is no active task.
   */
  public ITask getActiveTask( );

  /**
   * Stops the active task if any. Returns true if the task could be stopped or there was no active task.
   */
  public boolean stopActiveTask( );

  /**
   * This function adds a task change listener.
   *
   * @param listener
   *            The listener, which should be added.
   */
  public void addTaskExecutionListener( ITaskExecutionListener listener );

  /**
   * This function removes a task change listener. If it is not registerd, nothing will happen.
   *
   * @param listener
   *            The listener, which should be removed.
   */
  public void removeTaskExecutionListener( ITaskExecutionListener listener );
}