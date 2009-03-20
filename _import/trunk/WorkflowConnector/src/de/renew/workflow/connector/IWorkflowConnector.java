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
package de.renew.workflow.connector;

import de.renew.workflow.connector.cases.ICase;
import de.renew.workflow.connector.context.IActiveScenarioChangeListener;

/**
 * @author Stefan Kurzbach
 */
public interface IWorkflowConnector extends IActiveScenarioChangeListener<ICase>
{

  public boolean isConnected( );

  public void connect( );

  public void addWorklistChangeListener( final IWorklistChangeListener worklistChangeListener );

  public void removeWorklistChangeListener( final IWorklistChangeListener worklistChangeListener );

  /**
   * Returns true if the work item with the id is currently available
   */
  public boolean canRequest( final String id );

  public boolean isActive( final String id );

  /**
   * Requests a new WorkItem and confirms the active Activity, if there is one
   */
  public Object request( final String id );

  /**
   * Confirms an activity that was previously requested
   */
  public void confirm( final String id, final Object result );

  /**
   * Cancels an activity that was previously requested
   */
  public void cancel( final String id );

}