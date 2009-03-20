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
package de.renew.workflow.connector;

import de.renew.workflow.connector.cases.CaseHandlingProjectNature;
import de.renew.workflow.connector.cases.ICase;

/**
 * @author Gernot Belger
 */
public class DummyConnector implements IWorkflowConnector
{
  /**
   * @see de.renew.workflow.connector.IWorkflowConnector#addWorklistChangeListener(de.renew.workflow.connector.IWorklistChangeListener)
   */
  @Override
  public void addWorklistChangeListener( final IWorklistChangeListener worklistChangeListener )
  {
  }

  /**
   * @see de.renew.workflow.connector.IWorkflowConnector#canRequest(java.lang.String)
   */
  @Override
  public boolean canRequest( final String id )
  {
    return false;
  }

  /**
   * @see de.renew.workflow.connector.IWorkflowConnector#cancel(java.lang.String)
   */
  @Override
  public void cancel( final String id )
  {
  }

  /**
   * @see de.renew.workflow.connector.IWorkflowConnector#confirm(java.lang.String, java.lang.Object)
   */
  @Override
  public void confirm( final String id, final Object result )
  {
  }

  /**
   * @see de.renew.workflow.connector.IWorkflowConnector#connect()
   */
  @Override
  public void connect( )
  {
  }

  /**
   * @see de.renew.workflow.connector.IWorkflowConnector#isActive(java.lang.String)
   */
  @Override
  public boolean isActive( final String id )
  {
    return false;
  }

  /**
   * @see de.renew.workflow.connector.IWorkflowConnector#isConnected()
   */
  @Override
  public boolean isConnected( )
  {
    return false;
  }

  /**
   * @see de.renew.workflow.connector.IWorkflowConnector#removeWorklistChangeListener(de.renew.workflow.connector.IWorklistChangeListener)
   */
  @Override
  public void removeWorklistChangeListener( final IWorklistChangeListener worklistChangeListener )
  {
  }

  /**
   * @see de.renew.workflow.connector.IWorkflowConnector#request(java.lang.String)
   */
  @Override
  public Object request( final String id )
  {
    return null;
  }

  /**
   * @see de.renew.workflow.connector.context.IActiveScenarioChangeListener#activeScenarioChanged(de.renew.workflow.connector.cases.CaseHandlingProjectNature, de.renew.workflow.connector.cases.ICase)
   */
  @Override
  public void activeScenarioChanged( final CaseHandlingProjectNature newProject, final ICase caze )
  {
  }

}
