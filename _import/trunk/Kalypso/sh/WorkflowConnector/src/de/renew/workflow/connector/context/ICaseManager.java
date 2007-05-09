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
package de.renew.workflow.connector.context;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import de.renew.workflow.base.Workflow;
import de.renew.workflow.cases.Case;

/**
 * @author Stefan Kurzbach
 */
public interface ICaseManager<T extends Case>
{
  /**
   * Returns the current case
   */
  public T getCurrentCase( );

  /**
   * Returns the workflow for the current case
   */
  public Workflow getCurrentWorkflow( );

  /**
   * Sets the current case
   */
  public void setCurrentCase( final T caze );

  /**
   * Returns all the cases
   */
  public List<T> getCases( );

  /**
   * Returns the case with the given uri. If no scenario with the given uri exists, <code>null</code> will be
   * returned.
   */
  public T getCase( final String uri );

  /**
   * Removes the case.
   */
  public void removeCase( final T caze, final IProgressMonitor monitor ) throws CoreException;

  /**
   * Creates a new case with the given name.
   */
  public T createCase( final String name ) throws CoreException;

  /**
   * Registers <code>listener</code> with this case manager. If an identical listener is already registered, this
   * method has no effect.
   * 
   * @param listener
   *          the listener to be removed, must not be <code>null</code>
   */
  public void addCaseManagerListener( final ICaseManagerListener<T> listener );

  /**
   * Removes <code>listener</code> from this case manager. If no identical listener was registered, this method has no
   * effect.
   * 
   * @param listener
   *          the listener to be removed
   */
  public void removeCaseManagerListener( final ICaseManagerListener<T> listener );

  /**
   * Deregisters all listeners
   */
  public void dispose( );
}
