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
package de.renew.workflow.connector.context;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.WorkflowConnectorPlugin;

/**
 * This project nature adds the possibility to handle cases inside the project and keep information about the current
 * workflow state of cases
 * 
 * @author Stefan Kurzbach
 */
public abstract class CaseHandlingProjectNature<T extends Case> implements IProjectNature, ICaseManagerListener<T>
{
  public static final String ID = "org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature";

  private ICaseManager<T> m_caseManager;

  private IProject m_project;

  /**
   * Creates a specific case manager for this project
   */
  protected abstract ICaseManager<T> createCaseManager( final IProject project ) throws CoreException;

  private void init( )
  {
    try
    {
      m_caseManager = createCaseManager( m_project );
      m_caseManager.addCaseManagerListener( this );
    }
    catch( final CoreException e )
    {
      final Shell activeShell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
      final IStatus status = e.getStatus();
      ErrorDialog.openError( activeShell, "Problem", "Konnte neue Falldaten nicht erzeugen.", status );
      WorkflowConnectorPlugin.getDefault().getLog().log( status );
    }
  }

  synchronized public ICaseManager<T> getCaseManager( )
  {
    if( m_caseManager == null )
    {
      init();
    }
    return m_caseManager;
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#configure()
   */
  @SuppressWarnings("unused")
  public void configure( ) throws CoreException
  {
    // does nothing by default
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#deconfigure()
   */
  public void deconfigure( )
  {
    // does nothing by default
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#getProject()
   */
  public IProject getProject( )
  {
    return m_project;
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
   */
  synchronized public void setProject( final IProject project )
  {
    this.m_project = project;
  }

  /**
   * Constructs a path for the scenario relative to the project location.
   */
  public IPath getProjectPath( final T caze )
  {
    return new Path( caze.getName() );
  }

  /**
   * @see de.renew.workflow.connector.context.ICaseManagerListener#caseAdded(de.renew.workflow.cases.Case)
   */
  public void caseAdded( final T caze )
  {
    final IFolder newFolder = m_project.getFolder( getProjectPath( caze ) );

    if( !newFolder.exists() )
    {
      try
      {
        newFolder.create( false, true, null );
      }
      catch( final CoreException e )
      {
        final Shell activeShell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        ErrorDialog.openError( activeShell, "Problem", "Konnte neue Falldaten nicht erzeugen.", status );
        WorkflowConnectorPlugin.getDefault().getLog().log( status );
      }
    }
  }

  public void caseRemoved( final T caze )
  {
    final IFolder folder = m_project.getFolder( getProjectPath( caze ) );
    try
    {
      folder.delete( true, null );
    }
    catch( final CoreException e )
    {
      final Shell activeShell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      ErrorDialog.openError( activeShell, "Problem", "Konnte Falldaten nicht löschen.", status );
      WorkflowConnectorPlugin.getDefault().getLog().log( status );
    }
  }

}