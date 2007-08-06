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

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;

import de.renew.workflow.base.IWorkflowSystem;
import de.renew.workflow.base.Workflow;
import de.renew.workflow.base.WorkflowSystem;

/**
 * @author Stefan Kurzbach
 * 
 */
public class WorkflowProjectNature implements IProjectNature
{
  private static final String ID = "de.renew.workflow.connector.WorkflowProjectNature";

  protected IProject m_project;

  private IWorkflowSystem m_workflow;

  /**
   * @see org.eclipse.core.resources.IProjectNature#configure()
   */
  public void configure( ) throws CoreException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#deconfigure()
   */
  public void deconfigure( ) throws CoreException
  {
    // TODO Auto-generated method stub

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
  public void setProject( final IProject project )
  {
    m_project = project;
  }

  /**
   * Returns the current workflow description
   */
  public Workflow getCurrentWorklist( ) throws CoreException
  {
    if( m_workflow == null )
    {
      m_workflow = new WorkflowSystem( m_project );
    }
    return m_workflow.getCurrentWorkflow();
  }

  public static final WorkflowProjectNature toThisNature( final IProject project ) throws CoreException
  {
    return (WorkflowProjectNature) project.getNature( ID );
  }
  
  public static final void addNature( final IProject project ) throws CoreException
  {
    if( project.hasNature( ID ) )
    {
      return;
    }
    else
    {
      IProjectDescription description = project.getDescription();
      String[] natures = description.getNatureIds();
      String[] newNatures = new String[natures.length + 1];
      System.arraycopy( natures, 0, newNatures, 0, natures.length );
      newNatures[natures.length] = ID;
      description.setNatureIds( newNatures );
      project.setDescription( description, new NullProgressMonitor() );
    }
  }
}
