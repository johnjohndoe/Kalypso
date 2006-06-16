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
package org.kalypso.portal.wizard;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceStatus;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.eclipse.ui.dialogs.WizardNewProjectReferencePage;
import org.kalypso.portal.KalypsoPortalPlugin;

/**
 * Diese Klasse wurde von der Eclipse Klasse BasicNewProjectResourceWizard fast eins zu eins kopiert. Der Grund liegt
 * darin, dass die Nature eines Projektes nur einmal gesetzt werden kann und das wenn create() aufgerufen wird. Da die
 * methode createNewProject() private ist kann diese nicht überschrieben werden.
 * 
 * @see org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard
 * @author kuepfer
 */
public class NewDssProjectWizard extends Wizard implements INewWizard
{

  private WizardNewProjectCreationPage m_mainPage;

  private WizardNewProjectReferencePage m_referencePage;

  private IProject m_newProject;

  private IProjectDescription m_description;

  private String m_natureID;

  // TODO how do I get the the rrm modelnature from a central place
  private final static String m_defaultNature = "org.kalypso.simulation.ui.ModelNature";

  public NewDssProjectWizard( )
  {
    IDialogSettings workbenchSettings = KalypsoPortalPlugin.getDefault().getDialogSettings();
    IDialogSettings section = workbenchSettings.getSection( "NewDssProjectResourceWizard" );//$NON-NLS-1$
    if( section == null )
      section = workbenchSettings.addNewSection( "NewDssProjectResourceWizard" );//$NON-NLS-1$
    setDialogSettings( section );
  }

  public void setProjectDescription( IProjectDescription description )
  {
    m_description = description;
  }

  public void setNature( String id )
  {
    m_natureID = id;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    createNewProject();

    if( m_newProject == null )
      return false;

    // updatePerspective();
    // selectAndReveal(newProject);

    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    setNeedsProgressMonitor( true );
    setWindowTitle( "Neues Projekt" );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    super.addPages();

    m_mainPage = new WizardNewProjectCreationPage( "basicNewProjectPage" );//$NON-NLS-1$
    m_mainPage.setTitle( "Projekt" );
    m_mainPage.setDescription( "Erzeugen eines neuen Projektes." );
    this.addPage( m_mainPage );

    // only add page if there are already projects in the workspace
    if( ResourcesPlugin.getWorkspace().getRoot().getProjects().length > 0 )
    {
      m_referencePage = new WizardNewProjectReferencePage( "basicReferenceProjectPage" );//$NON-NLS-1$
      m_referencePage.setTitle( "Projketabhängikeiten" );
      m_referencePage.setDescription( "Wählen sie eine Projektabhängigkeit" );
      this.addPage( m_referencePage );
    }
  }

  /**
   * Creates a new project resource with the selected name.
   * <p>
   * In normal usage, this method is invoked after the user has pressed Finish on the wizard; the enablement of the
   * Finish button implies that all controls on the pages currently contain valid values.
   * </p>
   * <p>
   * Note that this wizard caches the new project once it has been successfully created; subsequent invocations of this
   * method will answer the same project resource without attempting to create it again.
   * </p>
   * 
   * @return the created project resource, or <code>null</code> if the project was not created
   */
  private IProject createNewProject( )
  {
    if( m_newProject != null )
      return m_newProject;

    // get a project handle
    final IProject newProjectHandle = m_mainPage.getProjectHandle();

    // get a project descriptor
    IPath newPath = null;
    if( !m_mainPage.useDefaults() )
      newPath = m_mainPage.getLocationPath();

    IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IProjectDescription description;
    if( m_description == null )
      description = workspace.newProjectDescription( newProjectHandle.getName() );
    else
      description = m_description;
    description.setLocation( newPath );

    // set nature
    final String[] natures = description.getNatureIds();
    final String[] newNatures = new String[natures.length + 1];
    System.arraycopy( natures, 0, newNatures, 0, natures.length );
    if( m_natureID == null )
      newNatures[natures.length] = m_defaultNature;
    else
      newNatures[natures.length] = m_natureID;
    description.setNatureIds( newNatures );

    // update the referenced project if provided
    if( m_referencePage != null )
    {
      IProject[] refProjects = m_referencePage.getReferencedProjects();
      if( refProjects.length > 0 )
        description.setReferencedProjects( refProjects );
    }

    // create the new project operation
    WorkspaceModifyOperation op = new WorkspaceModifyOperation()
    {
      @Override
      protected void execute( IProgressMonitor monitor ) throws CoreException
      {
        createProject( description, newProjectHandle, monitor );
      }
    };

    // run the new project creation operation
    try
    {
      getContainer().run( true, true, op );
    }
    catch( InterruptedException e )
    {
      return null;
    }
    catch( InvocationTargetException e )
    {
      // ie.- one of the steps resulted in a core exception
      Throwable t = e.getTargetException();
      if( t instanceof CoreException )
      {
        if( ((CoreException) t).getStatus().getCode() == IResourceStatus.CASE_VARIANT_EXISTS )
        {
          // The underlying file system is case insensitive. There is an existing project which conflicts with ''{0}''.
          MessageDialog.openError( getShell(), "Probleme beim erstellen des neuen Projektes", NLS.bind( "Das Dateisystem ist Case Sensitive. Es existiert ein Projekt welches mit ''{0}'' im Konflikt liegt. ", newProjectHandle.getName() ) );
        }
        else
        {
          ErrorDialog.openError( getShell(), "Probleme beim erstellen des neuen Projektes", null, // no special message
              ((CoreException) t).getStatus() );
        }
      }
      else
      {
        // CoreExceptions are handled above, but unexpected runtime
        // exceptions and errors may still occur.
        KalypsoPortalPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoPortalPlugin.getID(), 0, t.toString(), t ) );
        MessageDialog.openError( getShell(), "Probleme beim erstellen des neuen Projektes", NLS.bind( "Internal error: {0}", t.getMessage() ) );
      }
      return null;
    }

    m_newProject = newProjectHandle;

    return m_newProject;
  }

  /**
   * Creates a project resource given the project handle and description.
   * 
   * @param description
   *          the project description to create a project resource for
   * @param projectHandle
   *          the project handle to create a project resource for
   * @param monitor
   *          the progress monitor to show visual progress with
   * @exception CoreException
   *              if the operation fails
   * @exception OperationCanceledException
   *              if the operation is canceled
   */
  void createProject( IProjectDescription description, IProject projectHandle, IProgressMonitor monitor ) throws CoreException, OperationCanceledException
  {
    try
    {
      monitor.beginTask( "", 2000 );//$NON-NLS-1$

      projectHandle.create( description, new SubProgressMonitor( monitor, 1000 ) );

      if( monitor.isCanceled() )
        throw new OperationCanceledException();

      projectHandle.open( IResource.BACKGROUND_REFRESH, new SubProgressMonitor( monitor, 1000 ) );

    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * Returns the newly created project.
   * 
   * @return the created project, or <code>null</code> if project not created
   */
  public IProject getNewProject( )
  {
    return m_newProject;
  }

}
