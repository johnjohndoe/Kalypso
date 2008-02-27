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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IPageChangingListener;
import org.eclipse.jface.dialogs.PageChangingEvent;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;
import org.eclipse.ui.wizards.newresource.BasicNewResourceWizard;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCoreExtensions;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhHelper;
import org.kalypso.model.wspm.tuhh.core.util.TuhhDemoProject;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIImages;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.ui.editorLauncher.GmlEditorTemplateLauncher;

/**
 * Wizard to create a new wspm tuhh project.
 * <p>
 * Overwrite for special purposes, like create new project and import data from an existing datasource.
 * </p>
 * <p>
 * If overwritten, also {@link #doFinish(IProject, IProgressMonitor)} should be overwritten
 * </p>
 * 
 * @author Gernot Belger
 */
public class NewProjectWizard extends Wizard implements INewWizard, IExecutableExtension
{
  private final class DoFinishOperation extends WorkspaceModifyOperation
  {
    private final IProject m_project;

    private IFile m_file;

    protected DoFinishOperation( final IProject project )
    {
      m_project = project;
    }

    public IFile getFile( )
    {
      return m_file;
    }

    @Override
    protected void execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
    {
      m_file = doFinish( m_project, monitor );
    }
  }

  private static final String STR_WINDOW_TITLE = "Neues Projekt - Spiegellinienberechnung";

  private WizardNewProjectCreationPage m_createProjectPage;

  private IWorkbench m_workbench;

  private IConfigurationElement m_config;

  private IStructuredSelection m_selection;

  private final IPageChangingListener m_pageChangeingListener = new IPageChangingListener()
  {
    public void handlePageChanging( PageChangingEvent event )
    {
      doHandlePageChangeing( event );
    }
  };

  private final DemoProjectsWizardPage m_demoProjectPage;

  public NewProjectWizard( )
  {
    this( false );
  }

  public NewProjectWizard( final boolean showDemoPage )
  {
    setWindowTitle( STR_WINDOW_TITLE );
    setNeedsProgressMonitor( true );

    final IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), getClass().getName() );
    setDialogSettings( dialogSettings );

    if( showDemoPage )
      m_demoProjectPage = new DemoProjectsWizardPage( "demoProjectPage", KalypsoModelWspmTuhhCoreExtensions.getDemoProjects() );
    else
      m_demoProjectPage = null;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    m_createProjectPage = new WizardNewProjectCreationPage( "newProjectPage" );
    m_createProjectPage.setTitle( "Neues Spiegellinienberechnung-Projekt erzeugen" );
    m_createProjectPage.setDescription( "Dieser Dialog erstellt ein neues Spiegellinienberechnung-Projekt im Arbeitsbereich." );
    m_createProjectPage.setImageDescriptor( KalypsoModelWspmTuhhUIPlugin.getImageProvider().getImageDescriptor( KalypsoModelWspmTuhhUIImages.NEWPROJECT_PROJECT_PAGE_WIZBAN ) );

    if( m_demoProjectPage != null )
      addPage( m_demoProjectPage );

    addPage( m_createProjectPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#createPageControls(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPageControls( final Composite pageContainer )
  {
    // Overwritten in order to NOT create the pages during initalisation, so we can set
    // the project name later before the next page is created, this work however only once :-(
  }

  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_workbench = workbench;
    m_selection = selection;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#setContainer(org.eclipse.jface.wizard.IWizardContainer)
   */
  @Override
  public void setContainer( final IWizardContainer wizardContainer )
  {
    final IWizardContainer oldContainer = getContainer();
    if( oldContainer instanceof WizardDialog )
      ((WizardDialog) oldContainer).addPageChangingListener( m_pageChangeingListener );

    if( wizardContainer instanceof WizardDialog )
      ((WizardDialog) wizardContainer).addPageChangingListener( m_pageChangeingListener );

    super.setContainer( wizardContainer );
  }

  protected void doHandlePageChangeing( final PageChangingEvent event )
  {
    if( event.getCurrentPage() == m_demoProjectPage )
    {
      final TuhhDemoProject demoProject = m_demoProjectPage.getSelectedProject();

      // TODO: Does work only the first time :-( ,see above createPageControls
      m_createProjectPage.setInitialProjectName( demoProject.getProjectName() );
    }
  }

  protected IStructuredSelection getSelection( )
  {
    return m_selection;
  }

  protected IWorkbench getWorkbench( )
  {
    return m_workbench;
  }

  @Override
  public boolean performFinish( )
  {
    final IProject project = m_createProjectPage.getProjectHandle();

    final DoFinishOperation op = new DoFinishOperation( project );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( status.matches( IStatus.ERROR ) )
    {
      ErrorDialog.openError( getShell(), STR_WINDOW_TITLE, "Fehler beim Erzeugen des Projekts", status );
      KalypsoModelWspmTuhhUIPlugin.getDefault().getLog().log( status );
      deleteProject( project );
    }
    else if( status.matches( IStatus.CANCEL ) )
    {
      deleteProject( project );
    }
    else
    {
      if( !status.isOK() )
        ErrorDialog.openError( getShell(), STR_WINDOW_TITLE, "Fehler beim Erzeugen des Projekts", status );

      BasicNewProjectResourceWizard.updatePerspective( m_config );
      BasicNewResourceWizard.selectAndReveal( project, m_workbench.getActiveWorkbenchWindow() );

      openTreeView( op.getFile() );
      return true;
    }

    return false;
  }

  private void openTreeView( final IFile file )
  {
    final UIJob job = new UIJob( "Öffne Spiegellinienmodell" )
    {
      @Override
      public IStatus runInUIThread( IProgressMonitor monitor )
      {
        try
        {
          final GmlEditorTemplateLauncher launcher = new GmlEditorTemplateLauncher();
          final IEditorInput input = launcher.createInput( file );
          final IEditorDescriptor editor = launcher.getEditor();
          getWorkbench().getActiveWorkbenchWindow().getActivePage().openEditor( input, editor.getId() );

          return Status.OK_STATUS;
        }
        catch( final CoreException e )
        {
          return e.getStatus();
        }
      }
    };

    job.setUser( true );
    job.schedule();
  }

  private void deleteProject( final IProject project )
  {
    if( project != null && project.exists() )
    {
      final ICoreRunnableWithProgress runnable = new ICoreRunnableWithProgress()
      {
        public IStatus execute( IProgressMonitor monitor ) throws CoreException
        {
          project.delete( true, monitor );
          return Status.OK_STATUS;
        }
      };

      final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, runnable );
      ErrorDialog.openError( getShell(), getWindowTitle(), "Das zwischenzeitlich erzeugte Projekt konnte nicht gelöscht werden, ist aber vermutlich nicht korrekt initialisiert worden.\nBitte löschen Sie das Projekt im Navigator per Hand.", status );
    }
  }

  /**
   * Overwritten to remember configuration element.
   * 
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    m_config = config;
  }

  /**
   * Creates and configures the new project.
   * <p>
   * Overwrite, if more has to be done while finishing.
   * </p>
   */
  protected IFile doFinish( final IProject project, final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    monitor.beginTask( "Neues Spiegellinienprojekt", 4 );

    final TuhhDemoProject demoProject = m_demoProjectPage == null ? KalypsoModelWspmTuhhCoreExtensions.getEmptyProject() : m_demoProjectPage.getSelectedProject();
    final URL zipLocation = demoProject.getData();

    project.create( new SubProgressMonitor( monitor, 1 ) );
    project.open( new SubProgressMonitor( monitor, 1 ) );

    final IProjectDescription description = project.getDescription();
    final String[] natures = { "org.kalypso.simulation.ui.ModelNature" }; //$NON-NLS-1$
    description.setNatureIds( natures );
    project.setDescription( description, new SubProgressMonitor( monitor, 1 ) );

    return TuhhHelper.ensureValidWspmTuhhStructure( project, zipLocation, new SubProgressMonitor( monitor, 1 ) );
  }

}
