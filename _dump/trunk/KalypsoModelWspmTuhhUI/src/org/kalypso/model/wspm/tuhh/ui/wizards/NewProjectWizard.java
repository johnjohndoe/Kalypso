package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.lang.reflect.InvocationTargetException;

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
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
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
import org.kalypso.model.wspm.tuhh.core.gml.TuhhHelper;
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

    private DoFinishOperation( IProject project )
    {
      super();
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

  public NewProjectWizard( )
  {
    setWindowTitle( STR_WINDOW_TITLE );
    setNeedsProgressMonitor( true );

    final IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), getClass().getName() );
    setDialogSettings( dialogSettings );
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

    addPage( m_createProjectPage );
  }

  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_workbench = workbench;
    m_selection = selection;
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

    final IStatus status = RunnableContextHelper.execute( getContainer(), false, false, op );
    if( status.isOK() )
    {
      BasicNewProjectResourceWizard.updatePerspective( m_config );
      BasicNewResourceWizard.selectAndReveal( project, m_workbench.getActiveWorkbenchWindow() );

      openTreeView( op.getFile() );
    }
    else
    {
      ErrorDialog.openError( getShell(), STR_WINDOW_TITLE, "Fehler beim Erzeugen des Projekts", status );
      deleteProject( project );
    }

    return status.isOK();
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

      final IStatus status = RunnableContextHelper.execute( getContainer(), false, false, runnable );
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
    monitor.beginTask( "Projekt wird erzeugt", 4 );

    project.create( new SubProgressMonitor( monitor, 1 ) );
    project.open( new SubProgressMonitor( monitor, 1 ) );

    final IProjectDescription description = project.getDescription();
    final String[] natures = { "org.kalypso.simulation.ui.ModelNature" }; //$NON-NLS-1$
    description.setNatureIds( natures );
    project.setDescription( description, new SubProgressMonitor( monitor, 1 ) );

    return TuhhHelper.ensureValidWspmTuhhStructure( project, new SubProgressMonitor( monitor, 1 ) );
  }
}
