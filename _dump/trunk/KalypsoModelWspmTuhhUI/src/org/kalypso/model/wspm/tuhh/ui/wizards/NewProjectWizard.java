package org.kalypso.model.wspm.tuhh.ui.wizards;

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
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;
import org.eclipse.ui.wizards.newresource.BasicNewResourceWizard;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhHelper;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIImages;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;

public class NewProjectWizard extends Wizard implements INewWizard, IExecutableExtension
{
  private static final String STR_WINDOW_TITLE = "Neues Projekt - Spiegellinienberechnung";

  private WizardNewProjectCreationPage m_createProjectPage;

  private IWorkbench m_workbench;

  private IConfigurationElement m_config;

  public NewProjectWizard( )
  {
    setWindowTitle( STR_WINDOW_TITLE );
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

    super.addPages();
  }

  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_workbench = workbench;
  }

  @Override
  public boolean performFinish( )
  {
    final IProject project = m_createProjectPage.getProjectHandle();

    final WorkspaceModifyOperation op = new WorkspaceModifyOperation()
    {
      @Override
      protected void execute( final IProgressMonitor monitor ) throws CoreException
      {
        monitor.beginTask( "Projekt wird erzeugt", 4 );

        project.create( new SubProgressMonitor( monitor, 1 ) );
        project.open( new SubProgressMonitor( monitor, 1 ) );

        final IProjectDescription description = project.getDescription();
        final String[] natures = { "org.kalypso.simulation.ui.ModelNature" }; //$NON-NLS-1$
        description.setNatureIds( natures );
        project.setDescription( description, new SubProgressMonitor( monitor, 1 ) );

        TuhhHelper.ensureValidWspmTuhhStructure( project, new SubProgressMonitor( monitor, 1 ) );
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), false, false, op );
    if( status.isOK() )
    {
      BasicNewProjectResourceWizard.updatePerspective( m_config );
      BasicNewResourceWizard.selectAndReveal( project, m_workbench.getActiveWorkbenchWindow() );
    }
    else
    {
      ErrorDialog.openError( getShell(), STR_WINDOW_TITLE, "Fehler beim Erzeugen des Projekts", status );
      deleteProject( project );
    }

    return status.isOK();
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
      ErrorDialog.openError( getShell(), getWindowTitle(), "Das zwischenzeitlich erzeugte Projekt konnte nicht gel�scht werden, ist aber vermutlich nicht korrekt initialisiert worden.\nBitte l�schen Sie das Projekt im Navigator per Hand.", status );
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
}
