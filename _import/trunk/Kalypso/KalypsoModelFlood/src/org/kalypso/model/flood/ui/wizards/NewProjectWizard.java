package org.kalypso.model.flood.ui.wizards;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

public class NewProjectWizard extends BasicNewProjectResourceWizard
{
  @Override
  /**
   * This method was overriden in order to get rid of the 'select dependend projects' page from the
   * BasicNewProjectResourceWizard.
   */
  public IWizardPage getNextPage( final IWizardPage page )
  {
    // HACK: to do so, we just skip this particular page
    // Unfortunateley we cannot just overide 'addPages' and do not add the secod page,
    // because the BasicNewProjectResourceWizard relies on the second page to exist.
    final IWizardPage[] pages = getPages();

    if( page.equals( pages[0] ) )
      return null;

    return super.getNextPage( page );
  }

  @Override
  public boolean performFinish( )
  {
    boolean result = super.performFinish();

    final String MSG = "Message"; //$NON-NLS-1$

    if( !result )
      return false;

    try
    {
      final IProject project = getNewProject();
      // important: add ScenarioHandlingProjectNature before KalypsoRiscNature

      // Unpack project from template

      // configure all natures of this project

      // ProjectUtilities.addNature( project, ScenarioHandlingProjectNature.ID, new NullProgressMonitor() );
      // ProjectUtilities.addNature( project, WorkflowProjectNature.ID, new NullProgressMonitor() );
      // KalypsoRiskProjectNature.addNature( project );
    }
    catch( final Throwable th )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( th );
      KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( status );
      ErrorDialog.openError( getShell(), "Projekt Neu", "Fehler beim Erzeugen des Projekts", status );

      // TODO: remove previously created project on error

      return false;
    }

    return true;
  }
}
