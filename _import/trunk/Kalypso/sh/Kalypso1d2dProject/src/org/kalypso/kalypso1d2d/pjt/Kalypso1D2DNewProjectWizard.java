package org.kalypso.kalypso1d2d.pjt;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.ScenarioHandlingProjectNature;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.perspective.Perspective;

import de.renew.workflow.connector.WorkflowProjectNature;
import de.renew.workflow.contexts.IDialogWithResult;

/**
 * Creates a new Kalypso 1d 2d Project
 * 
 * @author Patrice Congo
 */
public class Kalypso1D2DNewProjectWizard extends BasicNewProjectResourceWizard implements IDialogWithResult
{
  final static public String ID = "org.kalypso.kalypso1d2d.pjt.Kalypso1D2DNewProjectWizard";  //$NON-NLS-1$

  final static private Logger logger = Logger.getLogger( Kalypso1D2DNewProjectWizard.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );  //$NON-NLS-1$

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  public Kalypso1D2DNewProjectWizard( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
    try
    {
      workbench.showPerspective( Perspective.ID, activeWorkbenchWindow );
    }
    catch( final WorkbenchException e )
    {
    }
  }

  @Override
  /**
   * This method was overriden in order to get rid of the 'select dependend projects' page from the
   * BasicNewProjectResourceWizard.
   */
  public IWizardPage getNextPage( IWizardPage page )
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
    final String MSG = Messages.getString("Kalypso1D2DNewProjectWizard.2"); //$NON-NLS-1$

    if( !result )
    {
      return false;
    }
    else
    {
      IProject project = getNewProject();
      try
      {
        //important: add ScenarioHandlingProjectNature before Kalypso1D2DProjectNature
        WorkflowProjectNature.addNature(project);
        ScenarioHandlingProjectNature.addNature( project );
        Kalypso1D2DProjectNature.addNature( project );

        /* Also activate new project */
        final ScenarioHandlingProjectNature nature = ScenarioHandlingProjectNature.toThisNature( project );
        final Scenario caze = nature.getCaseManager().getCases().get( 0 );
        KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext().setCurrentCase( caze );
      }
      catch( CoreException e )
      {
        logger.log( Level.INFO, MSG, e );
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        ErrorDialog.openError( getShell(), Messages.getString("Kalypso1D2DNewProjectWizard.3"), Messages.getString("Kalypso1D2DNewProjectWizard.4"), status ); //$NON-NLS-1$ //$NON-NLS-2$
        return false;
      }
      catch( Throwable th )
      {
        logger.log( Level.SEVERE, MSG, th );
        return false;
      }
      return true;
    }
  }

  /**
   * @see de.renew.workflow.contexts.IDialogWithResult#getResult()
   */
  public Object getResult( )
  {
    return getNewProject().getName();
  }
}
