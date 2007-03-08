package org.kalypso.kalypso1d2d.pjt.actions;

import java.util.HashMap;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.ui.wizards.imports.INewWizardKalypsoImport;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * Starts the import observation wizard
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportObservationHandler extends WorkflowCommandHandler
{
  private static final String WIZARD_ID = "org.kalypso.ui.wizards.imports.observation.ImportObservationWizard";

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  protected IStatus executeInternal( final ExecutionEvent event ) throws CoreException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    IStructuredSelection selection = (IStructuredSelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );
    if( selection == null )
    {
      final IResource currentFolder = (IFolder) context.getVariable( "activeSimulationModelBaseFolder" );
      selection = new StructuredSelection( currentFolder );
    }
    final IWorkbenchWindow workbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbench workbench = (workbenchWindow).getWorkbench();
//    final IFolder szenarioPath = (IFolder) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME );

    final IWizardDescriptor wizardDescriptor = workbench.getNewWizardRegistry().findWizard( WIZARD_ID );
    final INewWizardKalypsoImport wizard = (INewWizardKalypsoImport) wizardDescriptor.createWizard();
    final WizardDialog wizardDialog = new WizardDialog( workbenchWindow.getShell(), wizard );
    final HashMap<String, Object> data = new HashMap<String, Object>();
    final IFolder currentFolder = (IFolder) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME );
    data.put( "Project", currentFolder.getProject() );

//    final IFolder currentFolder = (IFolder) context.getVariable( "activeSimulationModelBaseFolder" );

    wizard.init( workbench, selection );
    wizard.initModelProperties( data );
    if( wizardDialog.open() == Window.OK )
    {
//      currentFolder.getProject().refreshLocal( IResource.DEPTH_INFINITE, null );

//      final IFile file = szenarioPath.getFile( "maps/base.gmt" );
//      if( file.exists() )
//      {
//        final IWorkbenchPage workbenchPage = workbenchWindow.getActivePage();
//        MapView view = (MapView) workbenchPage.showView( MapView.ID );
//        view.startLoadJob( file );
//      }
      return Status.OK_STATUS;
    }
    return Status.CANCEL_STATUS;

  }
}
