package org.kalypso.kalypsomodel1d2d.ui;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsosimulationmodel.core.ISimulationModelProvider;

public class ImportRoughnessHandler extends AbstractHandler
{
  private static final String ROUGHNESS_IMPORT_WIZARD_ID = "org.kalypso.ui.shapeImportWizards.utils.importRoughness.ImportWizard";

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {

    ISimulationModelProvider provider = (ISimulationModelProvider) event.getApplicationContext();
    IStatus status;
    try
    {
      status = executeInternal( provider );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
      status = StatusUtilities.statusFromThrowable( e );
    }

    if( !status.isOK() )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      // ErrorDialog.openError( provider.getShel(), event.getCommand().getName(), status.getMessage(), status );
    }

    return status;
  }

  protected IStatus executeInternal( ISimulationModelProvider provider ) throws CoreException
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IWizardDescriptor wizardDescriptor = workbench.getNewWizardRegistry().findWizard( ROUGHNESS_IMPORT_WIZARD_ID );
    final INewWizard wizard = (INewWizard) wizardDescriptor.createWizard();
    final WizardDialog wizardDialog = new WizardDialog( null, wizard );
    wizard.init( workbench, provider.getSelection() );
    if( wizardDialog.open() != Window.OK )
    {
      return Status.CANCEL_STATUS;
    }
    else
    {
      return Status.OK_STATUS;
    }

  }
}
