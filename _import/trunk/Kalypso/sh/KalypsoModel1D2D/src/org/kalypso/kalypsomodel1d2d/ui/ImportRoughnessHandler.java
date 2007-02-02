package org.kalypso.kalypsomodel1d2d.ui;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.wizards.IWizardDescriptor;

public class ImportRoughnessHandler extends AbstractHandler
{
  private static final String ROUGHNESS_IMPORT_WIZARD_ID = "org.kalypso.ui.shapeImportWizards.utils.importRoughness.ImportWizard";

  private static final Logger logger = Logger.getLogger( ImportRoughnessHandler.class );

  static
  {
    BasicConfigurator.configure();
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IWizardDescriptor wizardDescriptor = workbench.getNewWizardRegistry().findWizard( ROUGHNESS_IMPORT_WIZARD_ID );
    try
    {
      logger.debug( "parameters for roughness wizard: " + event.getParameters() );
      final IWorkbenchWizard wizard = wizardDescriptor.createWizard();
      final WizardDialog wizardDialog = new WizardDialog( null, wizard );
      wizardDialog.open();
      return wizardDialog.getReturnCode();
    }
    catch( final CoreException e )
    {
      logger.error( "could not create roughness wizard", e );
    }
    return null;
  }
}
