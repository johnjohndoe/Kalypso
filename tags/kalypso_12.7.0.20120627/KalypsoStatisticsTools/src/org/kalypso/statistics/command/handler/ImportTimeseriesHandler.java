package org.kalypso.statistics.command.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.kalypso.statistics.gui.wizards.importTimeseriesExternal.ImportObsWizard;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.utils.AppUtils;

public class ImportTimeseriesHandler extends AbstractHandler implements IHandler
{

  public static final String ID = ImportTimeseriesHandler.class.getCanonicalName();

  @Override
  public Object execute( final ExecutionEvent event )
  {
    Shell activeShell = Display.getDefault().getActiveShell();
    if( activeShell == null )
    {
      activeShell = new Shell( Display.getDefault() );
    }
    if( SessionDataProvider.getInstance().getDataProvider().getNodes().size() == 0 )
    {
      MessageDialog.openInformation( Display.getDefault().getActiveShell(), AppUtils.APPLICATION_TITLE, "No Nodes and/or stations are defined yet. Please define at least one node or station." );
      return Status.OK_STATUS;
    }

    final IWizardDescriptor wizardDesc = PlatformUI.getWorkbench().getNewWizardRegistry().findWizard( ImportObsWizard.ID );
    try
    {
      final IWorkbenchWizard wizard = wizardDesc.createWizard();
      wizard.init( PlatformUI.getWorkbench(), new StructuredSelection( Platform.getLocation() ) );
      final WizardDialog dialog = new WizardDialog( activeShell, wizard );
      dialog.open();
    }
    catch( final CoreException e1 )
    {
      final IStatus status = e1.getStatus();
      ErrorDialog.openError( activeShell, AppUtils.APPLICATION_TITLE, e1.getMessage(), status );
    }
    // treeViewer.refresh();

    return Status.OK_STATUS;
  }

}
