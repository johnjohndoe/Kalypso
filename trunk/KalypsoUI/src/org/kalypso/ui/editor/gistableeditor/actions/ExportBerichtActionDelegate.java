package org.kalypso.ui.editor.gistableeditor.actions;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.kalypso.ogc.gml.table.wizard.ExportableLayerTable;
import org.kalypso.services.proxy.DocBean;
import org.kalypso.ui.metadoc.table.ExportTableBerichtWizard;
import org.kalypso.ui.metadoc.util.MetadocServiceWrapper;

/**
 * @author Belger
 */
public class ExportBerichtActionDelegate extends GisTableAbstractActionDelagate
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    try
    {
      final String username = System.getProperty( "user.name" );
      final MetadocServiceWrapper service = new MetadocServiceWrapper( ".csv", username );
      final DocBean doc = service.getDoc();
      
      final ExportableLayerTable exp = new ExportableLayerTable( getEditor().getLayerTable() );
      
      final Wizard exportWizard = new ExportTableBerichtWizard( exp , doc );

      final WizardDialog dialog = new WizardDialog( getEditor().getSite().getShell(), exportWizard );
      final int ok = dialog.open();

      final Job job = new Job( "Berichtsablage" ) {
        protected IStatus run( IProgressMonitor monitor )
        { 
          try
          {
            if( ok == Window.OK )
              service.commitData();
            else
              service.cancelData();
          }
          catch( CoreException e )
          {
            return e.getStatus();
          }
          
          return Status.OK_STATUS;
        }};
        job.setUser( true );
        job.schedule();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      ErrorDialog.openError( getEditor().getSite().getShell(), "Berichtsablage",
          "Berichtsablagedienst konnte nicht initialisiert werden", e.getStatus() );
    }
  }

  /**
   * @see org.kalypso.ui.editor.gistableeditor.actions.GisTableAbstractActionDelagate#refreshAction()
   */
  protected void refreshAction()
  {
    // nix tun, immer Aktiv
  }
}