/**
 * 
 */
package org.kalypso.ui.model.wspm.importwizard;

import java.io.File;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IImportWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.ui.model.wspm.KalypsoUIModelWspmPlugin;
import org.kalypso.ui.model.wspm.core.wspwin.WspWinImporter;
import org.kalypso.ui.model.wspm.tuhh.WspmHelper;

/**
 * @author thuel2
 */
public class WspWinImportWizard extends Wizard implements IImportWizard
{
  private IStructuredSelection m_selection;

  private IWorkbench m_workbench;

  private WspWinImportPage m_wspWinImportPage;

  public WspWinImportWizard( )
  {
    setWindowTitle( "WspWin Daten" );  
    setNeedsProgressMonitor( true );

    final IDialogSettings pluginSettings = KalypsoUIModelWspmPlugin.getDefault().getDialogSettings();
    final IDialogSettings section = pluginSettings.getSection( "WspWinImportWizard" );//$NON-NLS-1$
    if( section != null )
      setDialogSettings( section );
    else
      setDialogSettings( pluginSettings.addNewSection( "WspWinImportWizard" ) );//$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    super.addPages();

    m_wspWinImportPage = new WspWinImportPage( m_workbench, m_selection );
    addPage( m_wspWinImportPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    m_wspWinImportPage.saveWidgetValues();

    // hole quell projekt
    final File wspwinDirectory = m_wspWinImportPage.getSourceDirectory();

    final Shell shell = getContainer().getShell();

    // hole zielprojekt
    final IContainer targetContainer = m_wspWinImportPage.getTargetContainer();
    if( targetContainer == null )
      return false;

    if( !targetContainer.exists() )
    {
      if( !MessageDialog.openConfirm( shell, "", "Verzeichnis existiert noch nicht. Soll es angelegt werden?" ) )
        return false;
    }

    final WorkspaceModifyOperation operation = new WorkspaceModifyOperation()
    {
      @Override
      protected void execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
      {
        monitor.beginTask( "WspWin Datenimport", 100 );

        try
        {
          monitor.subTask( " - Modellstruktur validieren..." + targetContainer.getName() );
          WspmHelper.ensureValidWspmTuhhStructure( targetContainer, new SubProgressMonitor( monitor, 10 ) );

          monitor.subTask( " - Datenimport" );
          final IStatus status = WspWinImporter.importProject( wspwinDirectory, targetContainer, new SubProgressMonitor( monitor, 90 ) );
          if( !status.isOK() )
            throw new CoreException( status );
        }
        catch( final CoreException rethrown )
        {
          throw rethrown;
        }
        catch( final Exception e )
        {
          throw new InvocationTargetException( e );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), false, false, operation );
    ErrorDialog.openError( shell, "WspWin Daten", "Fehler beim Datenimport", status );
    // if( !status.isOK() )
    // status.getException().printStackTrace();

    return status.isOK();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_workbench = workbench;
    m_selection = selection;
  }

}
