package org.kalypso.ui.navigator;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagramTemplateUtils;
import org.kalypso.ogc.sensor.diagview.grafik.GrafikLauncher;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Opens the Grafik tool. Can operate on observation template and grafik
 * template files.
 * 
 * @author schlienger
 */
public class GrafikViewActionDelegate implements IViewActionDelegate
{
  private IFile m_currentFile = null;

  private IViewPart m_view;

  public GrafikViewActionDelegate( )
  {
    // empty
  }

  /**
   * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
   */
  public void init( final IViewPart view )
  {
    m_view = view;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final IFile currentFile = m_currentFile;
    if( currentFile == null )
      return;

    final WorkspaceModifyOperation operation = new WorkspaceModifyOperation(
        null )
    {
      protected void execute( IProgressMonitor monitor ) throws CoreException
      {
        monitor.beginTask( "Grafik öffnen", IProgressMonitor.UNKNOWN );
        try
        {
          if( currentFile.getFileExtension().equalsIgnoreCase(
              DiagramTemplateUtils.ODT_FILE_EXTENSION ) )
          {
            final IContainer parent = currentFile.getParent();

            final IFolder folder = parent.getFolder( new Path( "grafik" ) );
            if( !folder.exists() )
              folder.create( true, true, new NullProgressMonitor() );

            GrafikLauncher.startGrafikODT( currentFile, folder, monitor );
          }
          else if( currentFile.getFileExtension().equalsIgnoreCase(
              GrafikLauncher.TPL_FILE_EXTENSION ) )
          {
            GrafikLauncher.startGrafikTPL( currentFile );
          }
        }
        catch( SensorException e )
        {
          e.printStackTrace();
          throw new CoreException( KalypsoGisPlugin.createErrorStatus(
              "Grafik konnte nicht gestartet werden", e ) );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    try
    {
      PlatformUI.getWorkbench().getProgressService()
          .busyCursorWhile( operation );
    }
    catch( Exception e )
    {
      MessageDialog.openError( m_view.getSite().getShell(),
          "Grafik konnte nicht gestartet werden", e.getLocalizedMessage() );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    final IStructuredSelection sel = (IStructuredSelection) selection;

    if( sel.getFirstElement() instanceof IFile )
      m_currentFile = (IFile) sel.getFirstElement();
    else
      m_currentFile = null;
  }
}