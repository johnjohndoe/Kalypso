package org.kalypso.ui.actions;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate2;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Action for exporting legends from themes.
 * 
 * @author Holger Albert
 */
public class ExportLegendAction implements IObjectActionDelegate, IActionDelegate2
{
  private IAction m_action;

  private ISelection m_selection;

  private static final String SETTINGS_LAST_DIR = "lastDir"; //$NON-NLS-1$

  /**
   * The constructor.
   */
  public ExportLegendAction( )
  {
  }

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  public void setActivePart( IAction action, IWorkbenchPart targetPart )
  {
    m_action = action;

    updateAction();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    throw new UnsupportedOperationException( Messages.getString("org.kalypso.ui.actions.ExportLegendAction.1") ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    m_action = action;
    m_selection = selection;

    updateAction();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate2#dispose()
   */
  public void dispose( )
  {
  }

  /**
   * @see org.eclipse.ui.IActionDelegate2#init(org.eclipse.jface.action.IAction)
   */
  public void init( IAction action )
  {
    m_action = action;

    updateAction();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate2#runWithEvent(org.eclipse.jface.action.IAction, org.eclipse.swt.widgets.Event)
   */
  public void runWithEvent( IAction action, Event event )
  {
    /* Need a shell. */
    final Shell shell = event.display.getActiveShell();
    String title = Messages.getString("org.kalypso.ui.actions.ExportLegendAction.2"); //$NON-NLS-1$

    /* Get the selected elements. */
    IStructuredSelection sel = (IStructuredSelection) m_selection;
    if( sel.isEmpty() )
    {
      MessageDialog.openWarning( shell, title, Messages.getString("org.kalypso.ui.actions.ExportLegendAction.3") ); //$NON-NLS-1$
      return;
    }

    /* Ask user for file */
    IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoGisPlugin.getDefault(), "gmlExport" ); //$NON-NLS-1$
    String lastDirPath = dialogSettings.get( SETTINGS_LAST_DIR );
    FileDialog fileDialog = new FileDialog( shell, SWT.SAVE );
    fileDialog.setFilterExtensions( new String[] { "*.png", "*.jpg", "*.gif" } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    fileDialog.setFilterNames( new String[] { Messages.getString("org.kalypso.ui.actions.ExportLegendAction.8"), Messages.getString("org.kalypso.ui.actions.ExportLegendAction.9"), Messages.getString("org.kalypso.ui.actions.ExportLegendAction.10") } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    fileDialog.setText( title );
    fileDialog.setFileName( Messages.getString("org.kalypso.ui.actions.ExportLegendAction.11") ); //$NON-NLS-1$
    if( lastDirPath != null )
      fileDialog.setFilterPath( lastDirPath );

    /* Open the file dialog. */
    String result = fileDialog.open();
    if( result == null )
      return;

    /* Create the file handle for export. */
    final File legendFile = new File( result );
    if( legendFile.exists() )
    {
      boolean okPressed = MessageDialog.openConfirm( shell, Messages.getString("org.kalypso.ui.actions.ExportLegendAction.12"), Messages.getString("org.kalypso.ui.actions.ExportLegendAction.13") + legendFile.getName() + Messages.getString("org.kalypso.ui.actions.ExportLegendAction.14") ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      if( !okPressed )
        return;
    }

    /* Store it in the dialog settings. */
    dialogSettings.put( SETTINGS_LAST_DIR, legendFile.getParent() );

    /* Memory for the themes. */
    final ArrayList<IKalypsoTheme> themes = new ArrayList<IKalypsoTheme>();

    /* Get the iterator. */
    Iterator< ? > iterator = sel.iterator();

    /* Collect them. */
    while( iterator.hasNext() )
    {
      Object object = iterator.next();
      if( object instanceof IKalypsoTheme )
        themes.add( (IKalypsoTheme) object );
    }

    /* Create the export job. */
    shell.getDisplay().asyncExec( new Runnable()
    {
      /**
       * @see java.lang.Runnable#run()
       */
      public void run( )
      {
        /* Now save it to a file. */
        String suffix = FileUtilities.getSuffix( legendFile );

        int format = SWT.IMAGE_PNG;
        if( "PNG".equals( suffix ) ) //$NON-NLS-1$
          format = SWT.IMAGE_PNG;
        else if( "JPG".equals( suffix ) ) //$NON-NLS-1$
          format = SWT.IMAGE_JPEG;
        else if( "GIF".equals( suffix ) ) //$NON-NLS-1$
          format = SWT.IMAGE_GIF;

        /* Export the legends. */
        IStatus status = MapUtilities.exportLegends( themes, legendFile, format, new NullProgressMonitor() );
        if( !status.isOK() )
        {
          /* Log the error. */
          KalypsoGisPlugin.getDefault().getLog().log( status );

          /* Open a error dialog. */
          MessageDialog errorDialog = new MessageDialog( shell, Messages.getString("org.kalypso.ui.actions.ExportLegendAction.18"), null, status.getMessage(), MessageDialog.ERROR, new String[] { Messages.getString("org.kalypso.ui.actions.ExportLegendAction.19") }, 0 ); //$NON-NLS-1$ //$NON-NLS-2$
          errorDialog.open();
        }
      }
    } );
  }

  /**
   * Updates the action.
   */
  private void updateAction( )
  {
    /* Here we could disable the action if no data is available, listening to the theme would be necessary then. */
    if( m_action != null )
      m_action.setEnabled( true );
  }
}