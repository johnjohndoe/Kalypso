package org.kalypso.ui.actions;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
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
import org.eclipse.ui.progress.UIJob;
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
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
    m_action = action;

    updateAction();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    throw new UnsupportedOperationException( Messages.getString( "org.kalypso.ui.actions.ExportLegendAction.1" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
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
  public void init( final IAction action )
  {
    m_action = action;

    updateAction();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate2#runWithEvent(org.eclipse.jface.action.IAction, org.eclipse.swt.widgets.Event)
   */
  public void runWithEvent( final IAction action, final Event event )
  {
    /* Need a shell. */
    final Shell shell = event.display.getActiveShell();
    final String title = Messages.getString( "org.kalypso.ui.actions.ExportLegendAction.2" ); //$NON-NLS-1$

    /* Get the selected elements. */
    final IStructuredSelection sel = (IStructuredSelection) m_selection;
    if( sel.isEmpty() )
    {
      MessageDialog.openWarning( shell, title, Messages.getString( "org.kalypso.ui.actions.ExportLegendAction.3" ) ); //$NON-NLS-1$
      return;
    }

    // TODO: copy/paste from org.kalypso.ui.actions.ExportLegendAction
    // Put into utility method! (don't forget to remove obsolete message-string)

    /* Ask user for file */
    final IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoGisPlugin.getDefault(), "gmlExport" ); //$NON-NLS-1$
    final String lastDirPath = dialogSettings.get( SETTINGS_LAST_DIR );
    final FileDialog fileDialog = new FileDialog( shell, SWT.SAVE );
    fileDialog.setFilterExtensions( new String[] { "*.png", "*.jpg", "*.gif" } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    fileDialog.setFilterNames( new String[] {
        Messages.getString( "org.kalypso.ui.actions.ExportLegendAction.8" ), Messages.getString( "org.kalypso.ui.actions.ExportLegendAction.9" ), Messages.getString( "org.kalypso.ui.actions.ExportLegendAction.10" ) } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    fileDialog.setText( title );
    // TODO: use map-name for default name e.g. 'mapxxx-legend.png')
    fileDialog.setFileName( Messages.getString( "org.kalypso.ui.actions.ExportLegendAction.11" ) ); //$NON-NLS-1$
    if( lastDirPath != null )
      fileDialog.setFilterPath( lastDirPath );

    /* Open the file dialog. */
    final String result = fileDialog.open();
    if( result == null )
      return;

    /* Create the file handle for export. */
    final File legendFile = new File( result );
    if( legendFile.exists() )
    {
      final boolean okPressed = MessageDialog.openConfirm( shell, Messages.getString( "org.kalypso.ui.actions.ExportLegendAction.12" ), Messages.getString( "org.kalypso.ui.actions.ExportLegendAction.13" ) + legendFile.getName() + Messages.getString( "org.kalypso.ui.actions.ExportLegendAction.14" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      if( !okPressed )
        return;
    }

    /* Store it in the dialog settings. */
    dialogSettings.put( SETTINGS_LAST_DIR, legendFile.getParent() );

    /* Collect all themes */
    final List<IKalypsoTheme> themeList = new ArrayList<IKalypsoTheme>();
    final Iterator< ? > iterator = sel.iterator();
    while( iterator.hasNext() )
    {
      final Object object = iterator.next();
      if( object instanceof IKalypsoTheme )
        themeList.add( (IKalypsoTheme) object );
    }
    final IKalypsoTheme[] themes = themeList.toArray( new IKalypsoTheme[themeList.size()] );

    /* Create the export job. */
    final Job job = new UIJob( "Export" )
    {
      /**
       * @see org.eclipse.ui.progress.UIJob#runInUIThread(org.eclipse.core.runtime.IProgressMonitor)
       */
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        /* Now save it to a file. */
        final String suffix = FileUtilities.getSuffix( legendFile );

        int format = SWT.IMAGE_PNG;
        if( "PNG".equals( suffix ) ) //$NON-NLS-1$
          format = SWT.IMAGE_PNG;
        else if( "JPG".equals( suffix ) ) //$NON-NLS-1$
          format = SWT.IMAGE_JPEG;
        else if( "GIF".equals( suffix ) ) //$NON-NLS-1$
          format = SWT.IMAGE_GIF;

        /* Export the legends. */
        return MapUtilities.exportLegends( themes, legendFile, format, -1, -1, monitor );
      }
    };
    job.setUser( true );
    job.schedule();
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