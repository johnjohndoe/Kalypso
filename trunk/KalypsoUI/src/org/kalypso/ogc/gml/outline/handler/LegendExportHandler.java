/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.outline.handler;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * This handler exports the legend of the selected layers in the map outline.
 * 
 * @author Holger Albert
 */
public class LegendExportHandler extends AbstractHandler
{
  /**
   * The last directory, used in this dialog.
   */
  private static final String SETTINGS_LAST_DIR = "lastDir"; //$NON-NLS-1$

  /**
   * The constructor.
   */
  public LegendExportHandler( )
  {
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  public Object execute( ExecutionEvent event ) throws ExecutionException
  {
    /* Get the context. */
    IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    /* Get the active workbench part. */
    IWorkbenchPart part = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );
    if( part == null )
      throw new ExecutionException( Messages.getString("org.kalypso.ogc.gml.outline.handler.LegendExportHandler.1") ); //$NON-NLS-1$

    /* Need a shell. */
    final Shell shell = part.getSite().getShell();
    String title = Messages.getString("org.kalypso.ogc.gml.outline.handler.LegendExportHandler.2"); //$NON-NLS-1$

    /* Get the selected elements. */
    IStructuredSelection sel = (IStructuredSelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );
    if( sel.isEmpty() )
    {
      MessageDialog.openWarning( shell, title, Messages.getString("org.kalypso.ogc.gml.outline.handler.LegendExportHandler.3") ); //$NON-NLS-1$
      return Status.CANCEL_STATUS;
    }

    /* Ask user for file */
    IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoGisPlugin.getDefault(), "gmlExport" ); //$NON-NLS-1$
    String lastDirPath = dialogSettings.get( SETTINGS_LAST_DIR );
    FileDialog fileDialog = new FileDialog( shell, SWT.SAVE );
    fileDialog.setFilterExtensions( new String[] { "*.png", "*.jpg", "*.gif" } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    fileDialog.setFilterNames( new String[] { Messages.getString("org.kalypso.ogc.gml.outline.handler.LegendExportHandler.8"), Messages.getString("org.kalypso.ogc.gml.outline.handler.LegendExportHandler.9"), Messages.getString("org.kalypso.ogc.gml.outline.handler.LegendExportHandler.10") } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    fileDialog.setText( title );
    fileDialog.setFileName( "Legend" ); //$NON-NLS-1$
    if( lastDirPath != null )
      fileDialog.setFilterPath( lastDirPath );

    /* Open the file dialog. */
    String result = fileDialog.open();
    if( result == null )
      return Status.CANCEL_STATUS;

    /* Create the file handle for export. */
    final File legendFile = new File( result );
    if( legendFile.exists() )
    {
      boolean okPressed = MessageDialog.openConfirm( shell, Messages.getString("org.kalypso.ogc.gml.outline.handler.LegendExportHandler.12"), Messages.getString("org.kalypso.ogc.gml.outline.handler.LegendExportHandler.13") + legendFile.getName() + Messages.getString("org.kalypso.ogc.gml.outline.handler.LegendExportHandler.14") ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      if( !okPressed )
        return Status.CANCEL_STATUS;
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
          MessageDialog errorDialog = new MessageDialog( shell, Messages.getString("org.kalypso.ogc.gml.outline.handler.LegendExportHandler.18"), null, status.getMessage(), MessageDialog.ERROR, new String[] { "Ok" }, 0 ); //$NON-NLS-1$ //$NON-NLS-2$
          errorDialog.open();
        }
      }
    } );

    return Status.OK_STATUS;
  }
}