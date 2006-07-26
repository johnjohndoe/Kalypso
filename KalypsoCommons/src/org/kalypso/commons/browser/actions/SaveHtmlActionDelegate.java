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
package org.kalypso.commons.browser.actions;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

/**
 * Action delegate to save a view or editor as html.
 * <p>
 * Used for the browser view.
 * </p>
 * <p>
 * The view or editor this action is used for must adapt to ...
 * </p>
 * TODO: images are not yet saved with the file, but only linked. Maybe do something like Firefix and save a compressed
 * html including everything?
 * 
 * @author Gernot Belger
 */
public class SaveHtmlActionDelegate implements IViewActionDelegate, IEditorActionDelegate
{
  private static final String SETTINGS_FILE_NAME = null;

  private static final String SETTINGS_FILE_PATH = null;

  private IWorkbenchPart m_part;

  private IDialogSettings m_dialogSettings;

  public SaveHtmlActionDelegate( )
  {
    m_dialogSettings = PluginUtilities.getDialogSettings( KalypsoCommonsPlugin.getDefault(), getClass().getName() );
  }

  /**
   * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
   */
  public void init( final IViewPart view )
  {
    m_part = view;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final String html = getHtml();
    if( html == null )
      return; // should not happen because action should be disable in this case

    final Shell shell = m_part.getSite().getShell();

    final FileDialog dialog = new FileDialog( shell, SWT.SAVE );
    dialog.setText( "Als Html speichern" );
    dialog.setFilterNames( new String[] { "Html Dateien", "Alle Dateien" } );
    dialog.setFilterExtensions( new String[] { "*.html", "*.*" } );

    final String lastName = m_dialogSettings.get( SETTINGS_FILE_NAME );
    final String lastPath = m_dialogSettings.get( SETTINGS_FILE_PATH );

    dialog.setFilterPath( lastPath );
    dialog.setFileName( lastName );

    final String savePath = dialog.open();
    if( savePath == null )
      return;

    m_dialogSettings.put( SETTINGS_FILE_NAME, dialog.getFileName() );
    m_dialogSettings.put( SETTINGS_FILE_PATH, savePath );

    final Job job = new Job( "Html speichern: " + savePath )
    {
      @Override
      protected IStatus run( IProgressMonitor monitor )
      {
        try
        {
          FileUtils.writeStringToFile( new File( savePath ), html, "UTF-8" );
        }
        catch( final IOException e )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          return status;
        }

        return Status.OK_STATUS;
      }
    };
    job.setUser( true );
    job.schedule();
  }

  private String getHtml( )
  {
    if( m_part != null )
    {
      final IHtmlProvider htmlProvider = (IHtmlProvider) m_part.getAdapter( IHtmlProvider.class );
      if( htmlProvider != null )
        return htmlProvider.getHtml();
    }

    return null;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    action.setEnabled( getHtml() != null );
  }

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( final IAction action, final IEditorPart targetEditor )
  {
    m_part = targetEditor;

    action.setEnabled( getHtml() != null );
  }

}
