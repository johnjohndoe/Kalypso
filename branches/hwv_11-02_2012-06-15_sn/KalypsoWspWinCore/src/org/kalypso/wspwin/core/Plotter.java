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
package org.kalypso.wspwin.core;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.process.StreamStreamer;
import org.kalypso.wspwin.core.i18n.Messages;

/**
 * Helper code to access WspWin-Plotter
 * 
 * @author Gernot Belger
 */
public class Plotter
{
  private static final String WSPWIN_PLOTTER = "org.kalypso.model.wspwin.plotter"; //$NON-NLS-1$

  private static final String WSPWIN_PLOTTER_PATH = WSPWIN_PLOTTER + ".path"; //$NON-NLS-1$

  private static final String STR_BASIC_PLOTTER_MESSAGE = Messages.getString("Plotter_1"); //$NON-NLS-1$

  /**
   * Check if the plotter exe is available. If not the user is asked to choose it in a file dialog.
   */
  public static final boolean checkPlotterExe( final Shell shell )
  {
    final File plotterExe = getPlotterExe();
    if( plotterExe != null && plotterExe.exists() && plotterExe.isFile() && plotterExe.canExecute() )
      return true;

    final String message = getPlotterDialogMessage( plotterExe == null );
    MessageDialog.openWarning( shell, Messages.getString("Plotter_2"), message ); //$NON-NLS-1$

    final FileDialog dlg = new FileDialog( shell );
    dlg.setFilterNames( new String[] { Messages.getString("Plotter_3"), Messages.getString("Plotter_4") } ); //$NON-NLS-1$ //$NON-NLS-2$
    dlg.setFilterExtensions( new String[] { "*.*", "*.exe" } ); //$NON-NLS-1$ //$NON-NLS-2$
    dlg.setFilterIndex( 1 );
    dlg.setText( Messages.getString("Plotter_7") ); //$NON-NLS-1$

    final String newPlotterPath = dlg.open();
    if( newPlotterPath == null )
      return false;

    final IPreferenceStore preferences = getPlotterPreferences();
    preferences.setValue( WSPWIN_PLOTTER_PATH, newPlotterPath );
    return true;
  }

  /**
   * Returns the currently configured plotter exe, regardless if it still exists or not.
   * 
   * @return <code>null</code>, if never a path has been specified.
   */
  private static File getPlotterExe( )
  {
    final IPreferenceStore preferences = getPlotterPreferences();
    final String plotterPath = preferences.getString( WSPWIN_PLOTTER_PATH );
    if( plotterPath == null || plotterPath.isEmpty() )
      return null;

    return new File( plotterPath );
  }

  /**
   * Returns the configured plotter.exe and also checks if it still exists and can be executed. If not,
   * <code>null</code> is returned.
   */
  public static File getPlotterExeChecked( )
  {
    final File plotterExe = getPlotterExe();
    if( plotterExe != null && plotterExe.exists() && plotterExe.isFile() && plotterExe.canExecute() )
      return plotterExe;

    return null;
  }

  private static IPreferenceStore getPlotterPreferences( )
  {
    return KalypsoWspWinCorePlugin.getDefault().getPreferenceStore();
  }

  private static String getPlotterDialogMessage( final boolean firstTime )
  {
    if( firstTime )
      return Messages.getString("Plotter_8") + STR_BASIC_PLOTTER_MESSAGE; //$NON-NLS-1$

    return Messages.getString("Plotter_9") + STR_BASIC_PLOTTER_MESSAGE; //$NON-NLS-1$
  }

  /**
   * Starts the plotter on the specified profile or length-section.<br>
   * The method {@link #checkPlotterExe(Shell)} should be called beforehand.
   */
  public static void openPrf( final File file, final boolean doPrint ) throws IOException
  {
    final File plotterExe = getPlotterExeChecked();
    if( plotterExe == null )
      return;

    final Runtime runtime = Runtime.getRuntime();

    final Collection<String> commands = new ArrayList<String>();
    commands.add( plotterExe.getAbsolutePath() );
    if( doPrint )
      commands.add( "-p" ); //$NON-NLS-1$
    commands.add( file.getName() );

    final String[] cmdArray = commands.toArray( new String[commands.size()] );
    final Process p = runtime.exec( cmdArray, null, file.getParentFile() );
    new StreamStreamer( p.getInputStream(), System.out );
    new StreamStreamer( p.getErrorStream(), System.err );
  }

}
