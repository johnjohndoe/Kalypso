/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.application;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.util.Properties;

import javax.swing.UIManager;

import org.eclipse.core.runtime.IPlatformRunnable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.internal.ide.IDEWorkbenchMessages;
import org.eclipse.ui.internal.ide.IDEWorkbenchPlugin;
import org.eclipse.ui.internal.ide.StatusUtil;
import org.kalypso.auth.KalypsoAuthPlugin;
import org.kalypso.auth.user.IKalypsoUser;
import org.kalypso.core.KalypsoStart;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author belger
 */
public class KalypsoApplication implements IPlatformRunnable
{
  private static final String METADATA_FOLDER = ".metadata"; //$NON-NLS-1$
  private static final String VERSION_FILENAME = "version.ini"; //$NON-NLS-1$
  private static final String WORKSPACE_VERSION_KEY = "org.eclipse.core.runtime"; //$NON-NLS-1$
  private static final String WORKSPACE_VERSION_VALUE = "1"; //$NON-NLS-1$

  /**
   * @see org.eclipse.core.runtime.IPlatformRunnable#run(java.lang.Object)
   */
  public Object run( final Object args ) throws Exception
  {
    try
    {
      // Set look and feel because we are using SWT-AWT
      UIManager.setLookAndFeel( UIManager.getSystemLookAndFeelClassName() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    // Important: only once create the display and set it from the outside
    // for both login and the workbench
    // Reason: some resources (e.g. the dialog images) are (only once) registered statically on the display
    // If the display gets disposed, all resources are disposed as well and will never be found
    // again
    final Display display = PlatformUI.createDisplay();

    try
    {
      final Shell shell = new Shell( display, SWT.ON_TOP );
      try
      {
        if( !checkInstanceLocation( shell ) )
        {
          Platform.endSplash();
          return EXIT_OK;
        }
      }
      finally
      {
        if( shell != null )
          shell.dispose();
      }
      
      KalypsoStart.parseArguments( (String[])args );

      // TODO: HACK TO BE REMOVED when separation client/server is ok
      KalypsoGisPlugin.getDefault();

      final KalypsoAuthPlugin authPlugin = KalypsoAuthPlugin.getDefault();
      authPlugin.startLoginProcedure( display );

      try
      {
        final IKalypsoUser user = authPlugin.getCurrentUser();
        if( user == null )
          return IPlatformRunnable.EXIT_OK;

        return startWorkbench( display, new KalypsoWorkbenchAdvisor( user ) );
      }
      catch( final IllegalStateException e )
      {
        return IPlatformRunnable.EXIT_OK;
      }
    }
    catch( InterruptedException e )
    {
      return IPlatformRunnable.EXIT_OK;
    }
    finally
    {
      display.dispose();

      // be sure to close splash screen
      // important for instance in the case no workbench started (user cancelled login-procedure)
      Platform.endSplash();
    }
  }

  private Object startWorkbench( final Display display, final WorkbenchAdvisor advisor )
  {
    final int returnCode = PlatformUI.createAndRunWorkbench( display, advisor );

    return returnCode == PlatformUI.RETURN_RESTART ? IPlatformRunnable.EXIT_RESTART : IPlatformRunnable.EXIT_OK;
  }

  /**
   * Return true if a valid workspace path has been set and false otherwise. Prompt for and set the path if possible and
   * required.
   * 
   * @return true if a valid instance location has been set and false otherwise
   */
  private boolean checkInstanceLocation( Shell shell )
  {
    // -data @none was specified but an ide requires workspace
    Location instanceLoc = Platform.getInstanceLocation();
    if( instanceLoc == null )
    {
      MessageDialog.openError( shell, IDEWorkbenchMessages.getString( "IDEApplication.workspaceMandatoryTitle" ), //$NON-NLS-1$
          IDEWorkbenchMessages.getString( "IDEApplication.workspaceMandatoryMessage" ) ); //$NON-NLS-1$
      return false;
    }

    // -data "/valid/path", workspace already set
    if( instanceLoc.isSet() )
    {
      // make sure the meta data version is compatible (or the user has
      // chosen to overwrite it).
      if( !checkValidWorkspace( shell, instanceLoc.getURL() ) )
        return false;

      // at this point its valid, so try to lock it and update the
      // metadata version information if successful
      try
      {
        if( instanceLoc.lock() )
        {
          writeWorkspaceVersion();
          return true;
        }
      }
      catch( IOException e )
      {
        // do nothing
      }

      MessageDialog.openError( shell, IDEWorkbenchMessages.getString( "IDEApplication.workspaceCannotLockTitle" ), //$NON-NLS-1$
          IDEWorkbenchMessages.getString( "IDEApplication.workspaceCannotLockMessage" ) ); //$NON-NLS-1$
      return false;
    }
    
    return false;
  }

  /**
   * Return true if the argument directory is ok to use as a workspace and false otherwise. A version check will be
   * performed, and a confirmation box may be displayed on the argument shell if an older version is detected.
   * 
   * @return true if the argument URL is ok to use as a workspace and false otherwise.
   */
  private boolean checkValidWorkspace( Shell shell, URL url )
  {
    String version = readWorkspaceVersion( url );

    // if the version could not be read, then there is not any existing
    // workspace data to trample, e.g., perhaps its a new directory that
    // is just starting to be used as a workspace
    if( version == null )
      return true;

    final int ide_version = Integer.parseInt( WORKSPACE_VERSION_VALUE );
    int workspace_version = Integer.parseInt( version );

    // equality test is required since any version difference (newer
    // or older) may result in data being trampled
    if( workspace_version == ide_version )
      return true;

    // At this point workspace has been detected to be from a version
    // other than the current ide version -- find out if the user wants
    // to use it anyhow.
    String title = IDEWorkbenchMessages.getString( "IDEApplication.versionTitle" ); //$NON-NLS-1$
    String message = IDEWorkbenchMessages.format( "IDEApplication.versionMessage", //$NON-NLS-1$
        new Object[]
        { url.getFile() } );

    MessageBox mbox = new MessageBox( shell, SWT.OK | SWT.CANCEL | SWT.ICON_WARNING | SWT.APPLICATION_MODAL );
    mbox.setText( title );
    mbox.setMessage( message );
    return mbox.open() == SWT.OK;
  }

  /**
   * Look at the argument URL for the workspace's version information. Return that version if found and null otherwise.
   */
  private static String readWorkspaceVersion( URL workspace )
  {
    File versionFile = getVersionFile( workspace, false );
    if( versionFile == null || !versionFile.exists() )
      return null;

    try
    {
      // Although the version file is not spec'ed to be a Java properties
      // file, it happens to follow the same format currently, so using
      // Properties to read it is convenient.
      Properties props = new Properties();
      FileInputStream is = new FileInputStream( versionFile );
      try
      {
        props.load( is );
      }
      finally
      {
        is.close();
      }

      return props.getProperty( WORKSPACE_VERSION_KEY );
    }
    catch( IOException e )
    {
      IDEWorkbenchPlugin.log( "Could not read version file", new Status( //$NON-NLS-1$
          IStatus.ERROR, IDEWorkbenchPlugin.IDE_WORKBENCH, IStatus.ERROR, e.getMessage() == null ? "" : e.getMessage(), //$NON-NLS-1$, 
          e ) );
      return null;
    }
  }

  /**
   * Write the version of the metadata into a known file overwriting any existing file contents. Writing the version
   * file isn't really crucial, so the function is silent about failure
   */
  private static void writeWorkspaceVersion()
  {
    Location instanceLoc = Platform.getInstanceLocation();
    if( instanceLoc == null || instanceLoc.isReadOnly() )
      return;

    File versionFile = getVersionFile( instanceLoc.getURL(), true );
    if( versionFile == null )
      return;

    OutputStream output = null;
    try
    {
      String versionLine = WORKSPACE_VERSION_KEY + '=' + WORKSPACE_VERSION_VALUE;

      output = new FileOutputStream( versionFile );
      output.write( versionLine.getBytes( "UTF-8" ) ); //$NON-NLS-1$
    }
    catch( IOException e )
    {
      IDEWorkbenchPlugin.log( "Could not write version file", //$NON-NLS-1$
          StatusUtil.newStatus( IStatus.ERROR, e.getMessage(), e ) );
    }
    finally
    {
      try
      {
        if( output != null )
          output.close();
      }
      catch( IOException e )
      {
        // do nothing
      }
    }
  }

  /**
   * The version file is stored in the metadata area of the workspace. This method returns an URL to the file or null if
   * the directory or file does not exist (and the create parameter is false).
   * 
   * @param create
   *          If the directory and file does not exist this parameter controls whether it will be created.
   * @return An url to the file or null if the version file does not exist or could not be created.
   */
  private static File getVersionFile( URL workspaceUrl, boolean create )
  {
    if( workspaceUrl == null )
      return null;

    try
    {
      // make sure the directory exists
      File metaDir = new File( workspaceUrl.getPath(), METADATA_FOLDER );
      if( !metaDir.exists() && ( !create || !metaDir.mkdir() ) )
        return null;

      // make sure the file exists
      File versionFile = new File( metaDir, VERSION_FILENAME );
      if( !versionFile.exists() && ( !create || !versionFile.createNewFile() ) )
        return null;

      return versionFile;
    }
    catch( IOException e )
    {
      // cannot log because instance area has not been set
      return null;
    }
  }

}