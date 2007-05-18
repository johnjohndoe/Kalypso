/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.application;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.product.KalypsoModelProductPlugin;

/**
 * @author Holger Albert
 */
public class KalypsoModelApplication implements IApplication
{
  /**
   * @see org.eclipse.equinox.app.IApplication#start(org.eclipse.equinox.app.IApplicationContext)
   */
  public Object start( IApplicationContext context ) throws Exception
  {
    final Display display = PlatformUI.createDisplay();

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

    final boolean isExpert = Boolean.getBoolean( "kalypso.model.product.expert" );
    final KalypsoModelWorkbenchAdvisor advisor = new KalypsoModelWorkbenchAdvisor( !isExpert );
    final int returnCode = PlatformUI.createAndRunWorkbench( display, advisor );
    if( returnCode == PlatformUI.RETURN_RESTART )
      return IApplication.EXIT_RESTART;

    display.dispose();

    return IApplication.EXIT_OK;
  }

  /**
   * @see org.eclipse.equinox.app.IApplication#stop()
   */
  public void stop( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    if( workbench == null )
      return;
    final Display display = workbench.getDisplay();
    display.syncExec( new Runnable()
    {
      public void run( )
      {
        if( !display.isDisposed() )
          workbench.close();
      }
    } );
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
    final Location instanceLoc = Platform.getInstanceLocation();
    final String dialogTitle = "Kalypso Simulation Platform";
    if( instanceLoc == null )
    {
      MessageDialog.openError( shell, dialogTitle, "IDEs need a valid workspace. Restart without the @none option." );
      return false;
    }

    // -data "/valid/path", workspace already set
    if( instanceLoc.isSet() )
    {
      // at this point its valid, so try to lock it and update the
      // metadata version information if successful
      try
      {
        if( instanceLoc.lock() )
          return true;

        // we failed to create the directory.
        // Two possibilities:
        // 1. directory is already in use
        // 2. directory could not be created
        File workspaceDirectory = new File( instanceLoc.getURL().getFile() );
        if( workspaceDirectory.exists() )
          MessageDialog.openError( shell, dialogTitle, "Could not launch the product because the associated workspace is currently in use by another INFORM.DSS application." );
        else
          MessageDialog.openError( shell, dialogTitle, "Could not launch the product because the specified workspace cannot be created.  The specified workspace directory is either invalid or read-only." );
      }
      catch( final IOException e )
      {
        final String message = "Could not obtain lock for workspace location";
        final IStatus status = StatusUtilities.createErrorStatus( message );
        KalypsoModelProductPlugin.getDefault().getLog().log( status );
        MessageDialog.openError( shell, dialogTitle, message );
      }
      return false;
    }

    /* No instance location is set, show error dialog. */
    MessageDialog.openError( shell, dialogTitle, "Could not launch the product because the there is no workspace specified." );

    return false;
  }
}