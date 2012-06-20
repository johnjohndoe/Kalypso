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
package org.kalypso.kalypsosimulationmodel.ui.calccore;

import java.io.File;
import java.io.FileFilter;
import java.util.regex.Pattern;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypsosimulationmodel.internal.KalypsoModelSimulationBase;
import org.kalypso.kalypsosimulationmodel.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public final class CalcCoreUtils
{
  public static final String VERSION_LATEST = "latest"; //$NON-NLS-1$

  public static final String VERSION_NEUESTE = "neueste"; //$NON-NLS-1$

  private CalcCoreUtils( )
  {
    throw new UnsupportedOperationException( "Helper class, do not instantiate" );//$NON-NLS-1$
  }

  public static enum COMPATIBILITY_MODE
  {
    NONE,
    NA,
    WSPM,
    RMA
  }

  private static final String BIN_DIR = "bin"; //$NON-NLS-1$

  public static File getExecutablesDirectory( )
  {
    final Location installLocation = Platform.getInstallLocation();
    final File installDir = FileUtils.toFile( installLocation.getURL() );
    return new File( installDir, BIN_DIR );
  }

  public static File findExecutable( final String version, final String pattern, final String searchPattern ) throws CoreException
  {
    return findExecutable( version, pattern, searchPattern, COMPATIBILITY_MODE.NONE );
  }

  public static File findExecutable( final String version, final String pattern, final String searchPattern, final COMPATIBILITY_MODE compatibilityMode ) throws CoreException
  {
    if( StringUtils.isBlank( version ) )
    {
      final File latestExecutable = getLatestExecutable( searchPattern );
      if( latestExecutable == null )
      {
        // Version des Rechenkerns nicht angegeben. Die Version muss in den Steuerparametern gesetzt werden.
        final String msg = Messages.getString( "org.kalypso.ogc.gml.featureview.control.ChooseExeControl.1" ); //$NON-NLS-1$
        final IStatus status = new Status( IStatus.ERROR, KalypsoModelSimulationBase.PLUGIN_ID, msg );
        throw new CoreException( status );
      }

      return latestExecutable;
    }

    switch( compatibilityMode )
    {
      case NA:
        /*
         * for backward compatibility, strings "neueste" or "latest" will be considered as well
         */
        if( VERSION_NEUESTE.equals( version ) || VERSION_LATEST.equals( version ) )
          return getLatestExecutable( searchPattern );
        break;
      case NONE:
      case WSPM:
      case RMA:
        break;
    }

    /* Always call this in order to provoke the download error message */
    getAvailableExecuteablesChecked( searchPattern );

    // REMARK: This is OS dependent; we use should use a pattern according to OS
    final String exeName = String.format( pattern, version );
    final File exeFile = new File( getExecutablesDirectory(), exeName );

    if( !exeFile.exists() )
    {
      final String msg = Messages.getString( "org.kalypso.ogc.gml.featureview.control.ChooseExeControl.6", exeFile.getAbsolutePath() ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelSimulationBase.PLUGIN_ID, msg );
      throw new CoreException( status );
    }

    if( !exeFile.isFile() )
    {
      final String msg = Messages.getString( "org.kalypso.ogc.gml.featureview.control.ChooseExeControl.2", exeFile.getAbsolutePath() ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelSimulationBase.PLUGIN_ID, msg );
      throw new CoreException( status );
    }

    if( !exeFile.canExecute() )
    {
      final String msg = Messages.getString( "org.kalypso.ogc.gml.featureview.control.ChooseExeControl.5", exeFile.getAbsolutePath() ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelSimulationBase.PLUGIN_ID, msg );
      throw new CoreException( status );
    }

    return exeFile;
  }

  public static File getLatestExecutable( final String searchPattern ) throws CoreException
  {
    final File[] executables = getAvailableExecuteablesChecked( searchPattern );
    if( executables.length == 1 )
      return executables[0];

    File latest = executables[0];
    for( int i = 1; i < executables.length; i++ )
    {
      if( executables[i].lastModified() > latest.lastModified() )
        latest = executables[i];
    }
    return latest;
  }

  public static File[] getAvailableExecuteablesChecked( final String searchPattern ) throws CoreException
  {
    final File[] executables = getAvailableExecutables( searchPattern );
    if( ArrayUtils.isEmpty( executables ) )
    {
      final File exeDir = getExecutablesDirectory();
      final String msg = String.format( Messages.getString( "org.kalypso.kalypsosimulationmodel.ui.calccore.CalcCoreUtils.0" ), exeDir.getAbsolutePath() ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, KalypsoModelSimulationBase.PLUGIN_ID, msg );
      throw new CoreException( status );
    }
    return executables;
  }

  private static File[] getAvailableExecutables( final String searchPattern )
  {
    /*
     * we will assume that the latest executable is the one with the most recent file modification date
     */
    final File[] executables = getExecutablesDirectory().listFiles( new FileFilter()
    {
      @Override
      public boolean accept( final File pathname )
      {
        return Pattern.matches( searchPattern, pathname.getName() );
      }
    } );
    return executables;
  }

  public static File[] checkExecutablesAvailable( final Shell shell, final String exePattern, final String dialogTitle )
  {
    try
    {
      return CalcCoreUtils.getAvailableExecuteablesChecked( exePattern );
    }
    catch( final CoreException e )
    {
      final StatusDialog statusDialog = new StatusDialog( shell, e.getStatus(), dialogTitle );
      statusDialog.open();
      return null;
    }
  }

}
