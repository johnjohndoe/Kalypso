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
package org.kalypso.model.hydrology.internal.processing;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.kalypsosimulationmodel.ui.calccore.CalcCoreUtils;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;

/**
 * Executes the Kalypso-NA.exe and checks if it has succeeded.<br/>
 * The input data must already have been created.
 * 
 * @author Gernot Belger
 */
public class KalypsoNaProcessor
{
  public static final String EXECUTABLES_FILE_TEMPLATE = "Kalypso-NA_%s.exe"; //$NON-NLS-1$

  public static final String EXECUTABLES_FILE_PATTERN = "Kalypso-NA_(.+)\\.exe"; //$NON-NLS-1$

  private static final String STRING_RESULT_SUCCESSFUL_1 = "berechnung wurde ohne fehler beendet"; //$NON-NLS-1$

  private static final String STRING_RESULT_SUCCESSFUL_2 = "Berechnung wurde ohne Fehler beendet!";

  private static final String FILENAME_OUTPUT_RES = "output.res"; //$NON-NLS-1$

  private static final String FILENAME_EXE_LOG = "exe.log";//$NON-NLS-1$

  private static final String FILENAME_EXE_ERR = "exe.err";//$NON-NLS-1$

  private final NaAsciiDirs m_asciiDirs;

  private final String m_exeVersion;

  public KalypsoNaProcessor( final NaAsciiDirs asciiDirs, final String exeVersion )
  {
    m_asciiDirs = asciiDirs;
    m_exeVersion = exeVersion;
  }

  public boolean run( final ISimulationMonitor monitor ) throws SimulationException
  {
    final File kalypsoNaExe = copyExecutable();
    if( kalypsoNaExe == null )
      return false;

    runExe( kalypsoNaExe, monitor );

    return checkSucceeded();
  }

  private File copyExecutable( ) throws SimulationException
  {
    try
    {
      final File kalypsoNaExe = CalcCoreUtils.findExecutable( m_exeVersion, EXECUTABLES_FILE_TEMPLATE, EXECUTABLES_FILE_PATTERN, CalcCoreUtils.COMPATIBILITY_MODE.NA );
      if( kalypsoNaExe == null )
        throw new SimulationException( "No Kalypso-NA.exe version configured." );

      final File destFile = new File( m_asciiDirs.startDir, kalypsoNaExe.getName() );
      if( !destFile.exists() )
        FileUtils.copyFile( kalypsoNaExe, destFile );

      return destFile;
    }
    catch( final CoreException e )
    {
      final IStatus status = e.getStatus();
      final String msg = String.format( "No Kalypso-NA.exe version configured: %s", status.getMessage() );
      throw new SimulationException( msg, e );
    }
    catch( final IOException e )
    {
      final String msg = String.format( "Failed to copy Kalypso-NA.exe into calculation directory: %s", e.getLocalizedMessage() );
      throw new SimulationException( msg, e );
    }
  }

  private void runExe( final File kalypsoNaExe, final ISimulationMonitor monitor ) throws SimulationException
  {
    final String commandString = kalypsoNaExe.getAbsolutePath();

    final long timeOut = 0l; // no timeout control

    FileOutputStream logOS = null;
    FileOutputStream errorOS = null;
    try
    {
      logOS = new FileOutputStream( new File( m_asciiDirs.asciiDir, FILENAME_EXE_LOG ) );
      errorOS = new FileOutputStream( new File( m_asciiDirs.asciiDir, FILENAME_EXE_ERR ) );
      ProcessHelper.startProcess( commandString, new String[0], m_asciiDirs.startDir, monitor, timeOut, logOS, errorOS, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.249" ), e ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( logOS );
      IOUtils.closeQuietly( errorOS );
    }
  }

  private boolean checkSucceeded( )
  {
    final String logContent = readOutputRes();
    if( logContent == null )
      return false;

    if( logContent.contains( STRING_RESULT_SUCCESSFUL_1 ) )
      return true;
    if( logContent.contains( STRING_RESULT_SUCCESSFUL_2 ) )
      return true;

    return false;
  }

  private String readOutputRes( )
  {
    try
    {
      return FileUtils.readFileToString( new File( m_asciiDirs.startDir, FILENAME_OUTPUT_RES ), null );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      return null;
    }
  }

}
