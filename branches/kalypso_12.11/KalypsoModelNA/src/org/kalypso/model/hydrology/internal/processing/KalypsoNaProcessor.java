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
import org.kalypso.commons.java.lang.ProcessHelper;
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
  private static final String FILENAME_EXE_LOG = "exe.log";//$NON-NLS-1$

  private static final String FILENAME_EXE_ERR = "exe.err";//$NON-NLS-1$

  private final NaAsciiDirs m_asciiDirs;

  private File m_kalypsoNaExe;

  private final File m_exeFile;

  public KalypsoNaProcessor( final NaAsciiDirs asciiDirs, final File exeFile )
  {
    m_asciiDirs = asciiDirs;
    m_exeFile = exeFile;
  }

  public void prepare( ) throws SimulationException
  {
    m_kalypsoNaExe = copyExecutable();
    if( m_kalypsoNaExe == null )
      return;
  }

  private File copyExecutable( ) throws SimulationException
  {
    try
    {
      final File destFile = new File( m_asciiDirs.startDir, m_exeFile.getName() );
      if( !destFile.exists() )
        FileUtils.copyFile( m_exeFile, destFile );

      destFile.setExecutable( true );

      return destFile;
    }
    catch( final IOException e )
    {
      final String msg = String.format( Messages.getString( "KalypsoNaProcessor.1" ), e.getLocalizedMessage() ); //$NON-NLS-1$
      throw new SimulationException( msg, e );
    }
  }

  public void run( final ISimulationMonitor monitor ) throws SimulationException
  {
    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.27" ) ); //$NON-NLS-1$

    final String[] commandString = new String[] { m_kalypsoNaExe.getAbsolutePath() };

    final long timeOut = 0L; // no timeout control

    FileOutputStream logOS = null;
    FileOutputStream errorOS = null;
    try
    {
      logOS = new FileOutputStream( new File( m_asciiDirs.asciiDir, FILENAME_EXE_LOG ) );
      errorOS = new FileOutputStream( new File( m_asciiDirs.asciiDir, FILENAME_EXE_ERR ) );
      ProcessHelper.startProcess( commandString, null, m_asciiDirs.startDir, monitor, timeOut, logOS, errorOS, null );
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
}
