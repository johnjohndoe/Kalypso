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
package org.kalypso.model.hydrology.internal;

import java.io.File;
import java.io.IOException;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

import org.kalypso.contribs.java.util.logging.SimplisticFormatter;

/**
 * @author Dejan Antanaskovic
 */
public class NACalculationLogger
{
  public static final String LOGFILE_NAME = "calculation.log"; //$NON-NLS-1$

  private Logger m_logger;

  private FileHandler m_logHandler;

  private final File m_logFile;

  public NACalculationLogger( final File logDir )
  {
    m_logFile = new File( logDir, LOGFILE_NAME );
  }

  private final void startLogging( )
  {
    if( m_logHandler != null )
      return;

    try
    {
      m_logFile.getParentFile().mkdirs();
      m_logFile.createNewFile();

      m_logHandler = new FileHandler( m_logFile.getAbsolutePath(), false );
      m_logHandler.setFormatter( new SimplisticFormatter() );
      m_logHandler.setEncoding( "UTF-8" );

      m_logger = Logger.getLogger( LOGFILE_NAME );
      m_logger.addHandler( m_logHandler );
      m_logger.info( "Calculation logging started" ); //$NON-NLS-1$
    }
    catch( final IOException e )
    {
      Logger.getAnonymousLogger().warning( e.getLocalizedMessage() );
      e.printStackTrace();
    }
  }

  public final Logger getLogger( )
  {
    startLogging();
    return m_logger;
  }

  public final void stopLogging( )
  {
    if( m_logHandler != null )
    {
      m_logHandler.flush();
      m_logHandler.close();
      m_logHandler = null;
      m_logger = null;
    }
  }
}
