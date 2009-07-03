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
package org.kalypso.convert.namodel;

import java.io.File;
import java.io.IOException;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

/**
 * @author Dejan Antanaskovic
 */
public class NACalculationLogger
{
  private static final String NA_LOG_FILE_PATH[] = { NaModelConstants.OUTPUT_DIR_NAME, "Ergebnisse", "Aktuell", "Log", "calculation.xlog" };

  private boolean m_isLoggerCreated;

  private final String m_calculationFolder;

  private Logger m_logger;

  public NACalculationLogger( final String calculationFolder )
  {
    m_isLoggerCreated = false;
    m_calculationFolder = calculationFolder;
  }

  public final void startLogging( )
  {
    if( m_isLoggerCreated )
      return;
    try
    {
      final StringBuffer buffer = new StringBuffer( m_calculationFolder );
      for( final String pathMember : NA_LOG_FILE_PATH )
        buffer.append( File.separator ).append( pathMember );
      final File logFile = new File( buffer.toString() );
      logFile.getParentFile().mkdirs();
      logFile.createNewFile();
      // Create a non-appending file handler
      final FileHandler handler = new FileHandler( buffer.toString(), false );

      m_logger = Logger.getAnonymousLogger();
      m_logger.addHandler( handler );
      m_isLoggerCreated = true;
      m_logger.info( "Calculation logging started" );
    }
    catch( final IOException e )
    {
      Logger.getAnonymousLogger().warning( e.getLocalizedMessage() );
      e.printStackTrace();
    }
  }

  public final Logger getCalculationLogger( )
  {
    startLogging();
    return m_logger;
  }

// public static final Logger getProjectLogger( )
// {
// return Logger.getLogger( NA_LOGGER );
// }
}
