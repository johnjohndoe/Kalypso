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
package org.kalypso.model.wspm.pdb.gaf.internal;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.LineNumberReader;
import java.io.PrintWriter;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.pdb.gaf.internal.GafReader.SkipLineException;

/**
 * @author Gernot Belger
 */
public class GafLogger
{
  private static final int NO_LINE_NUMBER = -1;

  private final PrintWriter m_logWriter;

  private LineNumberReader m_reader;

  public GafLogger( final File logFile ) throws FileNotFoundException
  {
    m_logWriter = new PrintWriter( logFile );
  }

  public void setReader( final LineNumberReader reader )
  {
    m_reader = reader;
  }

  private int getLineNumber( )
  {
    if( m_reader == null )
      return NO_LINE_NUMBER;

    return m_reader.getLineNumber();
  }

  public void log( final SkipLineException e, final String line )
  {
    log( e.getSeverity(), e.getMessage(), line, null );
  }


  public void log( final IStatus status, final String line )
  {
    log( status.getSeverity(), status.getMessage(), line, status.getException() );
  }

  public void log( final int severity, final String message )
  {
    log( severity, message, null, null );
  }

  public void log( final int severity, final String message, final String line, final Throwable e )
  {
    final String level = StatusUtilities.getLocalizedSeverity( severity );

    final int lineNumber = getLineNumber();
    if( lineNumber != NO_LINE_NUMBER )
      m_logWriter.format( "Line %6d: ", lineNumber );

    m_logWriter.format( "%s - %s", level, message );

    if( line != null )
      m_logWriter.format( ": \"%s\"", line );

    m_logWriter.println();

    if( e != null )
      e.printStackTrace( m_logWriter );
  }

  public void close( )
  {
    if( m_logWriter != null )
      m_logWriter.close();
  }
}