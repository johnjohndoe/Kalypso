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
package org.kalypso.model.wspm.pdb.ui.gaf.internal;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.math.BigDecimal;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.ProgressInputStream;
import org.kalypso.model.wspm.pdb.db.PdbState;
import org.kalypso.model.wspm.pdb.db.PdbWaterBody;

/**
 * @author Gernot Belger
 */
public class GafReader
{
  public class SkipLineException extends RuntimeException
  {
  }

  private LineNumberReader m_reader;

  private final PdbState m_state;

  private final PdbWaterBody m_waterBody;

  private final PrintWriter m_logWriter;

  private GafProfile m_currentProfile;

  public GafReader( final PdbState state, final PdbWaterBody waterBody, final PrintWriter logWriter )
  {
    m_state = state;
    m_waterBody = waterBody;
    m_logWriter = logWriter;
  }

  public void read( final File gafFile, final IProgressMonitor monitor ) throws IOException
  {
    /* Reading gaf with progress stream to show nice progress for large files */
    final long contentLength = gafFile.length();

    monitor.beginTask( "Reading GAF", (int) contentLength );
    final InputStream fileStream = new BufferedInputStream( new FileInputStream( gafFile ) );
    final ProgressInputStream progresStream = new ProgressInputStream( fileStream, contentLength, monitor );
    m_reader = new LineNumberReader( new InputStreamReader( progresStream ) );

    readLines();
  }

  public void close( ) throws IOException
  {
    if( m_reader != null )
      m_reader.close();
  }

  public void closeQuietly( )
  {
    IOUtils.closeQuietly( m_reader );
  }

  private void readLines( ) throws IOException
  {
    while( m_reader.ready() )
    {
      final String line = m_reader.readLine();
      if( line == null )
        return;

      readLine( line );
    }

    commitProfile();
  }

  private void readLine( final String line )
  {
    try
    {
      final GafPoint point = parseLine( line );
      addPoint( point );
    }
    catch( final SkipLineException e )
    {
      // just skip, error handling already has been done
      // TODO: count errors, stop parsing after 1000 errors
    }
  }

  private GafPoint parseLine( final String line )
  {
    final String[] tokens = StringUtils.split( line );
    if( tokens.length < 7 )
    {
      logWarning( "Format error: too few tokens in line", line );
      throw new SkipLineException();
    }

    final String stationToken = tokens[0];
    final String pointId = tokens[1];
    final String widthToken = tokens[2];
    final String heightToken = tokens[3];
    final String kzToken = tokens[4];
    final String rwToken = tokens[5];
    final String hwToken = tokens[6];
    final String hykToken = tokens.length > 6 ? tokens[7] : null;

    final BigDecimal station = parseDecimal( stationToken, "Station" );
    final BigDecimal width = parseDecimal( widthToken, "Width" );
    final BigDecimal height = parseDecimal( heightToken, "Height" );
    final BigDecimal rw = parseDecimal( rwToken, "Rechtswert" );
    final BigDecimal hw = parseDecimal( hwToken, "Hochwert" );

    final String kz = parseKennziffer(kzToken);
    final String hyk = parseHyk( hykToken );

    return new GafPoint( station, pointId, width, height, kz, rw, hw, hyk );
  }

  private BigDecimal parseDecimal( final String token, final String label )
  {
    try
    {
      final String cleanToken = StringUtils.replaceChars( token, ',', '.' );
      return new BigDecimal( cleanToken );
    }
    catch( final NumberFormatException e )
    {
      final String message = String.format( "Failed to parse field '%s'", label );
      logError( message, token );
      throw new SkipLineException();
    }
  }

  private String parseKennziffer( final String kzToken )
  {
    // TODO Auto-generated method stub
    return null;
  }

  private String parseHyk( final String hykToken )
  {
    // TODO Auto-generated method stub
    return null;
  }

  private void addPoint( final GafPoint point )
  {
    final BigDecimal station = point.getStation();

    // TODO: check, if this station has already be processed -> error

    if( m_currentProfile != null && !station.equals( m_currentProfile.getStation() ) )
      commitProfile();

    if( m_currentProfile == null )
      createProfile( station );

    m_currentProfile.addPoint( point );
  }

  private void createProfile( final BigDecimal station )
  {
    // TODO Auto-generated method stub
    m_currentProfile = new GafProfile( station );
  }

  private void commitProfile( )
  {
    // TODO Auto-generated method stub

    m_currentProfile = null;
  }

  private void logWarning( final String message, final String line )
  {
    log( "WARNING", message, line );
  }

  private void logError( final String message, final String line )
  {
    log( "ERROR", message, line );
  }

  private void log( final String level, final String message, final String line )
  {
    m_logWriter.format( "Line %d: %s - %s (%s)%n", level, message, line );
  }
}