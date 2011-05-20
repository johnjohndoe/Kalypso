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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.math.BigDecimal;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.ObjectUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.ProgressInputStream;
import org.kalypso.contribs.java.lang.NumberUtils;

/**
 * @author Gernot Belger
 */
public class GafReader
{
  public class SkipLineException extends RuntimeException
  {
    private final int m_severity;

    public SkipLineException( final int severity, final String message )
    {
      super( message );

      m_severity = severity;
    }

    public int getSeverity( )
    {
      return m_severity;
    }
  }

  private LineNumberReader m_reader;

  private final GafProfiles m_gafProfiles;

  private final GafLogger m_logger;

  private int m_goodLines;

  private int m_badLines;

  private GafCodes m_gafCodes;

  public GafReader( final GafLogger logger, final Gaf2Db gaf2db )
  {
    m_logger = logger;

    m_gafProfiles = new GafProfiles( logger, gaf2db );
  }

  public void read( final File gafFile, final IProgressMonitor monitor ) throws IOException
  {
    /* Reading gaf with progress stream to show nice progress for large files */
    final long contentLength = gafFile.length();
    monitor.beginTask( "Reading GAF", (int) contentLength );

    /* Init codes */
    m_gafCodes = new GafCodes();

    /* Open Reader */
    final InputStream fileStream = new BufferedInputStream( new FileInputStream( gafFile ) );
    final ProgressInputStream progresStream = new ProgressInputStream( fileStream, contentLength, monitor );
    m_reader = new LineNumberReader( new InputStreamReader( progresStream ) );
    m_logger.setReader( m_reader );

    readLines();

    m_logger.setReader( null );

    m_logger.log( IStatus.INFO, String.format( "%6d lines read", m_goodLines ), null, null );
    m_logger.log( IStatus.INFO, String.format( "%6d lines skipped", m_badLines ), null, null );
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

    m_gafProfiles.stop();
  }

  private void readLine( final String line )
  {
    try
    {
      final GafPoint point = parseLine( line );
      m_gafProfiles.addPoint( point );
      m_goodLines++;
    }
    catch( final SkipLineException e )
    {
      m_logger.log( e, line );
      m_badLines++;
      // TODO: stop parsing after 1000 errors
    }
  }

  private GafPoint parseLine( final String line )
  {
    final String[] tokens = StringUtils.split( line );
    if( tokens.length < 9 )
      throw new SkipLineException( IStatus.INFO, "Skipping line: too few tokens in line" );

    final Object[] items = parseTokens( tokens );
    checkCommentLine( items );

    final BigDecimal station = asDecimal( items[0], "Station" );
    final String pointId = tokens[1];
    final BigDecimal width = asDecimal( items[2], "Width" );
    final BigDecimal height = asDecimal( items[3], "Height" );
    final String kz = tokens[4].toUpperCase();
    final String roughnessClass = tokens[5];
    final String vegetationClass = tokens[6];
    final BigDecimal rw = asDecimal( items[7], "Rechtswert" );
    final BigDecimal hw = asDecimal( items[8], "Hochwert" );
    final String hyk = tokens.length < 10 ? null : tokens[9].toUpperCase();

    final GafCode kzCode = checkKz( kz );
    final GafCode hykCode = checkHyk( hyk );

    return new GafPoint( station, pointId, width, height, kzCode, roughnessClass, vegetationClass, rw, hw, hykCode );
  }

  /**
   * Skip line, if all token are strings -> we assume it's a comment line
   */
  private void checkCommentLine( final Object[] items )
  {
    for( final Object item : items )
    {
      if( !(item instanceof String) )
        return;
    }

    throw new SkipLineException( IStatus.INFO, "Skpping line" );
  }

  private Object[] parseTokens( final String[] tokens )
  {
    final Object[] items = new Object[tokens.length];
    for( int i = 0; i < items.length; i++ )
      items[i] = parseToken( tokens[i] );
    return items;
  }

  private Object parseToken( final String token )
  {
    final BigDecimal decimal = NumberUtils.parseQuietDecimal( token );
    if( decimal != null )
      return decimal;

    return ObjectUtils.toString( token );
  }

  private BigDecimal asDecimal( final Object item, final String label )
  {
    if( item instanceof BigDecimal )
      return (BigDecimal) item;

    final String message = String.format( "Field '%s' is not a number", label );
    throw new SkipLineException( IStatus.ERROR, message );
  }

  private GafCode checkKz( final String kz )
  {
    final GafCode code = m_gafCodes.getCode( kz );

    if( code == null )
      throw new SkipLineException( IStatus.WARNING, String.format( "Unknown KZ: '%s'; line skipped", kz ) );

    return code;
  }

  private GafCode checkHyk( final String hyk )
  {
    /* Empty string is allowed: no-code */
    if( StringUtils.isBlank( hyk ) )
      return GafCodes.NULL_HYK;

    final GafCode hykCode = m_gafCodes.getHykCode( hyk );

    if( hykCode == null )
      throw new SkipLineException( IStatus.WARNING, String.format( "Unknown Hyk: '%s'; line skipped", hyk ) );

    return hykCode;
  }
}