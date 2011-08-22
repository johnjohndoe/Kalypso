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
package org.kalypso.model.wspm.pdb.internal.gaf;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.text.StrMatcher;
import org.apache.commons.lang3.text.StrTokenizer;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.ProgressInputStream;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;

/**
 * @author Gernot Belger
 */
public class GafReader
{
  private final Collection<GafLine> m_points = new ArrayList<GafLine>();

  private LineNumberReader m_reader;

  private int m_goodLines;

  private int m_badLines;

  public IStatus read( final File gafFile, final IProgressMonitor monitor ) throws IOException
  {
    /* Reading gaf with progress stream to show nice progress for large files */
    final long contentLength = gafFile.length();
    monitor.beginTask( "Reading GAF", (int) contentLength );

    /* Open Reader */
    final InputStream fileStream = new BufferedInputStream( new FileInputStream( gafFile ) );
    final ProgressInputStream progresStream = new ProgressInputStream( fileStream, contentLength, monitor );
    m_reader = new LineNumberReader( new InputStreamReader( progresStream ) );

    readLines();

    final IStatusCollector logger = new StatusCollector(WspmPdbCorePlugin.PLUGIN_ID);

    logger.add( IStatus.INFO, String.format( "Reading file '%s'", gafFile.getAbsolutePath() ) );
    if( m_goodLines > 0 )
      logger.add( IStatus.OK, String.format( "%6d lines read", m_goodLines ) );
    if( m_badLines > 0 )
      logger.add( IStatus.INFO, String.format( "%6d lines skipped", m_badLines ) );

    return logger.asMultiStatus( "Successfully read GAF file" );
  }

  public GafLine[] getLines( )
  {
    return m_points.toArray( new GafLine[m_points.size()] );
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
  }

  private void readLine( final String line )
  {
    try
    {
      final GafLine point = parseLine( line );
      m_points.add( point );

      m_goodLines++;
    }
    catch( final CoreException e )
    {
      final GafLine point = new GafLine( e.getStatus() );
      m_points.add( point );
      m_badLines++;
      // TODO: stop parsing after 1000 errors
    }
  }

  private GafLine parseLine( final String line ) throws CoreException
  {
    final StrTokenizer tokenizer = new StrTokenizer( line );
    tokenizer.setDelimiterMatcher( StrMatcher.trimMatcher() );
    tokenizer.setQuoteMatcher( StrMatcher.noneMatcher() );
    tokenizer.setIgnoredMatcher( StrMatcher.noneMatcher() );
    tokenizer.setTrimmerMatcher( StrMatcher.noneMatcher() );
    tokenizer.setEmptyTokenAsNull( false );
    tokenizer.setIgnoreEmptyTokens( false );
    final String[] tokens = tokenizer.getTokenArray();

    if( tokens.length < 9 )
      throw failLine( IStatus.INFO, "Skipping line: too few tokens in line" );

    final Object[] items = parseTokens( tokens );
    checkCommentLine( items );

    final BigDecimal station = asDecimal( items[0], "Station" );
    final String pointId = asString( tokens[1] );
    final BigDecimal width = asDecimal( items[2], "Width" );
    final BigDecimal height = asDecimal( items[3], "Height" );
    final String code = asString( tokens[4] ).toUpperCase();
    final String roughnessClass = asString( tokens[5] );
    final String vegetationClass = asString( tokens[6] );
    final BigDecimal hw = asDecimal( items[7], "Hochwert" );
    final BigDecimal rw = asDecimal( items[8], "Rechtswert" );
    final String hyk = tokens.length < 10 ? StringUtils.EMPTY : asString( tokens[9] ).toUpperCase();

    return new GafLine( station, pointId, width, height, code, roughnessClass, vegetationClass, rw, hw, hyk, Status.OK_STATUS );
  }

  private String asString( final String token )
  {
    return token.trim();
  }

  /**
   * Skip line, if all token are strings -> we assume it's a comment line
   */
  private void checkCommentLine( final Object[] items ) throws CoreException
  {
    for( final Object item : items )
    {
      if( !(item instanceof String) )
        return;
    }

    throw failLine( IStatus.INFO, "Skipping line without numbers - comment line?" );
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

  private BigDecimal asDecimal( final Object item, final String label ) throws CoreException
  {
    if( item instanceof BigDecimal )
      return (BigDecimal) item;

    final String message = String.format( "Field '%s' is not a number", label );
    throw failLine( IStatus.ERROR, message );
  }

  private CoreException failLine( final int severity, final String message )
  {
    final String msg = String.format( "LINE %d: %s", m_reader.getLineNumber(), message );
    final IStatus status = new Status( severity, WspmPdbCorePlugin.PLUGIN_ID, msg );
    return new CoreException( status );
  }
}