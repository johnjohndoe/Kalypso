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
package org.kalypso.wspwin.core;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.StringTokenizer;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.lang3.StringUtils;
import org.kalypso.wspwin.core.WspCfg.TYPE;
import org.kalypso.wspwin.core.i18n.Messages;

/**
 * Represents one line of a .ber file.
 *
 * @author Belger
 */
public class CalculationBean
{
  private final String m_name;

  private final String m_fileName;

  private final BigDecimal m_fromStation;

  private final BigDecimal m_toStation;

  public CalculationBean( final String name, final String fileName, final BigDecimal fromStation, final BigDecimal toStation )
  {
    m_name = name;
    m_fileName = fileName;
    m_fromStation = fromStation;
    m_toStation = toStation;
  }

  public String getFileName( )
  {
    return m_fileName;
  }

  public BigDecimal getFromStation( )
  {
    return m_fromStation;
  }

  public String getName( )
  {
    return m_name;
  }

  public BigDecimal getToStation( )
  {
    return m_toStation;
  }

  public static CalculationBean[] readBerFile( final File berFile ) throws ParseException, IOException
  {
    // if a zustand has no calculations, no .ber file is present.
    if( !berFile.exists() )
      return new CalculationBean[0];

    final List<CalculationBean> beans = new ArrayList<>( 10 );

    LineIterator lineIt = null;
    try
    {
      int count = 0;
      lineIt = FileUtils.lineIterator( berFile, "CP850" ); //$NON-NLS-1$

      // ignore first line, we just read all lines
      lineIt.nextLine();
      count++;

      while( lineIt.hasNext() )
      {
        final String line = lineIt.nextLine();
        count++;

        if( line.length() < 60 )
          throw new ParseException( Messages.getString("org.kalypso.wspwin.core.CalculationBean.0") + line, count ); //$NON-NLS-1$

        final String name = line.substring( 0, 60 ).trim();

        final StringTokenizer tokenizer = new StringTokenizer( line.substring( 60 ) );

        if( tokenizer.countTokens() != 3 )
          throw new ParseException( Messages.getString("org.kalypso.wspwin.core.CalculationBean.1") + line, count ); //$NON-NLS-1$

        final BigDecimal fromStation = new BigDecimal( tokenizer.nextToken() );
        final BigDecimal toStation = new BigDecimal( tokenizer.nextToken() );
        final String fileName = tokenizer.nextToken();

        beans.add( new CalculationBean( name, fileName, fromStation, toStation ) );
      }

      return beans.toArray( new CalculationBean[beans.size()] );
    }
    finally
    {
      LineIterator.closeQuietly( lineIt );
    }
  }

  public static void writeBerFile( final File outputFile, final CalculationBean[] calculations ) throws IOException
  {
    try( final PrintWriter pw = new PrintWriter( outputFile) )
    {
      pw.println( calculations.length );

      for( final CalculationBean calculation : calculations )
      {
        final String name = calculation.getName();
        final String shortName = StringUtils.abbreviateMiddle( name, ".", 57 ); //$NON-NLS-1$

        final BigDecimal fromStation = calculation.getFromStation();
        final BigDecimal toStation = calculation.getToStation();
        final String calcFilename = calculation.getFileName();

        pw.format( Locale.US, "%-57s %9.4f %9.4f  %12s%n", shortName, fromStation, toStation, calcFilename ); //$NON-NLS-1$
      }

      if( pw.checkError() )
        throw new IOException();

      pw.close();
    }
  }

  public ICalculationContentBean readContent( final TYPE projectType, final File profDir ) throws IOException
  {
    final File file = new File( profDir, getFileName() );

    switch( projectType )
    {
      case PASCHE:
        return CalculationContentBeanPasche.read( this, file );

      case KNAUF:
        return CalculationContentBeanKnauf.read( this, file );
    }

    throw new IllegalArgumentException();
  }

  /**
   * Creates a result filename from this filename by replacing character 3+4 with <code>code</code>.
   *
   * @param code
   *          2-character string.
   */
  public String getResultFilename( final String code )
  {
    final char[] fileNameChars = m_fileName.toCharArray();
    System.arraycopy( code.toCharArray(), 0, fileNameChars, 2, code.length() );
    return new String( fileNameChars );
  }
}