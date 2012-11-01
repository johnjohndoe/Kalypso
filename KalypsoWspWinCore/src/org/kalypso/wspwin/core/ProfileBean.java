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

import java.io.IOException;
import java.io.LineNumberReader;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.StringTokenizer;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.wspwin.core.i18n.Messages;

/**
 * Represents a line from a profproj.txt or .str file
 * @author Belger
 */
public class ProfileBean
{
  static final int MAX_WATERNAME_LENGTH = 9;

  static final int MAX_STATENAME_LENGTH = 10;

  public static final String DEFAULT_MFB = "0"; //$NON-NLS-1$

  public static final int DEFAULT_VZK = 0;

  private final String m_waterName;

  private final String m_stateName;

  private final BigDecimal m_station;

  private final String m_fileName;

  private final String m_mehrfeldCode;

  private final int m_vzk;

  public ProfileBean( final String waterName, final String stateName, final BigDecimal station, final String fileName )
  {
    this( waterName, stateName, station, fileName, DEFAULT_MFB, DEFAULT_VZK );
  }

  public ProfileBean( final String waterName, final String stateName, final BigDecimal station, final String fileName, final String mehrfeldCode, final int vzk )
  {
    m_waterName = waterName;
    m_stateName = stateName;
    m_station = station;
    m_fileName = fileName;
    m_mehrfeldCode = mehrfeldCode;
    m_vzk = vzk;
  }

  public String getFileName( )
  {
    return m_fileName;
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }

  public String getWaterName( )
  {
    return m_waterName;
  }

  public String getStateName( )
  {
    return m_stateName;
  }

  public String getMehrfeldCode( )
  {
    return m_mehrfeldCode;
  }

  public int getVzk( )
  {
    return m_vzk;
  }

  public static ProfileBean[] readProfiles( final LineNumberReader reader, final int profilCount ) throws IOException, ParseException
  {
    final List<ProfileBean> beans = new ArrayList<>( 20 );
    for( int i = 0; i < profilCount; i++ )
    {
      if( !reader.ready() )
        throw new ParseException( Messages.getString("org.kalypso.wspwin.core.ProfileBean.0") + reader.getLineNumber(), reader.getLineNumber() ); //$NON-NLS-1$

      final String line = reader.readLine();
      if( line == null || line.trim().length() == 0 )
        throw new ParseException( Messages.getString("org.kalypso.wspwin.core.ProfileBean.1") + reader.getLineNumber(), reader.getLineNumber() ); //$NON-NLS-1$

      final StringTokenizer tokenizer = new StringTokenizer( line );
      if( tokenizer.countTokens() != 6 )
        throw new ParseException( Messages.getString("org.kalypso.wspwin.core.ProfileBean.2") + reader.getLineNumber(), reader.getLineNumber() ); //$NON-NLS-1$

      try
      {
        final String waterName = tokenizer.nextToken();
        final BigDecimal station = new BigDecimal( tokenizer.nextToken() );
        final String mfb = tokenizer.nextToken(); // Mehrfeldbr¸ckenkennung
        final int vzk = parseVZK( tokenizer.nextToken() ); // Verzweigungskennung
        final String zustandName = tokenizer.nextToken();
        final String fileName = tokenizer.nextToken();

        final ProfileBean bean = new ProfileBean( waterName, zustandName, station, fileName, mfb, vzk );
        beans.add( bean );
      }
      catch( final NumberFormatException e )
      {
        throw new ParseException( Messages.getString("org.kalypso.wspwin.core.ProfileBean.6") + reader.getLineNumber(), reader.getLineNumber() ); //$NON-NLS-1$
      }
    }

    return beans.toArray( new ProfileBean[beans.size()] );
  }

  private static int parseVZK( final String token )
  {
    try
    {
      return Integer.parseInt( token.trim() );
    }
    catch( final NumberFormatException e )
    {
      e.printStackTrace();
      return 0;
    }
  }

  public String formatProfprojLine( )
  {
    final String waterName = shortenName( getWaterName(), MAX_WATERNAME_LENGTH );
    final String stateName = shortenName( getStateName(), MAX_STATENAME_LENGTH );

    return String.format( Locale.US, "%-9s%9.4f %8s %4d %-10s %-12s", waterName, m_station, m_mehrfeldCode, m_vzk, stateName, m_fileName ); //$NON-NLS-1$
  }

  public String formatStrLine( )
  {
    final String waterName = shortenName( getWaterName(), 7 );
    final String stateName = shortenName( getStateName(), MAX_STATENAME_LENGTH );

    String mehrFeldString;
    if( "00".equals( m_mehrfeldCode ) ) //$NON-NLS-1$
      mehrFeldString = "0"; //$NON-NLS-1$
    else
      mehrFeldString = m_mehrfeldCode;

    return String.format( Locale.US, "%-9s%9.4f  %-4s     %-3d %-10s %-12s", waterName, m_station, mehrFeldString, m_vzk, stateName, m_fileName ); //$NON-NLS-1$
  }

  public static String shortenName( final String name, final int maxLength )
  {
    final String noSpaces = StringUtils.remove( name, ' ' ); //$NON-NLS-1$
    return StringUtils.abbreviateMiddle( noSpaces, "_", maxLength ); //$NON-NLS-1$
  }
}