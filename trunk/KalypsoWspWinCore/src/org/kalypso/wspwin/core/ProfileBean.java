/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
  private final String m_waterName;
  private final String m_stateName;
  private final double m_station;
  private final String m_fileName;

  public ProfileBean( final String waterName, final String stateName, final double station, final String fileName )
  {
    m_waterName = waterName;
    m_stateName = stateName;
    m_station = station;
    m_fileName = fileName;
  }

  public String getFileName( )
  {
    return m_fileName;
  }

  public double getStation( )
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

  public static ProfileBean[] readProfiles( final LineNumberReader reader, final int profilCount ) throws IOException, ParseException
  {
    final List<ProfileBean> beans = new ArrayList<ProfileBean>( 20 );
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
        final double station = Double.parseDouble( tokenizer.nextToken() );
        /* final String vzk = */tokenizer.nextToken(); // Verzweigungskennung
        /* final String mfb = */tokenizer.nextToken(); // Mehrfeldbrückenkennung
        final String zustandName = tokenizer.nextToken();
        final String fileName = tokenizer.nextToken();

        final ProfileBean bean = new ProfileBean( waterName, zustandName, station, fileName );
        beans.add( bean );
      }
      catch( final NumberFormatException e )
      {
        throw new ParseException( Messages.getString("org.kalypso.wspwin.core.ProfileBean.6") + reader.getLineNumber(), reader.getLineNumber() ); //$NON-NLS-1$
      }
    }

    return beans.toArray( new ProfileBean[beans.size()] );
  }

  public String formatLine( )
  {
    final String waterName = shortenName( getWaterName() );
    final String stateName = shortenName( getStateName() );

    return String.format( Locale.US, "%-9s%9.4f 0        0    %-10s %-12s", waterName, m_station, stateName, m_fileName );
  }

  public static String shortenName( final String name )
  {
    final String noSpaces = StringUtils.remove( name, ' ' ); //$NON-NLS-1$
    return StringUtils.abbreviateMiddle( noSpaces, "_", 10 ); //$NON-NLS-1$
  }
}