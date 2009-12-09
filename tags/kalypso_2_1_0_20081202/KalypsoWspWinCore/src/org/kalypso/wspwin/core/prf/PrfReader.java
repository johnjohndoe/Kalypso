/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.wspwin.core.prf;

import java.io.BufferedReader;
import java.io.IOException;
import java.text.NumberFormat;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;
import org.kalypso.wspwin.core.prf.datablock.TextDataBlock;

/**
 * Hat ein Profil pro Datei
 */
public class PrfReader
{
  private final static Logger m_logger = Logger.getLogger( PrfReader.class.getName() );

  /**
   * <p>
   * Format to print the station with
   * </p>
   * Decimal point is . and exactly 4 fraction digits
   */
  public final static NumberFormat STATION_FORMAT = NumberFormat.getNumberInstance( Locale.US );
  static
  {
    STATION_FORMAT.setMaximumFractionDigits( 4 );
    STATION_FORMAT.setMinimumFractionDigits( 4 );
  }

  private final Map<String, IDataBlock> m_dbs = new HashMap<String, IDataBlock>();

  private final Map<Integer, String[]> m_metaMap = new HashMap<Integer, String[]>();

  public final String[] getKeyValue( final int index )
  {
    return m_metaMap.get( index );
  }

  public void readFromReader( final BufferedReader br ) throws IOException
  {
    readMetadata( br );

    final int[] pointCounts = parseLine14( br.readLine() );

    // Plotvorgaben �berlesen
    br.readLine();

    // jetzt die einzelnen Datenbl�cke laden
    for( int i = 0; i < pointCounts.length; i++ )
    {
      try
      {
        final DataBlockHeader dbh = new DataBlockHeader( br );
        final int int8 = dbh.getSpecification( 8 );
        if( int8 > 0 )
        {
          final IDataBlock dB = new TextDataBlock( dbh );
          dB.readFromReader( pointCounts[i], br );
          m_dbs.put( createFirstLine( dbh.getFirstLine() ), dB );
        }
        else
        {
          final IDataBlock dB = new CoordDataBlock( dbh );
          dB.readFromReader( pointCounts[i], br );
          m_dbs.put( createFirstLine( dbh.getFirstLine() ), dB );
        }
      }
      catch( IOException e )
      {
        m_logger.log( Level.SEVERE, "Weniger Datenbl�cke in Zeile 14 angegeben als tats�chlich vorhanden" );
        throw new IOException();
      }
    }
    br.close();
  }

  public String createFirstLine( final String key )
  {
    if( key.startsWith( "GEL" ) )
      return "GELAENDE-";
    if( key.startsWith( "TRENNF" ) )
      return "TRENNFLAECHEN";
    if( key.startsWith( "DUR" ) )
      return "DURCHSTROEMTE";
    if( key.startsWith( "RAU" ) )
      return "RAUHEIT";
    if( key.startsWith( "REC" ) )
      return "RECHTSWERT";
    if( key.startsWith( "HOC" ) )
      return "HOCHWERT";
    if( key.startsWith( "UK-B" ) )
      return "UK-BRUECKE";
    if( key.startsWith( "OK-B" ) )
      return "OK-BRUECKE";
    if( key.startsWith( "KOM" ) )
      return "KOMMENTAR:";
    if( key.startsWith( "BOR" ) )
      return "BORDVOLL";
    if( key.startsWith( "AX" ) )
      return "AX   m";
    if( key.startsWith( "AY" ) )
      return "AY   m";
    if( key.startsWith( "DP" ) )
      return "DP   m";
    if( key.startsWith( "EI" ) )
      return "EI";
    if( key.startsWith( "KRE" ) )
      return "KREIS";
    if( key.startsWith( "TRA" ) )
      return "TRAPEZ";
    if( key.startsWith( "MAU" ) )
      return "MAULPROFIL";
    if( key.startsWith( "OK-W" ) )
      return "OK-WEHR";
    if( key.startsWith( "TRENNL" ) )
      return "TRENNLINIE";

    return key.toUpperCase();
  }

  /**
   * Parst die 14.Zeile einer Profildatei als Array von ints
   */
  private static int[] parseLine14( final String string )
  {
    // es gibt zwei Formate f�r diese Zeile:
    // 1. Format: Whitespace separated
    // 2. Format: MapperFormat
    // TODO: Kim Serializer:MapperFormat Anzahl �ber 999 Punkte lesen

    // 1. Format: Whitespace separated
    final StringTokenizer sT = (string == null) ? null : new StringTokenizer( string );
    final int count = (sT == null) ? -1 : sT.countTokens() - 1;
    if( (count < 0) || (count != Integer.parseInt( sT.nextToken() )) )
      m_logger.log( Level.SEVERE, "Format der Zeile 14 Falsch" );

    final int[] counts = new int[count];
    for( int i = 0; i < count; i++ )
      counts[i] = Integer.parseInt( sT.nextToken() );

    return counts;
  }

  public Map<Integer, String[]> getMetaData( )
  {
    return m_metaMap;
  }

  /**
   * Liest die ersten 13 Zeilen einer Profildatei
   */
  private final void readMetadata( final BufferedReader r ) throws IOException
  {
    // final Map<Integer, DataString> metaMap = new HashMap<Integer, DataString>();

    // final ArrayList<String> metaStrings = new ArrayList<String>();

    if( !r.ready() )
    {
      m_logger.log( Level.SEVERE, "Fehler beim �ffnen der Profildatei." );
      throw new IOException();
    }

    for( int i = 1; i < 14; i++ )
    {
      final String line = r.readLine();
      if( line == null )
      {
        m_logger.log( Level.SEVERE, "Header der Profildatei falsch." );
        break;
      }

      // Linie nach Daten und Text trennen (max 40 Zeichen Text)
      final String textString = line.substring( 0, Math.min( 40, line.length() ) );
      textString.trim();
      final String dataString = line.length() > 40 ? line.substring( 40, line.length() ).trim() : "";
      m_metaMap.put( i, new String[] { textString, dataString } );

    }
  }

  // public IResultSet createResults( )
  // {
  // if( m_dbs.size() == 0 )
  // return null;
  //
  // String name = null;
  // for( final IDataBlock db : m_dbs )
  // {
  // name = parseLaengsschnittName( db.getSecondLine() );
  // if( name != null && name.length() > 0 )
  // break;
  // }
  //
  // if( name == null )
  // name = "<unbekannt>";
  //
  // final Result result = new Result( name );
  //
  // for( final IDataBlock db : m_dbs )
  // {
  // if( db instanceof IResultDataBlock )
  // ((IResultDataBlock) db).fillResult( result );
  // }
  //
  // return result;
  // }

  // private String parseLaengsschnittName( final String secondName )
  // {
  // final String name;
  // if( secondName.length() > 99 )
  // name = secondName.substring( 99 ).trim();
  // else
  // name = secondName.trim();
  //
  // final int index = name.indexOf( '@' );
  // if( index == -1 )
  // return name;
  //
  // return name.substring( 0, index );
  // }
  public IDataBlock getDataBlock( final String key )
  {
    return m_dbs.get( createFirstLine( key.toUpperCase() ) );
  }

}