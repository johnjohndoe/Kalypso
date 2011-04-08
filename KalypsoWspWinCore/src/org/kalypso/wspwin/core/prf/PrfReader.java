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

import org.apache.commons.lang.StringUtils;
import org.kalypso.wspwin.core.i18n.Messages;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.DoubleDataBlock;
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

  private final int getDataSize(final int key , final int size )
  {
    if( key == IWspWinConstants.SPEZIALPROFIL_TRAPEZ )
      return 6;
    else if( key == IWspWinConstants.SPEZIALPROFIL_MAUL )
      return 5;
    else if( key == IWspWinConstants.SPEZIALPROFIL_EI )
      return 5;
    else if( key == IWspWinConstants.SPEZIALPROFIL_KREIS )
      return 4;
    else if( key == IWspWinConstants.SPEZIALPROFIL_SINUOSITAET )
      return 4;
    else
      return size;

  }

  private final int getDataBlockType( final int key )
  {
    if( key == IWspWinConstants.SPEZIALPROFIL_COMMENT )
      return IWspWinConstants.DATA_BLOCK_TYPE_TEXT;
    else if( key == IWspWinConstants.SPEZIALPROFIL_TEXT )
      return IWspWinConstants.DATA_BLOCK_TYPE_TEXT;
    else if( key >= IWspWinConstants.SPEZIALPROFIL_TRAPEZ && key <= IWspWinConstants.SPEZIALPROFIL_MAUL )
      return IWspWinConstants.DATA_BLOCK_TYPE_DOUBLE;
    else if( key == IWspWinConstants.SPEZIALPROFIL_SINUOSITAET )
      return IWspWinConstants.DATA_BLOCK_TYPE_DOUBLE;
    else if( key == 0 )
      return IWspWinConstants.DATA_BLOCK_TYPE_COORDINATE;
    return IWspWinConstants.DATA_BLOCK_TYPE_UNKNOWN;
  }

  private final IDataBlock createDataBlock( final DataBlockHeader dbh, final int dataBlockType )
  {
    if( dataBlockType == IWspWinConstants.DATA_BLOCK_TYPE_DOUBLE )
    {
      return new DoubleDataBlock( dbh );
    }
    else if( dataBlockType == IWspWinConstants.DATA_BLOCK_TYPE_COORDINATE )
    {
      return new CoordDataBlock( dbh );
    }
    else
    {
      return new TextDataBlock( dbh );
    }
  }

  public void readFromReader( final BufferedReader br ) throws IOException
  {
    readMetadata( br );

    final int[] pointCounts = parseLine14( br.readLine() );

    // Plotvorgaben ¸berlesen
    br.readLine();

    // jetzt die einzelnen Datenblˆcke laden
    for( final int pointCount : pointCounts )
    {
      try
      {
        final DataBlockHeader dbh = new DataBlockHeader( br );
        if( isValid( dbh ) )
        {
          final int key = dbh.getSpecification( 8 );
          final int dataBlockType = getDataBlockType(key);
          final IDataBlock dB = createDataBlock( dbh, dataBlockType );
          dB.readFromReader(getDataSize( key, pointCount ), br );
          m_dbs.put( createFirstLine( dbh.getFirstLine() ), dB );
        }
      }
      catch( final IOException e )
      {
        m_logger.log( Level.SEVERE, Messages.getString( "org.kalypso.wspwin.core.prf.PrfReader.0" ) ); //$NON-NLS-1$
        throw new IOException();
      }
    }
    br.close();
  }

  private boolean isValid( final DataBlockHeader dbh )
  {
    if( StringUtils.isBlank( dbh.getFirstLine() ) )
      return false;

    if( dbh.getSecondLine() == null )
      return false;

    if( StringUtils.isBlank( dbh.getThirdLine() ) )
      return false;

    return true;
  }

  public String createFirstLine( final String key )
  {
    if( key == null )
      return "Unbekannt"; //$NON-NLS-1$

    if( key.startsWith( "GEL" ) ) //$NON-NLS-1$
      return "GELAENDE-"; //$NON-NLS-1$
    if( key.startsWith( "TRENNF" ) ) //$NON-NLS-1$
      return "TRENNFLAECHEN"; //$NON-NLS-1$
    if( key.startsWith( "DUR" ) ) //$NON-NLS-1$
      return "DURCHSTROEMTE"; //$NON-NLS-1$
    if( key.startsWith( "RAU" ) ) //$NON-NLS-1$
      return "RAUHEIT"; //$NON-NLS-1$
    if( key.startsWith( "REC" ) ) //$NON-NLS-1$
      return "RECHTSWERT"; //$NON-NLS-1$
    if( key.startsWith( "HOC" ) ) //$NON-NLS-1$
      return "HOCHWERT"; //$NON-NLS-1$
    if( key.startsWith( "UK-B" ) ) //$NON-NLS-1$
      return "UK-BRUECKE"; //$NON-NLS-1$
    if( key.startsWith( "OK-B" ) ) //$NON-NLS-1$
      return "OK-BRUECKE"; //$NON-NLS-1$
    if( key.startsWith( "KOM" ) ) //$NON-NLS-1$
      return "KOMMENTAR:"; //$NON-NLS-1$
    if( key.startsWith( "BOR" ) ) //$NON-NLS-1$
      return "BORDVOLL"; //$NON-NLS-1$
    if( key.startsWith( "AX" ) ) //$NON-NLS-1$
      return "AX   m"; //$NON-NLS-1$
    if( key.startsWith( "AY" ) ) //$NON-NLS-1$
      return "AY   m"; //$NON-NLS-1$
    if( key.startsWith( "DP" ) ) //$NON-NLS-1$
      return "DP   m"; //$NON-NLS-1$
    if( key.startsWith( "EI" ) ) //$NON-NLS-1$
      return "EI"; //$NON-NLS-1$
    if( key.startsWith( "KRE" ) ) //$NON-NLS-1$
      return "KREIS"; //$NON-NLS-1$
    if( key.startsWith( "TRA" ) ) //$NON-NLS-1$
      return "TRAPEZ"; //$NON-NLS-1$
    if( key.startsWith( "MAU" ) ) //$NON-NLS-1$
      return "MAULPROFIL"; //$NON-NLS-1$
    if( key.startsWith( "OK-W" ) ) //$NON-NLS-1$
      return "OK-WEHR"; //$NON-NLS-1$
    if( key.startsWith( "TRENNL" ) ) //$NON-NLS-1$
      return "TRENNLINIE"; //$NON-NLS-1$

    return key.toUpperCase();
  }

  /**
   * Parst die 14.Zeile einer Profildatei als Array von ints
   */
  private static int[] parseLine14( final String string )
  {
    // es gibt zwei Formate f¸r diese Zeile:
    // 1. Format: Whitespace separated
    // 2. Format: MapperFormat
    // TODO: Kim Serializer:MapperFormat Anzahl ¸ber 999 Punkte lesen

    // 1. Format: Whitespace separated
    final StringTokenizer sT = (string == null) ? null : new StringTokenizer( string );
    final int count = (sT == null) ? -1 : sT.countTokens() - 1;
    if( (count < 0) || (count != Integer.parseInt( sT.nextToken() )) )
      m_logger.log( Level.SEVERE, Messages.getString( "org.kalypso.wspwin.core.prf.PrfReader.39" ) ); //$NON-NLS-1$

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
      m_logger.log( Level.SEVERE, Messages.getString( "org.kalypso.wspwin.core.prf.PrfReader.40" ) ); //$NON-NLS-1$
      throw new IOException();
    }

    for( int i = 1; i < 14; i++ )
    {
      final String line = r.readLine();
      if( line == null )
      {
        m_logger.log( Level.SEVERE, Messages.getString( "org.kalypso.wspwin.core.prf.PrfReader.41" ) ); //$NON-NLS-1$
        break;
      }

      // Linie nach Daten und Text trennen (max 40 Zeichen Text)
      final String textString = line.substring( 0, Math.min( 40, line.length() ) );
      textString.trim();
      final String dataString = line.length() > 40 ? line.substring( 40, line.length() ).trim() : ""; //$NON-NLS-1$
      m_metaMap.put( i, new String[] { textString, dataString } );

    }
  }

  public IDataBlock getDataBlock( final String key )
  {
    return m_dbs.get( createFirstLine( key.toUpperCase() ) );
  }

}