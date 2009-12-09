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
package org.kalypso.wspwin.core.prf;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;

public class PrfWriter
{
  private final Logger m_logger = Logger.getLogger( PrfWriter.class.getName() );

  private final ArrayList<IDataBlock> m_dbs = new ArrayList<IDataBlock>();

  private final Map<Integer, String[]> m_metaMap = new HashMap<Integer, String[]>();

  public void addKeyValue( final int lineNr, final String[] data )
  {
    m_metaMap.put( lineNr, data );
  }

  public void addDataBlock( final IDataBlock dataBlock )
  {
    m_dbs.add( dataBlock );
  }

  public void store( final PrintWriter pw )
  {
    writeMetadata( pw );
    writeZeile14( m_dbs, pw );
    pw.println( "0.0000  0.0000  0.0000  0.0000  0.0000  0.0000   0 0 0" );// Plotvorgaben
    try
    {
      for( final IDataBlock db : m_dbs )
        db.printToPrinter( pw );
    }
    catch( final IOException e )
    {
      m_logger.log( Level.SEVERE, e.getMessage() );
      e.printStackTrace();
    }
    finally
    {
      pw.close();
    }
  }

  /**
   * Schreibt die Anzahlen der Datenblöcke als Zeile 14 raus
   */
  private void writeZeile14( final List< ? extends IDataBlock> dbs, final PrintWriter pw )
  {
    pw.print( dbs.size() );
    for( final IDataBlock dataBlock : dbs )
    {
      pw.print( ' ' );
      final int i = dataBlock.getCoordCount();
      pw.print( i );
    }
    pw.println();
  }

  public static final DataBlockHeader createHeader( final String key )

  {
    final DataBlockHeader dbh = new DataBlockHeader();

    if( key.startsWith( "GEL" ) )
    {
      dbh.setFirstLine( "GELAENDE-" );
      dbh.setSecondLine( "HOEHE" );
    }
    else if( key.startsWith( "TRENNF" ) )
    {
      dbh.setFirstLine( "TRENNFLAECHEN" );
    }
    else if( key.startsWith( "DUR" ) )
    {
      dbh.setFirstLine( "DURCHSTROEMTE" );
      dbh.setSecondLine( "BEREICHE" );
    }
    else if( key.startsWith( "KST" ) )
    {
      dbh.setFirstLine( "RAUHEIT" );
      dbh.setSecondLine( "kst   m" );
    }
    else if( key.startsWith( "KS" ) )
    {
      dbh.setFirstLine( "RAUHEIT" );
      dbh.setSecondLine( "k-s   m" );
    }
    else if( key.startsWith( "REC" ) )
    {
      dbh.setFirstLine( "RECHTSWERT" );
    }
    else if( key.startsWith( "HOC" ) )
    {
      dbh.setFirstLine( "HOCHWERT" );
    }
    else if( key.startsWith( "UK-B" ) )
    {
      dbh.setFirstLine( "UK-BRUECKE" );
    }
    else if( key.startsWith( "OK-B" ) )
    {
      dbh.setFirstLine( "OK-BRUECKE" );
    }
    else if( key.startsWith( "KOM" ) )
    {
      dbh.setFirstLine( "KOMMENTAR:" );
    }
    else if( key.startsWith( "BOR" ) )
    {
      dbh.setFirstLine( "BORDVOLL" );
    }
    else if( key.startsWith( "AX" ) )
    {
      dbh.setFirstLine( "AX   m" );
    }
    else if( key.startsWith( "AY" ) )
    {
      dbh.setFirstLine( "AY   m" );
    }
    else if( key.startsWith( "DP" ) )
    {
      dbh.setFirstLine( "DP   m" );
    }
    else if( key.startsWith( "EI" ) )
    {
      dbh.setFirstLine( "EI" );
    }
    else if( key.startsWith( "KRE" ) )
    {
      dbh.setFirstLine( "KREIS" );
    }
    else if( key.startsWith( "TRA" ) )
    {
      dbh.setFirstLine( "TRAPEZ" );
    }
    else if( key.startsWith( "MAU" ) )
    {
      dbh.setFirstLine( "MAULPROFIL" );
    }
    else if( key.startsWith( "OK-W" ) )
    {
      dbh.setFirstLine( "OK-WEHR" );
    }
    else if( key.startsWith( "TRENNL" ) )
    {
      dbh.setFirstLine( "TRENNLINIE" );
      dbh.setSecondLine( "WEHR" );
    }

    return dbh;
  }

  private void writeMetadata( final PrintWriter pw )
  {
    for( int i = 1; i < 14; i++ )
    {
      final String[] line = m_metaMap.get( i );
      final StringBuffer buffer = new StringBuffer( "#" );
      buffer.append( line[0] );

      if( buffer.length() > 41 )
        buffer.setLength( 41 );
      else
      {
        final int l = 41 - buffer.length();
        final char[] space = new char[l];
        Arrays.fill( space, ' ' );

        buffer.append( space );
      }
      buffer.append( line[1] );
      pw.println( buffer.toString().trim().substring( 1 ) );
    }
  }
}