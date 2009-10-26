/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraï¿½e 22
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
package org.kalypso.model.wspm.tuhh.core.wspwin.strang;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.ArrayList;

import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.wspwin.core.prf.DataBlockWriter;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.LengthSectionDataBlock;

import au.com.bytecode.opencsv.CSVReader;

/**
 * @author kimwerner
 */
public class LngSink
{
  private final PrintWriter m_writer;

  private final Reader m_reader;

  private DataBlockWriter m_dataBlockWriter;

  public LngSink( final String source, final String destination ) throws IOException
  {
    m_reader = new FileReader( source );
    m_writer = new PrintWriter( new FileOutputStream( new File( destination ) ) );
  }

  public LngSink( final Reader source, final PrintWriter destination )
  {
    m_reader = source;
    m_writer = destination;

  }

  final DataBlockHeader createHeader( final String id )
  {
    final DataBlockHeader dbh = new DataBlockHeader();
    dbh.setFirstLine( id );
    if( "TEXT".equalsIgnoreCase( id ) )
    {
      dbh.setThirdLine( " 0  0  0  0  0  0  0  0 12" );
    }
    return dbh;
  }

  @SuppressWarnings("unchecked")
  private DataBlockWriter extractDataBlocks( final Reader reader, final int colStation, final char separator ) throws IOException
  {
    final DataBlockWriter prfwriter = new DataBlockWriter();
    final CSVReader tableReader = new CSVReader( reader, separator );
    String[] line = tableReader.readNext();

    // Get MetaData
    int nbr = 0;
    while( line != null && line[0].startsWith( "#" ) )
    {
      line[0] = line[0].substring( 1 );
      prfwriter.addKeyValue( nbr++, line );
      line = tableReader.readNext();
    }
    // Get ColumnData
    final String[] col1 = line;
    final String[] col2 = tableReader.readNext();
    int colStat = colStation;
    if( colStat < 0 )
    {
      for( int i = 0; i < col1.length; i++ )
      {
        {
          if( "STATION".equalsIgnoreCase( col1[i] ) )
          {
            colStat = i;
            break;
          }
        }
      }
    }
    if( colStat < 0 )
      throw new IOException( "keine Spalte für Station angegeben" );

    // Get TableData
    final Object[] table = new Object[col1.length];
    for( int i = 0; i < col1.length; i++ )
    {
      table[i] = new ArrayList<Object>();
    }
    String[] values = tableReader.readNext();
    while( values != null )
    {
      if( values.length == col1.length )
      {
        for( int i = 0; i < values.length; i++ )
        {
          final Double dbl = NumberUtils.parseQuietDouble( values[i].toString() );
          if( dbl.isNaN() )
            ((ArrayList<Object>) table[i]).add( values[i] );
          else
            ((ArrayList<Object>) table[i]).add( dbl );
        }
      }
      values = tableReader.readNext();
    }

    // Add DataBlocks
    for( int i = 0; i < col1.length; i++ )
    {
      if( i != colStat )
      {
        final DataBlockHeader dbh = createHeader( col1[i] );

        if( i < col2.length )
          dbh.setSecondLine( col2[i] );

        final LengthSectionDataBlock block = new LengthSectionDataBlock( dbh );
        block.setCoords( ((ArrayList<Double>) table[colStat]).toArray( new Double[] {} ), ((ArrayList) table[i]).toArray() );
        prfwriter.addDataBlock( block );
      }
    }
    return prfwriter;
  }

  public void read( final char separator, final int colStation ) throws IOException
  {
    m_dataBlockWriter = extractDataBlocks( m_reader, colStation, separator );
  }

  public void read( final char separator ) throws IOException
  {
    m_dataBlockWriter = extractDataBlocks( m_reader, -1, separator );
  }

  public void read( ) throws IOException
  {
    m_dataBlockWriter = extractDataBlocks( m_reader, -1, ';' );
  }

  public void write( )
  {
    m_dataBlockWriter.store( m_writer );
  }
}
