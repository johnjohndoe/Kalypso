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
package org.kalypso.model.wspm.tuhh.core.wspwin.strang;

import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;

import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.wspwin.core.prf.DataBlockWriter;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.LengthSectionDataBlock;

import au.com.bytecode.opencsv.CSVReader;

/**
 * @author kimwerner
 */
public class LngSink implements IProfilSink
{

  private final HashMap<String, String> m_idMap = new HashMap<String, String>();

  public LngSink( )
  {
    m_idMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION, "STATION" );
    m_idMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND, "SOHLHOEHE" );
    m_idMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BOE_LI, "BOESCHUNG-LI" );
    m_idMap.put( IWspmConstants.LENGTH_SECTION_PROPERT_BOE_RE, "BOESCHUNG-RE" );
   // m_idMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_WEIR_OK, "WEIR_OK" );
    m_idMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK, "DECKENOBERK" );
    m_idMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK, "DECKENUNTERK" );
   // m_idMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_WIDTH, "BRIDGE_WIDTH" );
   // m_idMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_ROHR_DN, "ROHR_DN" );
    m_idMap.put( IWspmConstants.POINT_PROPERTY_COMMENT, "TEXT" );
  }

  final DataBlockHeader createHeader( final String id )
  {
    final DataBlockHeader dbh = new DataBlockHeader();
    final String fl = m_idMap.get( id );
    dbh.setFirstLine( fl == null ? id : fl );
    if( "TEXT".equalsIgnoreCase( fl ) )
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
      throw new IOException( "keine Spalte f�r Station angegeben" );

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

  @SuppressWarnings("unchecked")
  private DataBlockWriter extractDataBlocks( final IObservation<TupleResult> obs )
  {
    final DataBlockWriter prfwriter = new DataBlockWriter();

    // Get TableData
    final Object[] table = new Object[obs.getResult().getComponents().length];
    for( int i = 0; i < obs.getResult().getComponents().length; i++ )
    {
      table[i] = new ArrayList<Object>();
    }
    for( final IRecord values : obs.getResult() )
    {
      for( int i = 0; i < obs.getResult().getComponents().length; i++ )
      {
        final Object oVal = values.getValue( i );
        ((ArrayList<Object>) table[i]).add( oVal instanceof BigDecimal ? ((BigDecimal) oVal).doubleValue() : oVal );

      }
    }

    // Add DataBlocks
    for( int i = 0; i < obs.getResult().getComponents().length; i++ )
    {
      if( i != obs.getResult().indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION ) )
      {
        final DataBlockHeader dbh = createHeader( obs.getResult().getComponent( i ).getId() );

        final LengthSectionDataBlock block = new LengthSectionDataBlock( dbh );
        Object object = table[obs.getResult().indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION )];
        block.setCoords( ((ArrayList< ? >) object).toArray( new Double[] {} ), ((ArrayList) table[i]).toArray() );
        if( block.getCoordCount() > 0 )
          prfwriter.addDataBlock( block );
      }
    }
    return prfwriter;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(java.lang.Object, java.io.Writer)
   */
  @SuppressWarnings("unchecked")
  @Override
  public boolean write( Object source, Writer writer )
  {
    DataBlockWriter dataBlockWriter = null;
    if( source instanceof IObservation< ? > )
      dataBlockWriter = extractDataBlocks( (IObservation<TupleResult>) source );
    if( source instanceof String )
      try
      {
        dataBlockWriter = extractDataBlocks( new FileReader( source.toString() ), -1, ';' );
      }
      catch( Exception e )
      {
        return false;
      }
    if( dataBlockWriter == null )
      return false;
    if( writer instanceof PrintWriter )
      dataBlockWriter.store( (PrintWriter) writer );
    else
      dataBlockWriter.store( new PrintWriter( writer ) );
    return true;
  }
}
