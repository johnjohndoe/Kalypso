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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.tuhh.core.profile.WspmTuhhProfileHelper;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.wspwin.core.prf.DataBlockWriter;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.LengthSectionDataBlock;

/**
 * TODO: check, why is most of the code unused? Delete commented stuff
 * 
 * @author kimwerner TODO: split in two classes: Observation <-> file
 */
public class LngSink implements IProfilSink
{
  private final HashMap<String, String[]> m_PropertyMap = new HashMap<String, String[]>();

// private final HashMap<String, Object[]> m_StrangTable = new HashMap<String, Object[]>();

// private String[] m_columns = null;
//
// private String m_sourceDir = null;
//
// private int m_colStation = -1;
//
// private int m_colStrang = -1;
//
// private final String[] m_propertyKeys;
//
// private HashMap<String, DataBlockHeader> m_dataBlockHeader = new HashMap<String, DataBlockHeader>();

  private final String blanc200;

  public LngSink( )
  {
    m_PropertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION, new String[] { "STATION" } ); //$NON-NLS-1$
    m_PropertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND, new String[] { "SOHLHOEHE" } ); //$NON-NLS-1$
    m_PropertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BOE_LI, new String[] { "BOESCHUNG-LI" } ); //$NON-NLS-1$
    m_PropertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERT_BOE_RE, new String[] { "BOESCHUNG-RE" } ); //$NON-NLS-1$
    m_PropertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_WEIR_OK, new String[] { "OK-WEHRS" } ); //$NON-NLS-1$
    m_PropertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK, new String[] { "DECKENOBERK" } ); //$NON-NLS-1$
    m_PropertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK, new String[] { "DECKENUNTERK" } ); //$NON-NLS-1$
    // m_PropertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_WIDTH, "BRIDGE_WIDTH" );
    // m_PropertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_ROHR_DN, "ROHR_DN" );
    m_PropertyMap.put( IWspmConstants.POINT_PROPERTY_COMMENT, new String[] { "TEXT", "", " 0  0  0  0  0  0  0  0 12" } ); //$NON-NLS-1$
    m_PropertyMap.put( "TEXT", new String[] { "TEXT", "", " 0  0  0  0  0  0  0  0 12" } ); //$NON-NLS-1$
    m_PropertyMap.put( IWspmConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL, new String[] { "Wasserspiegel NN+m" } );//$NON-NLS-1$

    final char[] space = new char[200];
    Arrays.fill( space, ' ' );//$NON-NLS-1$
    blanc200 = new String( space );

//    m_propertyKeys = new String[] { "Zeile1_unbekannt",//$NON-NLS-1$
//        "Auftraggeber_1",//$NON-NLS-1$
//        "Auftraggeber_2",//$NON-NLS-1$
//        "Projektbezeichnung_1",//$NON-NLS-1$
//        "Projektbezeichnung_2",//$NON-NLS-1$
//        "Projektbezeichnung_3",//$NON-NLS-1$
//        "Blattbezeichnung_1",//$NON-NLS-1$
//        "Blattbezeichnung_2",//$NON-NLS-1$
//        "Blattbezeichnung_3",//$NON-NLS-1$
//        "Projektnummer",//$NON-NLS-1$
//        "Zeile11_unbekannt",//$NON-NLS-1$
//        "Blattnummer",//$NON-NLS-1$
//        "Zeichnungsueberschrift" };//$NON-NLS-1$
  }

  final DataBlockHeader createHeader( final String id )
  {
    final DataBlockHeader dbh = new DataBlockHeader();
    final String[] fl = m_PropertyMap.get( id );
    if( fl == null )
      return null;

    dbh.setFirstLine( fl.length > 0 ? fl[0] : id );
    dbh.setSecondLine( fl.length > 1 ? blanc200 + fl[1] + "@" : "" );//$NON-NLS-1$
    dbh.setThirdLine( fl.length > 2 ? fl[2] : " 0  0  0  0  0  0  0  0  0" );//$NON-NLS-1$
    return dbh;
  }

// private Object[] getStrangTable( final String id )
// {
// if( m_StrangTable.containsKey( id ) )
// return m_StrangTable.get( id );
// final Object[] table = new Object[m_columns.length];
// for( int i = 0; i < m_columns.length; i++ )
// {
// table[i] = new ArrayList<Object>();
// }
// m_StrangTable.put( id, table );
// return table;
// }

// private final void addMetaData( final DataBlockWriter dbw, final String key ) throws IOException
// {
//    final File f = new File( m_sourceDir + key + ".properties" );//$NON-NLS-1$
// if( !f.exists() )
// return;
//
// final SortedProperties props = new SortedProperties();
// props.load( new FileReader( f ) );
//
// for( int i = 0; i < m_propertyKeys.length; i++ )
// {
//      dbw.addKeyValue( i + 1, new String[] { props.getProperty( m_propertyKeys[i], "" ), "" } );//$NON-NLS-1$ //$NON-NLS-2$
// }
// }

// private int getColumnIndex( final String id )
// {
// for( int i = 0; i < m_columns.length; i++ )
//      if( id.equalsIgnoreCase( m_columns[i] ) ) //$NON-NLS-1$
// return i;
// return -1;
// }

// private void extractDataBlocks( final String sourcePath, final int colStation, final int colStrang, final char
  // separator ) throws IOException
// {
// m_sourceDir = sourcePath.substring( 0, sourcePath.lastIndexOf( File.separatorChar ) + 1 );
// final CSVReader tableReader = new CSVReader( new FileReader( sourcePath ), separator );
//
// // Get ColumnData
// m_columns = tableReader.readNext();// line;
//
// final String[] col2 = tableReader.readNext();
// if( colStation < 0 )
// m_colStation = getColumnIndex( "STATION" );
//
// if( colStrang < 0 )
// m_colStrang = getColumnIndex( "ID" );
//
// // m_colGround = getColumnIndex( "SOHLHOEHE" );
//
// if( m_colStation < 0 )
//      throw new IOException( Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.strang.LngSink_0" ) ); //$NON-NLS-1$
//
// // Get TableData
//
// String[] values = tableReader.readNext();
// while( values != null )
// {
// if( values.length == m_columns.length )
// {
// final String key = m_colStation < 0 ? "-" : values[m_colStation];
// final Object[] table = getStrangTable( key );
//
// for( int i = 0; i < values.length; i++ )
// {
// final Double dbl = NumberUtils.parseQuietDouble( values[i].toString() );
// if( dbl.isNaN() )
// ((ArrayList<Object>) table[i]).add( values[i] );
// else
// ((ArrayList<Object>) table[i]).add( dbl );
// }
// }
// values = tableReader.readNext();
// }
//
// // Add DataBlockHeader
//
// for( int i = 0; i < m_columns.length; i++ )
// {
// if( i != m_colStation && i != m_colStrang )
// {
// final DataBlockHeader dbh = m_PropertyMap.get( m_columns[i] ) == null ? new DataBlockHeader( m_columns[i] ) :
  // createHeader( m_columns[i] );
// if( i < col2.length )
// {
// dbh.setSecondLine( blanc200 + col2[i] );
// }
// m_dataBlockHeader.put( m_columns[i], dbh );
// //
// // final LengthSectionDataBlock block = new LengthSectionDataBlock( dbh );
// // block.setCoords( ((ArrayList<Double>) table[colStat]).toArray( new Double[] {} ), ((ArrayList) table[i]).toArray()
  // );
// // prfwriter.addDataBlock( block );
// // }
// // }
// }
// }
// }

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

    final int posGround = obs.getResult().indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND );
    // Add DataBlocks
    for( int i = 0; i < obs.getResult().getComponents().length; i++ )
    {
      if( i != obs.getResult().indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION ) )
      {
        final DataBlockHeader dbh = createHeader( obs.getResult().getComponent( i ).getId() );
        if( dbh == null )
          continue;
        final LengthSectionDataBlock block = new LengthSectionDataBlock( dbh );
        final Object object = table[obs.getResult().indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION )];
        block.setCoords( ((ArrayList< ? >) object).toArray( new Double[] {} ), ((ArrayList) table[i]).toArray() );
        if( i == posGround )
          prfwriter.addDataBlock( 0, block );// due restrictions of wspwin-plotter
        else if( block.getCoordCount() > 0 )
          prfwriter.addDataBlock( block );
      }
    }
    return prfwriter;
  }

  public final boolean write( final IObservation<TupleResult> obs, final Writer writer ) throws IOException
  {
    final DataBlockWriter dbw = extractDataBlocks( obs );
    dbw.store( new PrintWriter( writer ) );
    return true;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(java.lang.Object, java.io.Writer)
   */

  public boolean internalWrite( final IObservation<TupleResult> obs, final Writer writer ) throws IOException
  {

    final DataBlockWriter dbw = extractDataBlocks( obs );
    dbw.store( new PrintWriter( writer ) );
    return true;

// extractDataBlocks( source.toString(), -1, -1, ';' );
//
// for( final String key : m_StrangTable.keySet() )
// {
// final Object[] table = getStrangTable( key );
// final DataBlockWriter dbw = new DataBlockWriter();
//
// // add Zeile 1-14
//
// addMetaData( dbw, key );
//
// // // Add DataBlocks
// for( int i = 0; i < m_columns.length; i++ )
// {
// if( i != m_colStation )
// {
// final LengthSectionDataBlock block = new LengthSectionDataBlock( m_dataBlockHeader.get( m_columns[i] ) );
// block.setCoords( ((ArrayList<Double>) table[m_colStation]).toArray( new Double[] {} ), ((ArrayList)
    // table[i]).toArray() );
// if( i == m_colGround )
// dbw.addDataBlock( 0, block );
// else
// dbw.addDataBlock( block );
// }
// }
//
// if( writer != null )
// dbw.store( new PrintWriter( writer ) );
// else
// {
// dbw.store( new PrintWriter( m_sourceDir + key + ".lng" ) );
// }
// }
// return true;
// }
// catch( Exception e )
// {
// e.printStackTrace();
// return false;
// }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(java.lang.Object, java.io.Writer)
   */
  @SuppressWarnings("unchecked")
  @Override
  public boolean write( final Object source, final Writer writer ) throws IOException
  {
    // FIXME: no! move common code in a separate class. A "IProfilSink" should always only write profiles!
    if( source instanceof IObservation< ? > )
      return internalWrite( (IObservation<TupleResult>) source, writer );
    else if( source instanceof IProfil[] )
    {
      final IObservation<TupleResult> obs = WspmTuhhProfileHelper.profilesToLengthSection( (IProfil[]) source );
      return internalWrite( obs, writer );
    }
    else
      throw new IOException( "illegal Argument", new IllegalArgumentException( source.toString() ) );
  }
}
