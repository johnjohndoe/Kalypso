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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.wspwin.core.prf.DataBlockWriter;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.LengthSectionDataBlock;

/**
 * @author kimwerner
 */
public class LengthSectionExporter
{
  private final HashMap<String, String[]> m_PropertyMap = new HashMap<String, String[]>();

  private final String blanc200;

  public LengthSectionExporter( )
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
}
