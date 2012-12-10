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
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ObjectUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.LengthSectionMapping;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.wspwin.core.prf.DataBlockWriter;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;
import org.kalypso.wspwin.core.prf.datablock.LengthSectionDataBlock;
import org.kalypso.wspwin.core.prf.datablock.LengthSectionTextBlock;

/**
 * @author kimwerner
 */
public class LengthSectionExporter
{
  private DataBlockWriter extractDataBlocks( final IObservation<TupleResult> obs )
  {
    final DataBlockWriter prfwriter = new DataBlockWriter();

    // Get TableData
    final TupleResult result = obs.getResult();

    final int posGround = result.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND );
    final int stationIndex = result.indexOfComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION );

    for( int i = 0; i < result.getComponents().length; i++ )
    {
      if( i == stationIndex )
        continue;

      final DataBlockHeader dbh = LengthSectionMapping.createHeader( result.getComponent( i ).getId() );
      if( dbh == null )
        continue;

      final IDataBlock block = createDataBLock( dbh, result, stationIndex, i );

      if( i == posGround )
        prfwriter.addDataBlock( 0, block );// due restrictions of wspwin-plotter
      else if( block.getCoordCount() > 0 )
        prfwriter.addDataBlock( block );
    }
    return prfwriter;
  }

  private IDataBlock createDataBLock( final DataBlockHeader dbh, final TupleResult result, final int stationIndex, final int i )
  {
    final boolean isText = LengthSectionMapping.isText( dbh );
    if( isText )
    {
      final LengthSectionTextBlock block = new LengthSectionTextBlock( dbh );
      final String[] lines = componentToString( result, i );
      block.setCoords( lines );
      return block;
    }
    else
    {
      final LengthSectionDataBlock block = new LengthSectionDataBlock( dbh );
      final Double[][] coords = componentToCoords( result, stationIndex, i );
      block.setCoords( coords[0], coords[1] );
      return block;
    }
  }

  // FIXME: move to helper
  protected Double[][] componentToCoords( final TupleResult result, final int stationIndex, final int componentIndex )
  {
    final Double[][] coords = new Double[2][];

    final List<Double> stations = new ArrayList<Double>();
    final List<Double> doubles = new ArrayList<Double>();

    for( final IRecord values : result )
    {
      final Object oStation = values.getValue( stationIndex );
      final Object oVal = values.getValue( componentIndex );

      if( oStation instanceof Number && oVal instanceof Number )
      {
        stations.add( ((Number) oStation).doubleValue() );
        doubles.add( ((Number) oVal).doubleValue() );
      }
    }

    coords[0] = stations.toArray( new Double[stations.size()] );
    coords[1] = doubles.toArray( new Double[doubles.size()] );

    return coords;
  }

  // FIXME: move to helper
  protected String[] componentToString( final TupleResult result, final int componentIndex )
  {
    final List<String> lines = new ArrayList<String>();

    for( final IRecord values : result )
    {
      final Object oVal = values.getValue( componentIndex );
      lines.add( ObjectUtils.toString( oVal ) );
    }

    return lines.toArray( new String[lines.size()] );
  }

  public final boolean write( final IObservation<TupleResult> obs, final Writer writer ) throws IOException
  {
    final DataBlockWriter dbw = extractDataBlocks( obs );
    dbw.store( new PrintWriter( writer ) );
    return true;
  }
}
