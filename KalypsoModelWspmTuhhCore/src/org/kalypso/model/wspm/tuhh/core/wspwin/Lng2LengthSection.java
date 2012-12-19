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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeSet;

import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.wspwin.core.prf.PrfReader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;

/**
 * @author Gernot Belger
 */
public class Lng2LengthSection
{
  private final PrfReader m_prfReader;

  public Lng2LengthSection( final PrfReader prfReader )
  {
    m_prfReader = prfReader;
  }

  public void convertInto( final IObservation<TupleResult> observation )
  {
    // final Map<Integer, String[]> metaData = m_prfReader.getMetaData();
    final IDataBlock[] blocks = m_prfReader.getDataBlocks();

    final Map<String, DatablockIndex> indexedBlocks = buildBlockIndex( blocks );
    final BigDecimal[] stations = collectStations( indexedBlocks );

    // TODO: sign depends on upstreams/downstreams
    final BigDecimal sign = new BigDecimal( -1 );
    // TODO: we need to set the axis direction instead
    // if( sign.doubleValue() < 0 )
    // ArrayUtils.reverse( stations );

    fillObservation( observation, stations, indexedBlocks, sign );
  }

  protected Map<String, DatablockIndex> buildBlockIndex( final IDataBlock[] blocks )
  {
    final Map<String, DatablockIndex> indexedBlocks = new HashMap<>();
    for( final IDataBlock block : blocks )
    {
      final String blockHeader = block.getFirstLine();
      final String componentID = mapComponent( blockHeader );
      if( componentID != null )
      {
        final DatablockIndex datablockIndex = new DatablockIndex( block );
        indexedBlocks.put( componentID, datablockIndex );
      }
    }
    return indexedBlocks;
  }

  private String mapComponent( final String blockHeader )
  {
    if( blockHeader.toUpperCase().startsWith( "SOHLHOEHE" ) ) //$NON-NLS-1$
      return IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_GROUND;

    if( blockHeader.toUpperCase().startsWith( "WASSERSPIEGEL" ) ) //$NON-NLS-1$
      return IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL;

    // TODO: convert other result data as well
    return null;
  }

  private BigDecimal[] collectStations( final Map<String, DatablockIndex> indexedBlocks )
  {
    final Collection<BigDecimal> stations = new TreeSet<>();

    final Collection<DatablockIndex> blocks = indexedBlocks.values();
    for( final DatablockIndex datablock : blocks )
    {
      final BigDecimal[] blockStations = datablock.getStations();
      stations.addAll( Arrays.asList( blockStations ) );
    }

    return stations.toArray( new BigDecimal[stations.size()] );
  }

  private void fillObservation( final IObservation<TupleResult> observation, final BigDecimal[] stations, final Map<String, DatablockIndex> indexedBlocks, final BigDecimal sign )
  {
    final TupleResult result = observation.getResult();
    final Map<String, Integer> componentIndex = buildComponentIndex( result, indexedBlocks.keySet() );

    final int stationIndex = ComponentUtilities.getOrCreateComponent( result, IWspmTuhhConstants.LENGTH_SECTION_PROPERTY_STATION );

    for( final BigDecimal station : stations )
    {
      final Map<String, BigDecimal> values = fetchValues( indexedBlocks, station );
      if( !values.isEmpty() )
      {
        final IRecord record = result.createRecord();
        result.add( record );

        final BigDecimal stationValue = station.multiply( sign ).movePointLeft( 3 ).setScale( 4, BigDecimal.ROUND_HALF_UP );
        record.setValue( stationIndex, stationValue );

        for( final Entry<String, BigDecimal> entry : values.entrySet() )
        {
          final String component = entry.getKey();
          final Integer index = componentIndex.get( component );
          // REMARK: reduce scale, else we get huge number
          // TODO: instead, use scale of component
          final BigDecimal value = entry.getValue().setScale( 4, BigDecimal.ROUND_HALF_UP );

          record.setValue( index, value );
        }
      }
    }
  }

  private Map<String, BigDecimal> fetchValues( final Map<String, DatablockIndex> indexedBlocks, final BigDecimal station )
  {
    final Map<String, BigDecimal> values = new HashMap<>();

    for( final Entry<String, DatablockIndex> entry : indexedBlocks.entrySet() )
    {
      final String component = entry.getKey();
      final DatablockIndex block = entry.getValue();
      final BigDecimal value = block.getValue( station );
      if( value != null )
        values.put( component, value );
    }

    return values;
  }

  private Map<String, Integer> buildComponentIndex( final TupleResult result, final Collection<String> componentIDs )
  {
    final Map<String, Integer> index = new HashMap<>();

    for( final String componentID : componentIDs )
    {
      final int componentIndex = ComponentUtilities.getOrCreateComponent( result, componentID );
      index.put( componentID, componentIndex );
    }

    return index;
  }
}
