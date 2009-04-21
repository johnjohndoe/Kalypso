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
package org.kalypso.model.wspm.tuhh.core.profile.importer.hw;

import java.io.File;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Formatter;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.Map.Entry;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
public class HeightWidthData
{
  private final Map<String, Map<Integer, Coordinate>> m_coordinatesHash = new LinkedHashMap<String, Map<Integer, Coordinate>>();

  private final String m_name;

  private final File m_tempDir;

  public HeightWidthData( final String name, final File tempDir )
  {
    m_name = name;
    m_tempDir = tempDir;
  }

  public void addPoint( final BigDecimal distance, final double value, final String objectType, final int attributeType, final int ord, final int partOrd )
  {
    // just do hash by object-typ; preserving the order
    final Coordinate coordinate = new Coordinate( distance.doubleValue(), value );

    final Map<Integer, Coordinate> crdHash = getCoordinates( objectType );

    // TODO: check
    if( !objectType.startsWith( "K" ) || attributeType != 60 )
      crdHash.put( ord, coordinate );
  }

  private Map<Integer, Coordinate> getCoordinates( final String objectType )
  {
    final Map<Integer, Coordinate> crdHash = m_coordinatesHash.get( objectType );
    if( crdHash != null )
      return crdHash;

    final SortedMap<Integer, Coordinate> newMap = new TreeMap<Integer, Coordinate>();
    m_coordinatesHash.put( objectType, newMap );

    return newMap;
  }

  public void formatOut( final Formatter formatter )
  {
    final IHeightWidthResult[] results = getResults();
    for( final IHeightWidthResult heightWidthResult : results )
      heightWidthResult.formatOut( formatter );
  }

  public void formatErr( final Formatter formatter )
  {
    System.out.println( m_name );
    formatter.format( "%s%n%n", m_name );

    final IHeightWidthResult[] results = getResults();
    for( final IHeightWidthResult heightWidthResult : results )
      heightWidthResult.formatLog( formatter );
    formatter.format( "%n%n%n" );
  }

  public IHeightWidthResult[] getResults( )
  {
    final List<IHeightWidthResult> results = new ArrayList<IHeightWidthResult>();
    for( final Entry<String, Map<Integer, Coordinate>> entry : m_coordinatesHash.entrySet() )
    {
      final String key = entry.getKey();
      final Map<Integer, Coordinate> crdHash = entry.getValue();

      // All tubes are calculated
      if( key.startsWith( "K" ) )
        results.add( new TubeResult( m_name, key, m_name, m_name, crdHash.values(), m_tempDir ) );

      // Mabye we have a bridge like structure
      if( key.startsWith( "V01" ) )
      {
        /* Check if we have a 'Unterkante Brücke', then we can calculate some area as well */
        final Map<Integer, Coordinate> ukCrds = m_coordinatesHash.get( "V02" );
        if( ukCrds != null )
          results.add( new BridgeResult( m_name, "V01-V03", m_name, m_name, crdHash.values(), ukCrds.values(), m_tempDir ) );
      }
    }

    return results.toArray( new IHeightWidthResult[results.size()] );
  }
}
