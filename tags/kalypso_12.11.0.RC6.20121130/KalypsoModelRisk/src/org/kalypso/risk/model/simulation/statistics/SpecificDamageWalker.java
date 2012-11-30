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
package org.kalypso.risk.model.simulation.statistics;

import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.IGeoGridWalker;
import org.kalypso.transformation.transformer.JTSTransformer;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
public class SpecificDamageWalker implements IGeoGridWalker
{
  private final StatisticCollector m_statistics;

  private final int m_returnPeriod;

  private double m_cellArea;

  private JTSTransformer m_transformer;

  public SpecificDamageWalker( final StatisticCollector statistics, final int returnPeriod )
  {
    m_statistics = statistics;
    m_returnPeriod = returnPeriod;
  }

  @Override
  public void start( final IGeoGrid grid ) throws GeoGridException
  {
    m_cellArea = Math.abs( grid.getOffsetX().x - grid.getOffsetY().x ) * Math.abs( grid.getOffsetX().y - grid.getOffsetY().y );

    final String gridCRS = grid.getSourceCRS();
    final int gridSRID = JTSAdapter.toSrid( gridCRS );

    final String statisticsSRS = m_statistics.getSRSName();
    final int statisticsSRID = JTSAdapter.toSrid( statisticsSRS );

    try
    {
      m_transformer = new JTSTransformer( gridSRID, statisticsSRID );
    }
    catch( final FactoryException e )
    {
      throw new GeoGridException( "Failed to initialize geo transformer", e ); //$NON-NLS-1$
    }
  }

  @Override
  public void operate( final int x, final int y, final Coordinate c ) throws GeoGridException
  {
    // TODO: maybe we should remember that this location is not covered?
    if( Double.isNaN( c.z ) || c.z < 0.0 )
      return;

    try
    {
      /* transform current location to srs of statistics */
      final Coordinate transformed = m_transformer.transform( c );

      m_statistics.addSpecificDamage( m_returnPeriod, transformed, m_cellArea );
    }
    catch( MismatchedDimensionException | TransformException e )
    {
      throw new GeoGridException( "Failed to transform coordinate", e ); //$NON-NLS-1$
    }
  }

  @Override
  public Object finish( )
  {
    return null;
  }
}