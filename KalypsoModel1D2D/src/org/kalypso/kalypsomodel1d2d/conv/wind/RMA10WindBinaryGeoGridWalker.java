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
package org.kalypso.kalypsomodel1d2d.conv.wind;

import java.util.ArrayList;
import java.util.Formatter;
import java.util.List;

import org.deegree.framework.util.Pair;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.kalypsosimulationmodel.core.wind.BinaryGeoGridWrapperForPairsModel;
import org.kalypso.kalypsosimulationmodel.core.wind.NativeWindDataModelHelper;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * This grid walker will write the values inside of given envelope.
 *
 * @author ig
 *
 */
public class RMA10WindBinaryGeoGridWalker extends AbstractWindGeoGridWalker
{
  private static final double DOUBLE_DEFAULT_SCALE_VALUE = 1.0;

  private final List< GM_Position > m_listWrittenPositions;

  private final Formatter m_formatter;
  /**
   * The constructor.
   *
   * @param pGMEnvelope {@link GM_Envelope}
   *          This envelope will be walked.
   * @param grid descriptor {@link RectifiedGridDomain} of grid to visit
   */
  public RMA10WindBinaryGeoGridWalker( final GM_Envelope pGMEnvelope, final Formatter pFormatter, final RectifiedGridDomain pGridDescriptorOrig )
  {
    super( pGMEnvelope, pGridDescriptorOrig );
    m_listWrittenPositions = new ArrayList<>();
    m_formatter = pFormatter;
  }

  /**
   * @see org.kalypso.grid.IGeoGridWalker#operate(int, int, com.vividsolutions.jts.geom.Coordinate)
   */
  @Override
  public void operate( final int x, final int y, final Coordinate c ) throws GeoGridException
  {
    final Pair<Double, Double> lPairVectorNativeWindValues = ((BinaryGeoGridWrapperForPairsModel) getGrid()).getPairValue( x, y );
    final Pair<Double, Double> lPairSpeedDirection = NativeWindDataModelHelper.convertVectorWindToSpeedAndDirection( lPairVectorNativeWindValues );
    if( lPairSpeedDirection == null || Double.isNaN( lPairSpeedDirection.first ) || Double.isNaN( lPairSpeedDirection.second ) )
    {
      //points without values will not be printed into wind the file, but the coordinates are collected and printed in the coordinates file.
//      m_formatter.format( "WI %5d   %.3f   %.3f  %2.2f\n", m_listWrittenPositions.size() + 1, DOUBLE_DEFAULT_IGNORE_VALUE, DOUBLE_DEFAULT_IGNORE_VALUE, DOUBLE_DEFAULT_SCALE_VALUE );
    }
    else
    {
      m_formatter.format( "WI %5d %4.3f %4.3f %2.2f\n", m_listWrittenPositions.size() + 1, lPairSpeedDirection.first, lPairSpeedDirection.second, DOUBLE_DEFAULT_SCALE_VALUE ); //$NON-NLS-1$
    }
    m_listWrittenPositions.add( GeometryFactory.createGM_Position( c.x, c.y ) );
  }

  /**
   * @see org.kalypso.grid.IGeoGridWalker#start(org.kalypso.grid.IGeoGrid)
   */
  @Override
  public void start( final IGeoGrid grid )
  {
    setGrid( grid );
  }

  /**
   * @see org.kalypso.grid.IGeoGridWalker#finish()
   */
  @Override
  public Object finish( )
  {
    return m_listWrittenPositions;
  }


}
