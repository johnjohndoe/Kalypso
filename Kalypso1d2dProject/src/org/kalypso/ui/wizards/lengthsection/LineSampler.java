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
package org.kalypso.ui.wizards.lengthsection;

import java.math.BigDecimal;
import java.util.SortedSet;
import java.util.TreeSet;

import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandlerParameters;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;

/**
 * @author Gernot Belger
 */
class LineSampler
{
  private final SortedSet<BigDecimal> m_fromValueSet = new TreeSet<>();

  private final SortedSet<BigDecimal> m_toValueSet = new TreeSet<>();

  private final Feature[] m_features;

  private final IValuePropertyType m_fromProperty;

  private final IValuePropertyType m_toProperty;

  private final BigDecimal m_samplingDistance;

  private final LengthSectionHandlerParameters m_parameters;

  public LineSampler( final LengthSectionHandlerParameters parameters )
  {
    m_parameters = parameters;
    m_features = parameters.getSelectedRivers();
    m_fromProperty = parameters.getStationFromProperty();
    m_toProperty = parameters.getStationToProperty();
    m_samplingDistance = parameters.getSamplingDistance();
  }

  public BigDecimal[] calculateStations( )
  {
    for( final Feature feature : m_features )
    {
      final GM_Curve curve = m_parameters.getGeometry( feature );
      if( curve == null )
      {
        // TODO: message to user
        continue;
      }

      final double lineLength = curve.getLength();

      final BigDecimal from = m_parameters.getNumberProperty( feature, m_fromProperty, 0.0 );
      final BigDecimal to = m_parameters.getNumberProperty( feature, m_toProperty, lineLength );

      m_fromValueSet.add( from );
      m_toValueSet.add( to );
    }

    if( m_fromValueSet.size() == 0 || m_toValueSet.size() == 0 )
    {
      // this means actually, that there are no valid entries in the defined field. We should try to use the geometry of
      // the line itself in order to calculate the positions
      return new BigDecimal[0];
    }

    final BigDecimal min = m_fromValueSet.first();
    final BigDecimal max = m_toValueSet.last();

    // handle null pointer

    // TODO: very strange, this is most probably not what was intended...

    final BigDecimal minDecimal = min.setScale( 1, BigDecimal.ROUND_FLOOR );
    final BigDecimal maxDecimal = max.setScale( 1, BigDecimal.ROUND_CEILING );

    final BigDecimal completeLength = maxDecimal.subtract( minDecimal );

    final Double divide = completeLength.doubleValue() / m_samplingDistance.doubleValue();

    final BigDecimal value = new BigDecimal( divide ).setScale( 5, BigDecimal.ROUND_HALF_UP );

    final int numOfClasses = (value).intValue() + 2;

    final BigDecimal[] stationList = new BigDecimal[numOfClasses];
    for( int currentClass = 0; currentClass < numOfClasses - 1; currentClass++ )
    {
      final double currentValue = minDecimal.doubleValue() + currentClass * m_samplingDistance.doubleValue();

      final BigDecimal station = new BigDecimal( currentValue );

      stationList[currentClass] = station;
    }

    // add last point of the riverline
    final double currentValue = maxDecimal.doubleValue();

    final BigDecimal station = new BigDecimal( currentValue );

    stationList[numOfClasses - 1] = station;

    return stationList;
  }
}