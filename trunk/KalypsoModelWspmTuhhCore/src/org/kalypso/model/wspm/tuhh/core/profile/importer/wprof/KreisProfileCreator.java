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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;

import com.vividsolutions.jts.geom.Envelope;

class KreisProfileCreator extends BridgeProfileCreator
{
  private final String m_kreisPoints;

  public KreisProfileCreator( final ProfileData data, final String soilPoints, final String kreisPoints, final String okPoints, final String bridgeWidthPoints )
  {
    super( data, soilPoints, null, okPoints, bridgeWidthPoints, "Kreisdurchlass" );

    m_kreisPoints = kreisPoints;
  }

  private IWProfPoint[] getKreisPoints( )
  {
    final IWProfPoint[] kreisPoints = getPoints( m_kreisPoints );
    return kreisPoints;
  }

  @Override
  protected IWProfPoint[] getSoilPoints( )
  {
    final IWProfPoint[] soilPoints = super.getSoilPoints();
    final IWProfPoint[] kreisPoints = getKreisPoints();

    final Envelope envelope = calculateEnvelope( kreisPoints );
    final double yHalf = getYHalf( envelope );

    final IWProfPoint[] upperHalf = getHalfPoints( kreisPoints, yHalf, false );

    final Collection<IWProfPoint> newPointsList = new ArrayList<IWProfPoint>();

    addSmaller( newPointsList, envelope.getMinX(), soilPoints, true );
    newPointsList.addAll( Arrays.asList( upperHalf ) );
    addSmaller( newPointsList, envelope.getMaxX(), soilPoints, false );

    return newPointsList.toArray( new IWProfPoint[newPointsList.size()] );
  }


  private static void addSmaller( final Collection<IWProfPoint> listToAdd, final double x, final IWProfPoint[] input, final boolean smaller )
  {
    for( final IWProfPoint point : input )
    {
      final double distance = point.getDistance().doubleValue();
      if( distance < x == smaller )
        listToAdd.add( point );
    }
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.wprof.BridgeProfileCreator#getUkPoints()
   */
  @Override
  protected IWProfPoint[] getUkPoints( )
  {
    final IWProfPoint[] kreisPoints = getKreisPoints();
    final Envelope envelope = calculateEnvelope( kreisPoints );
    final double yHalf = getYHalf( envelope );

    return getHalfPoints( kreisPoints, yHalf, true );
  }

  private static double getYHalf( final Envelope envelope )
  {
    return (envelope.getMaxY() + envelope.getMinY()) / 2;
  }

  private static IWProfPoint[] getHalfPoints( final IWProfPoint[] points, final double yHalf, final boolean upper )
  {
    final SortedMap<BigDecimal, IWProfPoint> result = new TreeMap<BigDecimal, IWProfPoint>();

    for( final IWProfPoint point : points )
    {
      final double value = point.getValue();
      final BigDecimal distance = point.getDistance();
      if( value > yHalf == upper )
        result.put( distance, point );
    }

    final Collection<IWProfPoint> values = result.values();
    return values.toArray( new IWProfPoint[values.size()] );
  }

  private static Envelope calculateEnvelope( final IWProfPoint[] points )
  {
    final double x0 = points[0].getDistance().doubleValue();
    final double y0 = points[0].getValue();

    final Envelope env = new Envelope( x0, x0, y0, y0 );
    for( final IWProfPoint profPoint : points )
    {
      final double x = profPoint.getDistance().doubleValue();
      final double y = profPoint.getValue();

      env.expandToInclude( x, y );
    }
    return env;
  }

}
