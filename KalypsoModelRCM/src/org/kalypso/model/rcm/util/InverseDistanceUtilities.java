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
package org.kalypso.model.rcm.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.kalypso.model.rcm.internal.binding.InverseDistanceElement;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * This class contains functions for dealing with the inverse distance weighting method.
 * 
 * @author Holger Albert
 */
public class InverseDistanceUtilities
{
  /**
   * The constructor.
   */
  private InverseDistanceUtilities( )
  {
  }

  /**
   * This function determines the weights (factors) for each point using the inverse distance weighting.
   * 
   * @param areaGeometry
   *          The area geometry.
   * @param points
   *          The point geometries of the stations.
   * @param maximumNumber
   *          The number of stations which should be used in the inverse distance weighting. The nearest ones will be
   *          used. The unused stations will get a 0.0 factor.
   * @return The weights (factors) for each station.
   */
  public static double[] getWeights( final Geometry areaGeometry, final Point[] points, final int maximumNumber )
  {
    /* Get the inverse distance element, containing the factor, for each station. */
    /* They will be still sorted by the distance to the area. */
    final List<InverseDistanceElement> elements = getFactors( areaGeometry, points, maximumNumber );

    /* So bring the factors in the right order again. */
    final double[] weights = new double[points.length];
    for( int i = 0; i < elements.size(); i++ )
    {
      /* Get the inverse distance element, containing the factor. */
      final InverseDistanceElement element = elements.get( i );

      /* Get the index. */
      final int index = element.getIndex();

      /* Get the factor. */
      final double factor = element.getFactor();

      /* Set it at the right place. */
      weights[index] = factor;
    }

    return weights;
  }

  /**
   * This function determines the factors for each station using the inverse distance weighting.
   * 
   * @param areaGeometry
   *          The area geometry.
   * @param points
   *          The point geometries of the stations.
   * @param maximumNumber
   *          The number of stations which should be used in the inverse distance weighting. The nearest ones will be
   *          used. The unused stations will get a 0.0 factor.
   * @return The factors for each station (they will be sorted by the distance to the area).
   */
  private static List<InverseDistanceElement> getFactors( final Geometry areaGeometry, final Point[] points, final int maximumNumber )
  {
    /* Create the inverse distance elements. */
    final List<InverseDistanceElement> elements = getInverseDistanceElemets( areaGeometry, points );

    /* The sum of all distances. */
    double sumDistances = 0.0;

    /* Calculate the distances. */
    final List<Double> distances = new ArrayList<>();
    for( int i = 0; i < elements.size(); i++ )
    {
      if( maximumNumber > 0 && i >= maximumNumber )
        break;

      /* Get the inverse distance element. */
      final InverseDistanceElement element = elements.get( i );

      /* Get the distance. */
      final double distance = 1 / element.getDistance();

      /* First add it to the sum of distances. */
      sumDistances = sumDistances + distance;

      /* Then add it to the list of distances. */
      distances.add( new Double( distance ) );
    }

    /* Calculate the factors. */
    for( int i = 0; i < elements.size(); i++ )
    {
      if( maximumNumber > 0 && i >= maximumNumber )
        break;

      /* Get the inverse distance element. */
      final InverseDistanceElement element = elements.get( i );

      /* Get the distance. */
      final Double distance = distances.get( i );

      /* Calculate the factor. */
      final double factor = distance.doubleValue() / sumDistances;

      /* Add it to the corresponding element. */
      element.setFactor( factor );
    }

    return elements;
  }

  /**
   * This function returns a list of inverse distance elements. The first item of an element will be always the
   * parameter area geometry and the second item of an element will be a ombrometer point of the list. The elements will
   * be sorted by the distance of the contained items.
   * 
   * @param areaGeometry
   *          The area geometry.
   * @param points
   *          The point geometries of the stations.
   * @return A list of inverse distance elements. The first item of an element will be always the parameter area
   *         geometry and the second item of an element will be a station point of the list. The elements will be sorted
   *         by the distance of the contained items.
   */
  private static List<InverseDistanceElement> getInverseDistanceElemets( final Geometry areaGeometry, final Point[] points )
  {
    /* Memory for the results. */
    final List<InverseDistanceElement> results = new ArrayList<>();

    for( int i = 0; i < points.length; i++ )
    {
      /* Get the point. */
      final Point point = points[i];

      /* Create the inverse distance element. */
      final InverseDistanceElement element = new InverseDistanceElement( areaGeometry, point, i );

      /* Add to the results. */
      results.add( element );
    }

    /* Sort the results. */
    Collections.sort( results, null );

    return results;
  }
}