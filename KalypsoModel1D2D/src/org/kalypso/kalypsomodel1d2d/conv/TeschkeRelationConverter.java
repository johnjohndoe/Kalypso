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
package org.kalypso.kalypsomodel1d2d.conv;

import java.util.HashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;

/**
 * Helper class to write the polynomial stuff within the teschke relations.
 *
 * @author Gernot Belger
 */
public class TeschkeRelationConverter
{
  private final static class Range
  {
    public static final Range MAX_RANGE = new Range( Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY );

    private final double m_max;

    private final double m_min;

    public Range( final double min, final double max )
    {
      m_min = min;
      m_max = max;
    }

    /** Calculates the minimal range containing both given ranges. */
    public static Range merge( final Range range1, final Range range2 )
    {
      final double min = Math.min( range1.m_min, range2.m_min );
      final double max = Math.max( range1.m_max, range2.m_max );
      return new Range( min, max );
    }

    /**
     * Calculates the maximal range contained in both given ranges.
     *
     * @return <code>null</code>, if the ranges do not intersect.
     */
    public static Range intersect( final Range range1, final Range range2 )
    {
      final double min = Math.max( range1.m_min, range2.m_min );
      final double max = Math.min( range1.m_max, range2.m_max );

      if( max < min )
        return null;

      return new Range( min, max );
    }

    public double getMin( )
    {
      return m_min;
    }

    public double getMax( )
    {
      return m_max;
    }
  }

  private final Range m_range;

  private final Map<String, IPolynomial1D[]> m_sortedPolynomes;

  public TeschkeRelationConverter( final IPolynomial1D[] polynomials )
  {
    m_range = calculateCommonRange( polynomials );
    m_sortedPolynomes = sortPolynomes( polynomials );
  }

  /** Sorty the polynomial by type into arrays, which are again sorted by the min values of the polynomes. */
  private Map<String, IPolynomial1D[]> sortPolynomes( final IPolynomial1D[] polynomials )
  {
    final Map<String, SortedMap<Double, IPolynomial1D>> helperMap = new HashMap<>();

    for( final IPolynomial1D polynomial1D : polynomials )
    {
      final String phenomenon = polynomial1D.getDomainPhenomenon();
      final SortedMap<Double, IPolynomial1D> sortedMap;
      if( helperMap.containsKey( phenomenon ) )
        sortedMap = helperMap.get( phenomenon );
      else
      {
        sortedMap = new TreeMap<>();
        helperMap.put( phenomenon, sortedMap );
      }

      sortedMap.put( polynomial1D.getRangeMin(), polynomial1D );
    }

    final Map<String, IPolynomial1D[]> result = new HashMap<>( helperMap.size() );
    for( final String phenomenon : helperMap.keySet() )
    {
      final SortedMap<Double, IPolynomial1D> sortedMap = helperMap.get( phenomenon );
      final IPolynomial1D[] polynomial1Ds = sortedMap.values().toArray( new IPolynomial1D[sortedMap.size()] );
      result.put( phenomenon, polynomial1Ds );
    }

    return result;
  }

  public Double getMin( )
  {
    return m_range == null ? null : m_range.getMin();
  }

  public Double getMax( )
  {
    return m_range == null ? null : m_range.getMax();
  }

  private Range calculateCommonRange( final IPolynomial1D[] polynomials )
  {
    /* type -> range */
    final Map<String, Range> rangesPerType = new HashMap<>();

    /* find max range of each type */
    for( final IPolynomial1D polynomial1D : polynomials )
    {
      final String domainPhenomenon = polynomial1D.getDomainPhenomenon();
      final double min = polynomial1D.getRangeMin();
      final double max = polynomial1D.getRangeMax();
      final Range range = new Range( min, max );

      if( rangesPerType.containsKey( domainPhenomenon ) )
      {
        final Range currentRange = rangesPerType.get( domainPhenomenon );
        final Range newRange = Range.merge( currentRange, range );
        rangesPerType.put( domainPhenomenon, newRange );
      }
      else
        rangesPerType.put( domainPhenomenon, range );
    }

    /* Intersect all ranges to create overall range */
    Range range = Range.MAX_RANGE;
    for( final Range currentRange : rangesPerType.values() )
    {
      range = Range.intersect( range, currentRange );
      if( range == null )
        return null;
    }

    return range;
  }

  public IPolynomial1D[] getPolynomialsByType( final String domainPhenomenon )
  {
    return m_sortedPolynomes.get( domainPhenomenon );
  }

}
