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
package org.kalypso.model.wspm.core.profil.util;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointProperty;

/**
 * @author kimwerner
 */
/**
 * @author kimwerner
 */
public class ProfilUtil
{
  public static final IProfilPoint getProfilPoint( final IProfil profil, final IProfilPoint pointBefore, final IProfilPoint pointAfter )
  {
    if( profil == null )
      return null;
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final IProfilPoint leftP = ((pointBefore == null) && (!points.isEmpty())) ? points.getFirst() : pointBefore;

    final IProfilPoint rightP = (pointAfter == null) ? getPointAfter( profil, leftP ) : pointAfter;
    return splitSegment( profil, leftP, rightP );

  }

  /**
   * @return a subList include both Deviderpoints, maybe null
   */

  public static final List<IProfilPoint> getInnerPoints( final IProfil profil, final String deviderTyp )
  {
    final IProfilPointMarker[] markers = profil.getPointMarkerFor( deviderTyp );
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final int leftPos = (markers.length > 0) ? points.indexOf( markers[0].getPoint() ) : 0;
    final int rightPos = (markers.length > 1) ? points.indexOf( markers[markers.length - 1].getPoint() ) + 1 : 0;
    return (leftPos < rightPos) ? points.subList( leftPos, rightPos ) : null;
  }

  /**
   * @throws ProfilDataException
   * @see org.kalypso.model.wspm.core.profilinterface.IProfil#getValuesFor(org.kalypso.model.wspm.core.profildata.tabledata.ColumnKey)
   */
  public static Double[] getValuesFor( final IProfil profil, final String pointProperty )
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final Double[] values = new Double[points.size()];
    int i = 0;
    for( IProfilPoint point : points )
    {
      values[i] = point.getValueFor( pointProperty );
      i++;
    }
    return values;
  }

  public static final List<IProfilPoint> getInnerPoints( final IProfil profil, final IProfilPointMarker leftDevider, final IProfilPointMarker rightDevider )
  {

    final LinkedList<IProfilPoint> points = profil.getPoints();
    final int leftPos = (leftDevider != null) ? points.indexOf( leftDevider.getPoint() ) : 0;
    final int rightPos = (rightDevider != null) ? points.indexOf( rightDevider.getPoint() ) : 0;
    return (leftPos < rightPos) ? points.subList( leftPos, rightPos ) : null;

  }

  /**
   * return true if all selected properties are equal
   */
  public static final boolean comparePoints( final IProfilPointProperty[] properties, final IProfilPoint point1, final IProfilPoint point2 )
  {
    for( final IProfilPointProperty property : properties )
    {
      final String propertyId = property.getId();
      if( Math.abs( point1.getValueFor( propertyId ) - point2.getValueFor( propertyId ) ) > property.getPrecision() )
        return false;
    }
    return true;
  }

  /**
   * return true if getValueFor(property) is equal
   */
  public static final boolean comparePoints( final IProfilPointProperty property, final IProfilPoint point1, final IProfilPoint point2 )
  {
    return comparePoints( new IProfilPointProperty[] { property }, point1, point2 );
  }

  public static final IProfilPoint splitSegment( final IProfil profile, IProfilPoint startPoint, IProfilPoint endPoint )
  {
    if( (startPoint == null) || (endPoint == null) )
      throw new IllegalArgumentException( "Profilpunkt existiert nicht" );
    final IProfilPoint point = profile.createProfilPoint();
    final IProfilPointProperty[] properties = profile.getPointProperties();
    for( final IProfilPointProperty property : properties )
    {
      final String propertyId = property.toString();
      final Double m_x = property.doInterpolation( startPoint.getValueFor( propertyId ), endPoint.getValueFor( propertyId ) );
      point.setValueFor( propertyId, m_x );
    }
    return point;
  }

  /**
   * findet einen Punkt in einem Profil 1.hole Punkt[index] und vergleiche Punkt.breite mit breite -> 2.suche Punkt bei
   * breite mit einer Toleranz von delta 3.kein Punkt gefunden -> (return null)
   */
  public static final IProfilPoint findPoint( final IProfil profil, final int index, final double breite, final double delta )
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final IProfilPoint point = index >= points.size() ? null : points.get( index );

    if( point != null )
    {
      if( point.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) == breite )
        return point;
    }

    return findPoint( profil, breite, delta );

  }

  public static IProfilPoint findNearestPoint( final IProfil profil, final double breite )
  {

    final LinkedList<IProfilPoint> points = profil.getPoints();
    IProfilPoint pkt = points.getFirst();
    for( final IProfilPoint point : points )
    {

      if( Math.abs( pkt.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) - breite ) > Math.abs( point.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) - breite ) )
        pkt = point;

    }
    return pkt;
  }

  public static IProfilPoint findPoint( final IProfil profil, final double breite, final double delta )
  {
    final IProfilPoint pkt = findNearestPoint( profil, breite );

    final double xpos = pkt.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
    return (Math.abs( xpos - breite ) <= delta) ? pkt : null;
  }

  public static IProfilPoint getPointBefore( final IProfil profil, IProfilPoint point )
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    if( points.isEmpty() || point == points.getFirst() )
      return null;

    final int i = points.indexOf( point );
    if( i == -1 )
      throw new IllegalArgumentException( "Punkt nicht im Profil: " + point );

    return points.get( i - 1 );
  }

  public static IProfilPoint getPointAfter( final IProfil profil, final IProfilPoint point )
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    if( points.isEmpty() || point == points.getLast() )
      return null;

    final int i = points.indexOf( point );
    if( i == -1 )
      throw new IllegalArgumentException( "Punkt nicht im Profil: " + point );

    return points.get( i + 1 );
  }

  public static Double getMaxValueFor( final IProfil profil, final String property )
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    if( points.isEmpty() )
      return null;
    Double maxValue = Double.MIN_VALUE;
    for( IProfilPoint point : points )
    {

      maxValue = Math.max( maxValue, point.getValueFor( property ) );

    }
    return maxValue;
  }

  public static Point2D getPoint2D( final IProfilPoint p, final String pointProperty )
  {

    final double x = p.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
    final double y = p.getValueFor( pointProperty );
    return new Point2D.Double( x, y );

  }

  /**
   * @return null if profil has no points or property does not exists
   * @return always one point (first point if no match)
   */
  public static IProfilPoint findNearestPoint( final IProfil profil, final double breite, final double value, final String property )
  {
    LinkedList<IProfilPoint> points = profil.getPoints();
    if( points.isEmpty() )
      return null;
    IProfilPoint bestPoint = points.getFirst();
    for( IProfilPoint point : points )
    {

      if( (Math.abs( bestPoint.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) - breite ) > Math.abs( point.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) - breite ))
          || (Math.abs( bestPoint.getValueFor( property ) - value ) > Math.abs( point.getValueFor( property ) - value )) )
        bestPoint = point;

    }
    return bestPoint;
  }

  /**
   * @return null if profil has no points or property does not exists or nomatch
   */
  public static IProfilPoint[] findPoint( final IProfil profil, final Double[] values, final IProfilPointProperty[] properties )
  {
    final List<IProfilPoint> points = new ArrayList<IProfilPoint>();
    final int maxIndex = Math.min( values.length, properties.length );
    final LinkedList<IProfilPoint> profilePoints = profil.getPoints();
    for( final IProfilPoint point : profilePoints )
    {
      boolean isEqual = true;
      for( int i = 0; i < maxIndex; i++ )
      {
        final String propertyId = properties[i].toString();
        final Double delta = properties[i].getPrecision();

        if( Math.abs( point.getValueFor( propertyId ) - values[i] ) > delta )
        {
          isEqual = false;
          break;
        }
      }
      if( isEqual )
        points.add( point );
    }
    return points.toArray( new IProfilPoint[0] );
  }

  public static Point2D[] getPoints2D( final IProfil profil, final String pointProperty )
  {
    final List<IProfilPoint> points = profil.getPoints();
    final Point2D[] points2D = new Point2D[points.size()];
    int i = 0;
    for( final IProfilPoint p : points )
    {

      final double x = p.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      final double y = p.getValueFor( pointProperty );
      points2D[i++] = new Point2D.Double( x, y );

    }
    return points2D;
  }

  public static Double getMinValueFor( final IProfil profil, final String property )
  {
    final IProfilPoint minPoint = getMinPoint( profil, property );
    if( minPoint == null )
      return null;

    return minPoint.getValueFor( property );
  }

  /**
   * Return the profile-point of the given profile with the minimum value at the given property.
   */
  public static IProfilPoint getMinPoint( final IProfil profil, final String property )
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    if( points.isEmpty() )
      return null;

    Double minValue = Double.MAX_VALUE;
    IProfilPoint minPoint = null;
    for( final IProfilPoint point : points )
    {
      final double value = point.getValueFor( property );
      if( value < minValue )
      {
        minValue = value;
        minPoint = point;
      }
    }
    return minPoint;
  }

  /**
   * return a valid ProfilPoint if operation succeeds, othwerwise null
   * 
   * @see org.kalypso.model.wspm.core.profil.impl.points.IProfilPoints#addPoint(double, double)
   */
  public static final IProfilPoint addPoint( final IProfil profil, final double breite, final double hoehe )
  {
    final IProfilPoint point = profil.createProfilPoint();

    point.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, hoehe );
    point.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, breite );
    profil.addPoint( point );
    return point;
  }

}
